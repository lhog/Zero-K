return {
	-- Heavily inspired by https://github.com/KhronosGroup/glTF-WebGL-PBR/blob/master/shaders/pbr-vert.glsl
	vertex = [[
		%%VERTEX_GLOBAL_NAMESPACE%%

		uniform mat4 camera;		//ViewMatrix (gl_ModelViewMatrix is ModelMatrix!)
		uniform vec3 cameraPos;

		//The api_custom_unit_shaders supplies this definition:
		#ifdef use_shadows
			uniform mat4 shadowMatrix;
			uniform vec4 shadowParams;
		#endif

		out Data {
			vec3 worldPos;
			vec3 cameraDir;
			vec2 texCoord;

			#ifdef use_shadows
				vec4 shadowTexCoord;
			#endif

			#ifdef GET_NORMALMAP
				#ifdef HAS_TANGENTS
					mat3 worldTBN;
				#else
					vec3 worldNormal;
				#endif
			#else
				vec3 worldNormal;
			#endif
		};

		//For a moment pretend we have passed OpenGL 2.0 era
		#define modelMatrix gl_ModelViewMatrix			// don't trust the ModelView name, it's modelMatrix in fact
		#define viewMatrix camera						// viewMatrix is mat4 uniform supplied by Gadget(?)
		#define projectionMatrix gl_ProjectionMatrix	// just because I don't like gl_BlaBla

		void main(void)	{
			vec4 modelPos = gl_Vertex;
			vec3 modelNormal = gl_Normal;

			%%VERTEX_PRE_TRANSFORM%%

			#ifdef GET_NORMALMAP
				#ifdef HAS_TANGENTS
					vec3 worldNormalN = normalize(gl_NormalMatrix * modelNormal);
					// The use of gl_MultiTexCoord[5,6] is a hack due to lack of support of proper attributes
					// TexCoord5 and TexCoord6 are defined and filled in engine. See: rts/Rendering/Models/AssParser.cpp
					vec3 modelTangent = gl_MultiTexCoord5.xyz;
					vec3 worldTangent = normalize(vec3(modelMatrix * vec4(modelTangent, 0.0)));

					#if 0 //take modelBitangent from attributes
						vec3 modelBitangent = gl_MultiTexCoord6.xyz;
						vec3 worldBitangent = normalize(vec3(modelMatrix * vec4(modelBitangent, 0.0)));
					#else //calculate worldBitangent
						#ifdef TBN_REORTHO
							worldTangent = normalize(worldTangent - worldNormalN * dot(worldNormalN, worldTangent));
						#endif
						vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
					#endif

					//TODO check handedness: see http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-13-normal-mapping/ (Handedness)

					worldTBN = mat3(worldTangent, worldBitangent, worldNormalN);
				#else
					worldNormal = gl_NormalMatrix * modelNormal;
				#endif
			#else
				worldNormal = gl_NormalMatrix * modelNormal;
			#endif

			vec4 worldPos4 = modelMatrix * modelPos;
			worldPos = worldPos4.xyz / worldPos4.w; //doesn't make much sense (?)

			#ifdef use_shadows
				shadowTexCoord = shadowMatrix * worldPos4;
				shadowTexCoord.st = shadowTexCoord.st * (inversesqrt( abs(shadowTexCoord.st) + shadowParams.z) + shadowParams.w) + shadowParams.xy;
			#endif

			cameraDir = cameraPos - worldPos;

			// TODO: multiply by gl_TextureMatrix[0] ?
			texCoord = gl_MultiTexCoord0.xy;
			#ifdef FLIP_UV
				texCoord.t = 1.0 - texCoord.t;
			#endif

			//TODO:
			// 1) shadows: shadowTexCoord
			// 2) flashlights ?
			// 3) treadoffset ?

			gl_Position = projectionMatrix * (viewMatrix * worldPos4);

			%%VERTEX_POST_TRANSFORM%%
		}
	]],

	-- Heavily inspired by https://github.com/KhronosGroup/glTF-WebGL-PBR/blob/master/shaders/pbr-frag.glsl
	fragment = [[
		#ifdef GL_FRAGMENT_PRECISION_HIGH
			// ancient GL3 ATI drivers confuse GLSL for GLSL-ES and require this
			precision highp float;
		#else
			precision mediump float;
		#endif

		uniform vec3 sunPos;
		uniform vec3 sunColor;

		#ifdef HAS_TEX0
			uniform sampler2D tex0;
		#endif
		#ifdef HAS_TEX1
			uniform sampler2D tex1;
		#endif
		#ifdef HAS_TEX2
			uniform sampler2D tex2;
		#endif
		#ifdef HAS_TEX3
			uniform sampler2D tex3;
		#endif

		#ifdef GET_IBLMAP
			uniform samplerCube reflectionEnvTex;
			//uniform samplerCube specularEnvTex;
			#if (USE_TEX_LOD == 1) //manual LOD
				uniform float iblMapLOD;
			#endif
		#endif

		uniform sampler2D brdfLUT;
		uniform vec2 iblMapScale;

		uniform vec4 baseColorMapScale;
		uniform float normalMapScale;
		uniform vec3 emissiveMapScale;
		uniform float occlusionMapStrength;
		uniform float roughnessMapScale;
		uniform float metallicMapScale;
		uniform float parallaxMapScale;
		#if (PARALLAXMAP_LIMITS == 2) //manual limits
			uniform vec2 parallaxMapLimits;
		#endif

		#ifdef use_shadows
			uniform sampler2DShadow shadowTex;
			uniform float shadowDensity;
		#endif

		uniform int simFrame; //set by api_cus
		uniform vec4 teamColor; //set by engine

		#line 20126
		struct PBRInfo {
			float NdotL;					// cos angle between normal and light direction
			float NdotV;					// cos angle between normal and view direction
			float NdotH;					// cos angle between normal and half vector
			float LdotH;					// cos angle between light direction and half vector
			float VdotH;					// cos angle between view direction and half vector
			vec3 reflectance0;				// full reflectance color (normal incidence angle)
			vec3 reflectance90;				// reflectance color at grazing angle
			float roughness;				// authored roughness. Used in getIBLContribution()
			float roughness2;				// roughness^2 used in geometricOcclusion()
			float roughness4;				// roughness^4 used in microfacetDistribution()
			vec3 diffuseColor;				// color contribution from diffuse lighting
			vec3 specularColor;				// color contribution from specular lighting
		};

		vec4 texels[4]; //change to something else if more/less base textures are required

		const float M_PI = 3.141592653589793;
		const float MINROUGHNESS = 0.04;

		in Data {
			vec3 worldPos;
			vec3 cameraDir;
			vec2 texCoord;

			#ifdef use_shadows
				vec4 shadowTexCoord;
			#endif

			#ifdef GET_NORMALMAP
				#ifdef HAS_TANGENTS
					mat3 worldTBN;
				#else
					vec3 worldNormal;
				#endif
			#else
				vec3 worldNormal;
			#endif
		};

		//inspired by https://github.com/tobspr/GLSL-Color-Spaces/blob/master/ColorSpaces.inc.glsl
		const vec3 SRGB_INVERSE_GAMMA = vec3(2.2);
		const vec3 SRGB_GAMMA = vec3(1.0) / SRGB_INVERSE_GAMMA;
		const vec3 SRGB_ALPHA = vec3(0.055);
		const vec3 SRGB_MAGIC_NUMBER = vec3(12.92);
		const vec3 SRGB_MAGIC_NUMBER_INV = vec3(1.0) / SRGB_MAGIC_NUMBER;

		#line 20171

		float fromSRGB(float srgbIn) {
			#ifdef FASTGAMMA
				float rgbOut = pow(srgbIn, SRGB_INVERSE_GAMMA.x);
			#else
				float bLess = step(0.04045, srgbIn);
				float rgbOut1 = srgbIn * SRGB_MAGIC_NUMBER_INV.x;
				float rgbOut2 = pow((srgbIn + SRGB_ALPHA.x) / (1.0 + SRGB_ALPHA.x), 2.4);
				float rgbOut = mix( rgbOut1, rgbOut2, bLess );
			#endif
			return rgbOut;
		}

		vec3 fromSRGB(vec3 srgbIn) {
			#ifdef FASTGAMMA
				vec3 rgbOut = pow(srgbIn.rgb, SRGB_INVERSE_GAMMA);
			#else
				vec3 bLess = step(vec3(0.04045), srgbIn.rgb);
				vec3 rgbOut1 = srgbIn.rgb * SRGB_MAGIC_NUMBER_INV;
				vec3 rgbOut2 = pow((srgbIn.rgb + SRGB_ALPHA) / (vec3(1.0) + SRGB_ALPHA), vec3(2.4));
				vec3 rgbOut = mix( rgbOut1, rgbOut2, bLess );
			#endif
			return rgbOut;
		}

		vec4 fromSRGB(vec4 srgbIn) {
			#ifdef FASTGAMMA
				vec3 rgbOut = pow(srgbIn.rgb, SRGB_INVERSE_GAMMA);
			#else
				vec3 bLess = step(vec3(0.04045), srgbIn.rgb);
				vec3 rgbOut1 = srgbIn.rgb * SRGB_MAGIC_NUMBER_INV;
				vec3 rgbOut2 = pow((srgbIn.rgb + SRGB_ALPHA) / (vec3(1.0) + SRGB_ALPHA), vec3(2.4));
				vec3 rgbOut = mix( rgbOut1, rgbOut2, bLess );
			#endif
			return vec4(rgbOut, srgbIn.a);
		}

		#line 20209

		float toSRGB(float rgbIn) {
			#ifdef FASTGAMMA
				float srgbOut = pow(rgbIn, SRGB_GAMMA.x);
			#else
				float bLess = step(0.0031308, rgbIn);
				float srgbOut1 = rgbIn * SRGB_MAGIC_NUMBER.x;
				float srgbOut2 = (1.0 + SRGB_ALPHA.x) * pow(rgbIn, 1.0/2.4) - SRGB_ALPHA.x;
				float srgbOut = mix( srgbOut1, srgbOut2, bLess );
			#endif
			return srgbOut;
		}

		vec3 toSRGB(vec3 rgbIn) {
			#ifdef FASTGAMMA
				vec3 srgbOut = pow(rgbIn.rgb, SRGB_GAMMA);
			#else
				vec3 bLess = step(vec3(0.0031308), rgbIn.rgb);
				vec3 srgbOut1 = rgbIn.rgb * SRGB_MAGIC_NUMBER;
				vec3 srgbOut2 = (vec3(1.0) + SRGB_ALPHA) * pow(rgbIn.rgb, vec3(1.0/2.4)) - SRGB_ALPHA;
				vec3 srgbOut = mix( srgbOut1, srgbOut2, bLess );
			#endif
			return srgbOut;
		}

		vec4 toSRGB(vec4 rgbIn) {
			#ifdef FASTGAMMA
				vec3 srgbOut = pow(rgbIn.rgb, SRGB_GAMMA);
			#else
				vec3 bLess = step(vec3(0.0031308), rgbIn.rgb);
				vec3 srgbOut1 = rgbIn.rgb * SRGB_MAGIC_NUMBER;
				vec3 srgbOut2 = (vec3(1.0) + SRGB_ALPHA) * pow(rgbIn.rgb, vec3(1.0/2.4)) - SRGB_ALPHA;
				vec3 srgbOut = mix( srgbOut1, srgbOut2, bLess );
			#endif
			return vec4(srgbOut, rgbIn.a);
		}

		#line 20247

		vec3 getWorldFragNormal() {
			#ifdef GET_NORMALMAP
				#ifndef HAS_TANGENTS
					vec3 posDx = dFdx(worldPos);
					vec3 posDy = dFdy(worldPos);

					vec2 texDx = dFdx(texCoord);
					vec2 texDy = dFdy(texCoord);

					vec3 worldTangent = (texDy.t * posDx - texDx.t * posDy) / (texDx.s * texDy.t - texDy.s * texDx.t);

					// TODO: figure out which one is right/better
					#if 1
						vec3 worldNormalN = normalize(worldNormal);
					#else
						vec3 worldNormalN = normalize(cross(posDx, posDy));
					#endif

					#ifdef TBN_REORTHO
						worldTangent = normalize(worldTangent - worldNormalN * dot(worldNormalN, worldTangent));
					#endif

					vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
					mat3 worldTBN = mat3(worldTangent, worldBitangent, worldNormalN);
				#else // HAS_TANGENTS
					// Do nothing, got worldTBN from vertex shader
					// TODO: Orthogonalize TBN as well?
				#endif

				vec3 normalMapVal = GET_NORMALMAP;
				#ifdef SRGB_NORMALMAP
					normalMapVal = fromSRGB(normalMapVal);
				#endif
				normalMapVal = vec3(2.0) * normalMapVal - vec3(1.0); // [0:1] -> [-0.5:0.5]
				vec3 worldFragNormal = worldTBN * normalMapVal;
			#else // undefined GET_NORMALMAP
				// don't do normal mapping, just pass worldNormal
				vec3 worldFragNormal = worldNormal;
			#endif

			worldFragNormal *= vec3(normalMapScale, normalMapScale, 1.0);
			worldFragNormal = normalize(worldFragNormal);

			return worldFragNormal;
		}


		vec3 getIBLContribution(PBRInfo pbrInputs, vec3 n, vec3 reflection)
		{
			vec3 diffuse = vec3(iblMapScale.x);
			vec3 specular = vec3(iblMapScale.y);

			#ifdef GET_IBLMAP
				vec3 diffuseLight = texture(reflectionEnvTex, n).rgb;
				#ifdef SRGB_IBLMAP
					diffuseLight = fromSRGB(diffuseLight);
				#endif

				#if (USE_TEX_LOD == 2) // if USE_TEX_LOD == 1, then iblMapLOD is defined as a uniform
					ivec2 reflectionEnvTexSize = textureSize(reflectionEnvTex, 0);
					float iblMapLOD = log2(float(max(reflectionEnvTexSize.x, reflectionEnvTexSize.y)));
				#endif

				#ifdef USE_TEX_LOD
					float lod = (pbrInputs.roughness * iblMapLOD);
					vec3 specularLight = textureLod(reflectionEnvTex, reflection, lod).rgb;
				#else
					vec3 specularLight = texture(reflectionEnvTex, reflection).rgb;
				#endif

				#ifdef SRGB_IBLMAP
					specularLight = fromSRGB(specularLight);
				#endif
			#else
				vec3 diffuseLight = vec3(1.0);
				vec3 specularLight = vec3(1.0);
			#endif

			vec3 brdf = fromSRGB( texture(brdfLUT, vec2(pbrInputs.NdotV, 1.0 - pbrInputs.roughness)) ).rgb;
			diffuse *= diffuseLight * pbrInputs.diffuseColor;
			specular *= specularLight * (pbrInputs.specularColor * brdf.x + brdf.y);

			return clamp(diffuse + specular, vec3(0.0), vec3(1.0));
		}


		vec3 diffuseLambert(PBRInfo pbrInputs) {
			return pbrInputs.diffuseColor / M_PI;
		}

		vec3 specularReflection(PBRInfo pbrInputs) {
			return pbrInputs.reflectance0 + (pbrInputs.reflectance90 - pbrInputs.reflectance0) * pow( clamp(1.0 - pbrInputs.VdotH, 0.0, 1.0), 5.0 );
		}

		float geometricOcclusion(PBRInfo pbrInputs) {
			float NdotL = pbrInputs.NdotL;
			float NdotV = pbrInputs.NdotV;
			float r = pbrInputs.roughness2;

			float attenuationL = 2.0 * NdotL / (NdotL + sqrt(r * r + (1.0 - r * r) * (NdotL * NdotL)));
			float attenuationV = 2.0 * NdotV / (NdotV + sqrt(r * r + (1.0 - r * r) * (NdotV * NdotV)));
			return attenuationL * attenuationV;
		}

		float microfacetDistribution(PBRInfo pbrInputs) {
			float f = (pbrInputs.NdotH * pbrInputs.roughness4 - pbrInputs.NdotH) * pbrInputs.NdotH + 1.0;
			return pbrInputs.roughness4 / (M_PI * f * f);
		}

		float getShadowCoeff(vec4 shadowCoords) {
			#ifdef use_shadows
				float coeff = textureProj(shadowTex, shadowCoords + vec4(0.0, 0.0, -0.00005, 0.0));
				coeff  = (1.0 - coeff);
				coeff *= shadowDensity;
				return (1.0 - coeff);
			#else
				return 1.0;
			#endif
		}

		#if defined(GET_PARALLAXMAP) && defined(HAS_TANGENTS)
			#ifdef FAST_PARALLAXMAP
				// https://learnopengl.com/code_viewer_gh.php?code=src/5.advanced_lighting/5.1.parallax_mapping/5.1.parallax_mapping.fs
				// Simple parallax mapping
				vec2 parallaxMapping(vec2 texC, vec3 tangentViewDir)
				{
					float height = GET_PARALLAXMAP;
					return texC - tangentViewDir.xy * (height * parallaxMapScale);
				}
			#else
				// https://learnopengl.com/code_viewer_gh.php?code=src/5.advanced_lighting/5.3.parallax_occlusion_mapping/5.3.parallax_mapping.fs
				// Parallax occlusion mapping
				vec2 parallaxMapping(vec2 texC, vec3 tangentViewDir)
				{
					// number of depth layers
					const float minLayers = 8;
					const float maxLayers = 32;
					float numLayers = mix(maxLayers, minLayers, abs(dot(vec3(0.0, 0.0, 1.0), tangentViewDir)));
					// calculate the size of each layer
					float layerDepth = 1.0 / numLayers;
					// depth of current layer
					float currentLayerDepth = 0.0;
					// the amount to shift the texture coordinates per layer (from vector P)
					vec2 P = tangentViewDir.xy / tangentViewDir.z * parallaxMapScale;
					vec2 deltaTexCoords = P / numLayers;

					// get initial values
					vec2  currentTexCoords     = texC;
					float currentDepthMapValue = GET_PARALLAXMAP;

					while(currentLayerDepth < currentDepthMapValue)
					{
						// shift texture coordinates along direction of P
						currentTexCoords -= deltaTexCoords;
						// get depthmap value at current texture coordinates
						currentDepthMapValue = GET_PARALLAXMAP;
						// get depth of next layer
						currentLayerDepth += layerDepth;
					}

					// get texture coordinates before collision (reverse operations)
					vec2 prevTexCoords = currentTexCoords + deltaTexCoords;

					// get depth after and before collision for linear interpolation
					float afterDepth  = currentDepthMapValue - currentLayerDepth;
					float beforeDepth = GET_PARALLAXMAP - currentLayerDepth + layerDepth;

					// interpolation of texture coordinates
					float weight = afterDepth / (afterDepth - beforeDepth);
					vec2 finalTexCoords = prevTexCoords * weight + currentTexCoords * (1.0 - weight);

					return finalTexCoords;
				}
			#endif
		#endif


		%%FRAGMENT_GLOBAL_NAMESPACE%%

		void fillTexelsArray(vec2 texC) {
			#ifdef HAS_TEX0
				texels[0] = texture(tex0, texC);
			#else
				texels[0] = vec4(0.0);
			#endif

			#ifdef HAS_TEX1
				texels[1] = texture(tex1, texC);
			#else
				texels[1] = vec4(0.0);
			#endif

			#ifdef HAS_TEX2
				texels[2] = texture(tex2, texC);
			#else
				texels[2] = vec4(0.0);
			#endif

			#ifdef HAS_TEX3
				texels[3] = texture(tex3, texC);
			#else
				texels[3] = vec4(0.0);
			#endif
		}

		void main(void) {
			%%FRAGMENT_PRE_SHADING%%

			// Here we have chicken and egg problem in case TBN is calculated in frag shader.
			#if defined(GET_PARALLAXMAP) && defined(HAS_TANGENTS)
				mat3 invWorldTBN = transpose(worldTBN);
				vec3 tangentViewDir = normalize(invWorldTBN * cameraDir);

				vec2 samplingTexCoord = parallaxMapping(texCoord, tangentViewDir);
				#if (PARALLAXMAP_LIMITS == 1) //automated texture offset limits
					vec2 texDiff = samplingTexCoord - texCoord;
					float bumpVal = GET_PARALLAXMAP * parallaxMapScale;
					texDiff = clamp(texDiff, -vec2(bumpVal), vec2(bumpVal));
					samplingTexCoord = texCoord + texDiff;
				#elif (PARALLAXMAP_LIMITS == 2) //user-defined texture offset limits
					vec2 texDiff = samplingTexCoord - texCoord;
					texDiff = clamp(texDiff, -parallaxMapLimits, parallaxMapLimits);
					samplingTexCoord = texCoord + texDiff;
				#endif

				bvec4 badTexCoords = bvec4(samplingTexCoord.x > 1.0, samplingTexCoord.y > 1.0, samplingTexCoord.x < 0.0, samplingTexCoord.y < 0.0);
				if (any(badTexCoords))
					discard;
			#else
				vec2 samplingTexCoord = texCoord;
			#endif

			fillTexelsArray(samplingTexCoord);

			vec4 baseColor;
			#ifdef GET_BASECOLORMAP
				baseColor = GET_BASECOLORMAP;
				#ifdef SRGB_BASECOLORMAP
					baseColor = fromSRGB(baseColor);
				#endif
				baseColor *= baseColorMapScale;
			#else
				baseColor = baseColorMapScale;
			#endif

			vec3 emissive;
			#ifdef GET_EMISSIVEMAP
				emissive = GET_EMISSIVEMAP;
				#ifdef SRGB_EMISSIVEMAP
					emissive = fromSRGB(emissive);
				#endif
				emissive *= emissiveMapScale;
			#else
				emissive = emissiveMapScale;
			#endif

			float occlusion;
			#ifdef GET_OCCLUSIONMAP
				occlusion = GET_OCCLUSIONMAP;
				#ifdef SRGB_OCCLUSIONMAP
					occlusion = fromSRGB(occlusion);
				#endif
			#else
				occlusion = 1.0;
			#endif

			float roughness;
			#ifdef GET_ROUGHNESSMAP
				roughness = GET_ROUGHNESSMAP;
				#ifdef SRGB_ROUGHNESSMAP
					roughness = fromSRGB(roughness);
				#endif
				roughness *= roughnessMapScale;
			#else
				roughness = roughnessMapScale;
			#endif

			float metallic;
			#ifdef GET_METALLICMAP
				metallic = GET_METALLICMAP;
				#ifdef SRGB_METALLICMAP
					metallic = fromSRGB(metallic);
				#endif
				metallic *= metallicMapScale;
			#else
				metallic = metallicMapScale;
			#endif

			// sanitize inputs
			// TODO: expand ranges?
			roughness = clamp(roughness, MINROUGHNESS, 1.0);
			metallic = clamp(metallic, 0.0, 1.0);

			float roughness2 = roughness * roughness;
			float roughness4 = roughness2 * roughness2;

			vec3 f0 = vec3(MINROUGHNESS);
			vec3 diffuseColor = baseColor.rgb * (vec3(1.0) - f0);
			diffuseColor *= vec3(1.0 - metallic);
			vec3 specularColor = mix(f0, baseColor.rgb, vec3(metallic));

			// Compute reflectance.
			float reflectance = max(max(specularColor.r, specularColor.g), specularColor.b);

			// For typical incident reflectance range (between 4% to 100%) set the grazing reflectance to 100% for typical fresnel effect.
			// For very low reflectance range on highly diffuse objects (below 4%), incrementally reduce grazing reflecance to 0%.
			float reflectance90 = clamp(reflectance * 25.0, 0.0, 1.0);
			vec3 specularEnvironmentR0 = specularColor.rgb;
			vec3 specularEnvironmentR90 = vec3(1.0, 1.0, 1.0) * reflectance90;

			vec3 n = getWorldFragNormal();					// normal at surface point
			vec3 v = normalize(cameraDir);					// Vector from surface point to camera, might need to flip it
			vec3 l = normalize(sunPos);						// Vector from surface point to light
			vec3 h = normalize(l + v);						// Half vector between both l and v
			vec3 reflection = -normalize(reflect(v, n));

			float NdotL = clamp(dot(n, l), 0.001, 1.0);
			float NdotV = clamp(abs(dot(n, v)), 0.001, 1.0);
			float NdotH = clamp(dot(n, h), 0.0, 1.0);
			float LdotH = clamp(dot(l, h), 0.0, 1.0);
			float VdotH = clamp(dot(v, h), 0.0, 1.0);

			PBRInfo pbrInputs = PBRInfo(
				NdotL,
				NdotV,
				NdotH,
				LdotH,
				VdotH,
				specularEnvironmentR0,
				specularEnvironmentR90,
				roughness,
				roughness2,
				roughness4,
				diffuseColor,
				specularColor
			);

			// Calculate the shading terms for the microfacet specular shading model
			vec3 F = specularReflection(pbrInputs);
			float G = geometricOcclusion(pbrInputs);
			float D = microfacetDistribution(pbrInputs);

			// Calculation of analytical lighting contribution
			vec3 diffuseContrib = (1.0 - F) * diffuseLambert(pbrInputs);
			vec3 specContrib = F * G * D / (4.0 * NdotL * NdotV);

			// Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
			vec3 color = NdotL * sunColor * (diffuseContrib + specContrib);

			color += getIBLContribution(pbrInputs, n, reflection);

			color = mix(color, color * occlusion, occlusionMapStrength);

			float shadow = getShadowCoeff(shadowTexCoord + vec4(0.0, 0.0, -0.00005, 0.0));
			color *= shadow;

			//color = mix(color, teamColor.rgb, baseColor.a);
			color += emissive;

			float alpha = 1.0;

			#ifdef GAMMA_CORRECTION
				gl_FragColor = toSRGB( vec4(color, 1.0) );
			#else
				gl_FragColor = vec4(color, 1.0);
			#endif

//				#if (USE_TEX_LOD == 2) // if USE_TEX_LOD == 1, then iblMapLOD is defined as a uniform
//					ivec2 reflectionEnvTexSize = textureSize(reflectionEnvTex, 0);
//					float iblMapLOD = log2(float(max(reflectionEnvTexSize.x, reflectionEnvTexSize.y)));
//				#endif

			//gl_FragColor = vec4( textureLod(reflectionEnvTex, n, iblMapLOD).rgb, 1.0);
			//gl_FragColor = vec4( metallic, roughness, occlusion, 1.0);

			%%FRAGMENT_POST_SHADING%%
		}
	]],

	uniformInt = {
		tex0 = 0,
		tex1 = 1,
		tex2 = 2,
		tex3 = 3,
		--tex4 = 4,
		brdfLUT = 5,
		shadowTex = 6,
		--specularEnvTex = 7,
		reflectionEnvTex = 8,
	},
	uniformFloat = {
		-- sunDir = {gl.GetSun("pos")}, -- material has sunDirLoc
		--sunAmbient = {gl.GetSun("ambient" ,"unit")},
		--sunDiffuse = {gl.GetSun("diffuse" ,"unit")},
		--sunColor = {gl.GetSun("diffuse" ,"unit")},
		sunColor = {1.0, 1.0, 1.0},
		shadowDensity = {gl.GetSun("shadowDensity" ,"unit")},
		-- shadowParams  = {gl.GetShadowMapParams()}, -- material has shadowParamsLoc
	},
	uniformMatrix = {
	-- shadowMatrix = {gl.GetMatrixData("shadow")}, -- material has shadow{Matrix}Loc
	},
}