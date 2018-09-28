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

					#if 1 //take modelBitangent from attributes
						vec3 modelBitangent = gl_MultiTexCoord6.xyz;
						vec3 worldBitangent = normalize(vec3(modelMatrix * vec4(modelBitangent, 0.0)));
					#else //calculate worldBitangent
						#ifdef TBN_REORTHO
							worldTangent = normalize(worldTangent - worldNormalN * dot(worldNormalN, worldTangent));
						#endif

						//vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
						vec3 worldBitangent = normalize( cross(worldTangent, worldNormalN) );
					#endif

					float handednessSign = dot(cross(worldNormalN, worldTangent), worldBitangent);
					worldTangent = worldTangent * handednessSign;

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
				shadowTexCoord.xy *= (inversesqrt(abs(shadowTexCoord.xy) + shadowParams.zz) + shadowParams.ww);
				shadowTexCoord.xy += shadowParams.xy;
			#endif

			cameraDir = cameraPos - worldPos;

			// TODO: multiply by gl_TextureMatrix[0] ?
			texCoord = gl_MultiTexCoord0.xy;
			#ifdef FLIP_UV
				texCoord.t = 1.0 - texCoord.t;
			#endif

			//TODO:
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

		#define PARALLAXMAP_LIMITS_AUTO 1
		#define PARALLAXMAP_LIMITS_MANUAL 2

		#define IBL_TEX_LOD_AUTO 1
		#define IBL_TEX_LOD_MANUAL 2

		#define EMISSIVEMAP_TYPE_VAL 1
		#define EMISSIVEMAP_TYPE_MULT 2

		#define TONEMAPPING_ACES 1
		#define TONEMAPPING_UNCHARTED2 2
		#define TONEMAPPING_FILMIC 3
		#define TONEMAPPING_REINHARD 4
		#define TONEMAPPING_LOG 5
		#define TONEMAPPING_ROMBINDAHOUSE 6

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
			#ifdef HAS_SPECULARMAP
				uniform sampler2D specularEnvTex;
			#else
				uniform samplerCube specularEnvTex;
			#endif
			#ifdef HAS_IRRADIANCEMAP
				uniform sampler2D irradianceEnvTex;
			#else
				uniform samplerCube irradianceEnvTex;
			#endif
			#if (IBL_TEX_LOD == IBL_TEX_LOD_MANUAL) //manual LOD
				uniform float iblMapLOD;
			#endif
		#endif

		uniform mat4 camera;

		uniform sampler2D brdfLUT;
		uniform vec2 iblMapScale;

		uniform vec4 baseColorMapScale;
		uniform vec3 normalMapScale;
		#if EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_VAL
			uniform vec3 emissiveMapScale;
		#elif EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_MULT
			uniform float emissiveMapScale;
		#endif
		uniform float occlusionMapStrength;
		uniform float roughnessMapScale;
		uniform float metallicMapScale;
		uniform float parallaxMapScale;

		#if (PARALLAXMAP_LIMITS == PARALLAXMAP_LIMITS_MANUAL) //manual limits
			uniform vec2 parallaxMapLimits;
		#endif

		#ifdef IBL_INVTONEMAP
			uniform float iblMapInvToneMapExp;
		#endif

		#ifdef use_shadows
			uniform sampler2DShadow shadowTex;
			uniform float shadowDensity;
		#endif

		uniform float exposure;

		uniform int simFrame; //set by api_cus
		uniform vec4 teamColor; //set by engine

		#line 20126
		struct PBRInfo {
			float NdotL;					// cos angle between normal and light direction
			float NdotV;					// cos angle between normal and view direction
			float NdotH;					// cos angle between normal and half vector
			float LdotV;					// cos angle between light direction and view direction
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

		//const float M_PI = 3.141592653589793;
		const float M_PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062;
		const float M_PI2 = M_PI * 2.0;
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
		const vec3 SRGB_GAMMA = vec3(1.0 / 2.2);
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

		/////////////////////////////////////////
		vec3 ACESFilmicTM(in vec3 x) {
			float a = 2.51;
			float b = 0.03;
			float c = 2.43;
			float d = 0.59;
			float e = 0.14;
			return (x*(a*x+b))/(x*(c*x+d)+e);
		}

		vec3 Uncharted2TM(in vec3 x) {
			const float A = 0.15;
			const float B = 0.50;
			const float C = 0.10;
			const float D = 0.20;
			const float E = 0.02;
			const float F = 0.30;
			const float W = 11.2;
			const float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;

			x *= vec3(2.0); //exposure bias

			vec3 outColor = ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
			outColor /= white;

			return outColor;
		}

		vec3 FilmicTM(in vec3 x) {
			vec3 outColor = max(vec3(0.), x - vec3(0.004));
			outColor = (outColor * (6.2 * outColor + .5)) / (outColor * (6.2 * outColor + 1.7) + 0.06);
			return fromSRGB(outColor); //sadly FilmicTM outputs gamma corrected colors, so need to reverse that effect
		}

		vec3 ReinhardTM(in vec3 x) {
			return x / (vec3(1.) + x);
		}

		vec3 LogTM(vec3 c) {
			const float limit = 2.2;
			const float contrast = 0.35;

			c = log(c + 1.0) / log(limit + 1.0);
			c = clamp(c, 0.0, 1.0);

			c = mix(c, c * c * (3.0 - 2.0 * c), contrast);
			c = pow(c, vec3(1.05,0.9,1));

			return c;
		}

		vec3 RomBinDaHouseTM(vec3 c) {
			c = exp( -1.0 / ( 2.72 * c + 0.15 ) );
			return c;
		}

		const vec3 LUMA = vec3(0.2126, 0.7152, 0.0722);

		vec3 expExpand(in vec3 x, in float cutoff, in float mul) {
			float xL = dot(x, LUMA);

			float cutEval = step(cutoff, xL);

			float yL = (1.0 - cutEval) * xL + cutEval * (exp(mul * xL) - exp(mul * cutoff) + cutoff);
			return x * yL / xL;
		}
		/////////////////////////////////////////

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

					//vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
					vec3 worldBitangent = normalize( cross(worldTangent, worldNormalN) );

					float handednessSign = dot(cross(worldNormalN, worldTangent), worldBitangent);
					worldTangent = worldTangent * handednessSign;

					mat3 worldTBN = mat3(worldTangent, worldBitangent, worldNormalN);
				#else // HAS_TANGENTS
					// Do nothing, got worldTBN from vertex shader
					// TODO: Orthogonalize TBN as well?
				#endif

				vec3 normalMapVal = GET_NORMALMAP;
				#ifdef SRGB_NORMALMAP
					normalMapVal = fromSRGB(normalMapVal);
				#endif
				normalMapVal = vec3(2.0) * normalMapVal - vec3(1.0); // [0:1] -> [-1.0:1.0]
				vec3 worldFragNormal = worldTBN * normalMapVal;
			#else // undefined GET_NORMALMAP
				// don't do normal mapping, just pass worldNormal
				vec3 worldFragNormal = worldNormal;
			#endif

			worldFragNormal *= normalMapScale;
			worldFragNormal = normalize(worldFragNormal);

			return worldFragNormal;
		}

		vec4 rgbeToLinear(in vec4 rgbe) {
			return vec4(rgbe.rgb * exp2( rgbe.a * 255.0 - 128.0 ), 1.0);
		}

		vec4 sampleEquiRect(in sampler2D tex, in vec3 direction) {
			vec3 directionN = normalize(direction);
			vec2 uv = vec2((atan(directionN.z, directionN.x) / M_PI2) + 0.5, acos(directionN.y) / M_PI);
			uv = clamp(uv, vec2(0.0), vec2(1.0));
			//vec4 rgbe = texture(tex, uv, -2.0);
			vec4 rgbe = textureLod(tex, uv, 0.0);
			return rgbeToLinear(rgbe);
		}

		vec4 sampleEquiRectLod(in sampler2D tex, in vec3 direction, in float lod) {
			vec3 directionN = normalize(direction);
			vec4 rgbe = textureLod(tex, vec2((atan(directionN.z, directionN.x) / M_PI2) + 0.5, acos(directionN.y) / M_PI), lod);
			return rgbeToLinear(rgbe);
		}

		void getIBLContribution(PBRInfo pbrInputs, vec3 n, vec3 reflection, out vec3 diffuse, out vec3 specular)
		{
			diffuse = vec3(iblMapScale.x);
			specular = vec3(iblMapScale.y);

			#ifdef GET_IBLMAP
				#if 0 // TODO remove this when irradianceEnvTex is bound to something good
					#ifdef HAS_IRRADIANCEMAP
						vec3 diffuseLight = sampleEquiRect(irradianceEnvTex, n).rgb;
					#else
						vec3 diffuseLight = texture(irradianceEnvTex, n).rgb;
					#endif

					#ifdef SRGB_IBLMAP
						diffuseLight = fromSRGB(diffuseLight);
					#endif
				#else
					ivec2 irradianceEnvTexSize = textureSize(irradianceEnvTex, 0);
					float iblDiffMapLOD = log2(float(max(irradianceEnvTexSize.x, irradianceEnvTexSize.y)));
					#ifdef HAS_IRRADIANCEMAP
						vec3 diffuseLight = sampleEquiRectLod(irradianceEnvTex, n, iblDiffMapLOD - 4.0).rgb;
					#else
						vec3 diffuseLight = textureLod(irradianceEnvTex, n, iblDiffMapLOD - 4.0).rgb;
					#endif

					#ifdef SRGB_IBLMAP
						diffuseLight = fromSRGB(diffuseLight);
					#endif

					#ifdef IBL_INVTONEMAP
						#ifdef HAS_IRRADIANCEMAP
							float avgDLum = dot(LUMA, sampleEquiRectLod(irradianceEnvTex, n, iblDiffMapLOD).rgb);
						#else
							float avgDLum = dot(LUMA, textureLod(irradianceEnvTex, n, iblDiffMapLOD).rgb);
						#endif
						diffuseLight = expExpand(diffuseLight, avgDLum, iblMapInvToneMapExp);
					#endif

					#if 0
						diffuseLight = vec3(1.0);
					#endif
				#endif

				#if (IBL_TEX_LOD == IBL_TEX_LOD_AUTO) // if IBL_TEX_LOD == IBL_TEX_LOD_MANUAL, then iblMapLOD is defined as a uniform
					ivec2 specularEnvTexSize = textureSize(specularEnvTex, 0);
					float iblMapLOD = log2(float(max(specularEnvTexSize.x, specularEnvTexSize.y)));
				#endif

				#ifdef IBL_TEX_LOD
					float lod = (pbrInputs.roughness * iblMapLOD);
					#ifdef HAS_SPECULARMAP
						vec3 specularLight = sampleEquiRectLod(specularEnvTex, reflection, lod).rgb;
					#else
						vec3 specularLight = textureLod(specularEnvTex, reflection, lod).rgb;
					#endif
				#else
					#ifdef HAS_SPECULARMAP
						vec3 specularLight = sampleEquiRect(specularEnvTex, reflection).rgb;
					#else
						vec3 specularLight = texture(specularEnvTex, reflection).rgb;
					#endif
				#endif

				#ifdef SRGB_IBLMAP
					specularLight = fromSRGB(specularLight);
				#endif

				#ifdef IBL_INVTONEMAP
					float iblSpecMapLOD = log2(float(max(specularEnvTexSize.x, specularEnvTexSize.y)));
					#ifdef HAS_SPECULARMAP
						float avgSLum = dot(LUMA, sampleEquiRectLod(specularEnvTex, reflection, iblSpecMapLOD).rgb);
					#else
						float avgSLum = dot(LUMA, textureLod(specularEnvTex, reflection, iblSpecMapLOD).rgb);
					#endif
					specularLight = expExpand(specularLight, avgSLum, iblMapInvToneMapExp);
				#endif

			#else
				vec3 diffuseLight = vec3(1.0);
				vec3 specularLight = vec3(1.0);
			#endif

			//sanitize Lights
			diffuseLight = max(vec3(0.0), diffuseLight);
			specularLight = max(vec3(0.0), specularLight);

			vec3 brdf = fromSRGB( texture(brdfLUT, vec2(pbrInputs.NdotV, 1.0 - pbrInputs.roughness)) ).rgb;
			diffuse *= diffuseLight * pbrInputs.diffuseColor;
			specular *= specularLight * (pbrInputs.specularColor * brdf.x + brdf.y);
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

		#ifdef use_shadows
			float getShadowCoeff(in vec4 shadowCoords, PBRInfo pbrInputs) {
				const float cb = 0.00005;
				float bias = cb * tan(acos(pbrInputs.NdotL));
				bias = clamp(bias, 0.5 * cb, 2.0 * cb);

				float coeff = 0.0;

				vec2 invShadowMapSize = 1.0 / textureSize( shadowTex, 0 );

				const mat3 w = mat3(
					0.0625, 0.125, 0.0625,
					0.125 , 0.25 ,  0.125,
					0.0625, 0.125, 0.0625
				);

				for( int x = -1; x <= 1; x++ ) {
					for( int y = -1; y <= 1; y++ ) {
						vec2 offset = vec2( x, y ) * invShadowMapSize * 2.5;
						//vec2 offset = vec2( x, y ) * invShadowMapSize;
						coeff += w[1 + x][1 + y] * textureProj( shadowTex, shadowCoords + vec4(offset.x, offset.y, -bias, 0.0) );
					}
				}

				//coeff = textureProj(shadowTex, shadowCoords + vec4(0.0, 0.0, -bias, 0.0) );

				coeff  = (1.0 - coeff);
				coeff *= shadowDensity;
				return (1.0 - coeff);
			}
		#endif

		#if defined(GET_PARALLAXMAP) && defined(HAS_TANGENTS)
			#ifdef PARALLAXMAP_FAST
				// https://learnopengl.com/code_viewer_gh.php?code=src/5.advanced_lighting/5.1.parallax_mapping/5.1.parallax_mapping.fs
				// Simple parallax mapping
				vec2 parallaxMapping(vec2 texC, vec3 tangentViewDir)
				{
					float height = GET_PARALLAXMAP(texC);
					#ifdef PARALLAXMAP_PERSPECTIVE //Normal Parallax Mapping
						vec2 P = tangentViewDir.xy / tangentViewDir.z * height * parallaxMapScale;
					#else //Parallax Mapping with Offset Limiting
						vec2 P = tangentViewDir.xy * height * parallaxMapScale;
					#endif
					return texC - P;
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
					#ifdef PARALLAXMAP_PERSPECTIVE //Normal Parallax Mapping
						vec2 P = tangentViewDir.xy / tangentViewDir.z * parallaxMapScale;
					#else //Parallax Mapping with Offset Limiting
						vec2 P = tangentViewDir.xy * parallaxMapScale;
					#endif
					vec2 deltaTexCoords = P / numLayers;

					// get initial values
					vec2  currentTexCoords     = texC;
					float currentDepthMapValue = GET_PARALLAXMAP(currentTexCoords);

					while(currentLayerDepth < currentDepthMapValue)
					{
						// shift texture coordinates along direction of P
						currentTexCoords -= deltaTexCoords;
						// get depthmap value at current texture coordinates
						currentDepthMapValue = GET_PARALLAXMAP(currentTexCoords);
						// get depth of next layer
						currentLayerDepth += layerDepth;
					}

					// get texture coordinates before collision (reverse operations)
					vec2 prevTexCoords = currentTexCoords + deltaTexCoords;

					// get depth after and before collision for linear interpolation
					float afterDepth  = currentDepthMapValue - currentLayerDepth;
					float beforeDepth = GET_PARALLAXMAP(prevTexCoords) - currentLayerDepth + layerDepth;

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
				vec2 texDiff = samplingTexCoord - texCoord;
				#if (PARALLAXMAP_LIMITS == PARALLAXMAP_LIMITS_AUTO) //automated texture offset limits
					float bumpVal = GET_PARALLAXMAP(samplingTexCoord) * parallaxMapScale;
					texDiff = clamp(texDiff, -vec2(bumpVal), vec2(bumpVal));
					samplingTexCoord = texCoord + texDiff;
				#elif (PARALLAXMAP_LIMITS == PARALLAXMAP_LIMITS_MANUAL) //user-defined texture offset limits
					texDiff = clamp(texDiff, -parallaxMapLimits, parallaxMapLimits);
					samplingTexCoord = texCoord + texDiff;
				#endif

				bvec4 badTexCoords = bvec4(samplingTexCoord.x > 1.0, samplingTexCoord.y > 1.0, samplingTexCoord.x < 0.0, samplingTexCoord.y < 0.0);
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
				#if EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_VAL
					vec3 emissiveRaw = GET_EMISSIVEMAP;
				#elif EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_MULT
					float emissiveRaw = GET_EMISSIVEMAP;
				#endif
				#ifdef SRGB_EMISSIVEMAP
					emissiveRaw = fromSRGB(emissiveRaw);
				#endif
				#if EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_VAL
					emissive = emissiveRaw * emissiveMapScale;
				#elif EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_MULT
					emissive = baseColor.rgb * vec3(emissiveRaw * emissiveMapScale);
				#endif
			#else
				#if EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_VAL
					emissive = emissiveMapScale;
				#elif EMISSIVEMAP_TYPE == EMISSIVEMAP_TYPE_MULT
					emissive = vec3(emissiveMapScale);
				#endif
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
			roughness = max(roughness, MINROUGHNESS);
			metallic = max(metallic, 0.0);

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
			vec3 v = normalize(cameraDir);					// Vector from surface point to camera
			vec3 l = normalize(sunPos);						// Vector from surface point to light
			vec3 h = normalize(l + v);						// Half vector between both l and v
			vec3 reflection = -normalize(reflect(v, n));

			float NdotL = clamp(dot(n, l), 0.001, 1.0);
			float NdotV = clamp(abs(dot(n, v)), 0.001, 1.0);
			float NdotH = clamp(dot(n, h), 0.0, 1.0);
			float LdotV = clamp(dot(l, v), 0.0, 1.0);
			float LdotH = clamp(dot(l, h), 0.0, 1.0);
			float VdotH = clamp(dot(v, h), 0.0, 1.0);

			PBRInfo pbrInputs = PBRInfo(
				NdotL,
				NdotV,
				NdotH,
				LdotH,
				LdotV,
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
			vec3 modelDiffColor = NdotL * sunColor * diffuseContrib;
			vec3 modelSpecColor = NdotL * sunColor * specContrib;

			vec3 iblDiffuse;
			vec3 iblSpecular;

			getIBLContribution(pbrInputs, n, reflection, iblDiffuse, iblSpecular);

			vec3 totalDiffColor = modelDiffColor + iblDiffuse;
			vec3 totalDiffColorAO = mix(totalDiffColor, totalDiffColor * occlusion, occlusionMapStrength);

			vec3 totalSpecColor = modelSpecColor + iblSpecular;

			vec3 brdfPassColor = totalDiffColorAO + totalSpecColor;

			#ifdef use_shadows
				float shadow = getShadowCoeff(shadowTexCoord, pbrInputs);
				brdfPassColor *= shadow;
			#endif

			brdfPassColor += emissive;

			vec3 preExpColor = brdfPassColor * vec3(exposure);

			#if (TONEMAPPING == TONEMAPPING_ACES)
				vec3 tmColor = ACESFilmicTM(preExpColor);
			#elif (TONEMAPPING == TONEMAPPING_UNCHARTED2)
				vec3 tmColor = Uncharted2TM(preExpColor);
			#elif (TONEMAPPING == TONEMAPPING_FILMIC)
				vec3 tmColor = FilmicTM(preExpColor);
			#elif (TONEMAPPING == TONEMAPPING_REINHARD)
				vec3 tmColor = ReinhardTM(preExpColor);
			#elif (TONEMAPPING == TONEMAPPING_LOG)
				vec3 tmColor = LogTM(preExpColor);
			#elif (TONEMAPPING == TONEMAPPING_ROMBINDAHOUSE)
				vec3 tmColor = RomBinDaHouseTM(preExpColor);
			#else
				vec3 tmColor = preExpColor;
			#endif

			vec3 preGammaColor = mix(tmColor, teamColor.rgb, baseColor.a);

			#ifdef GAMMA_CORRECTION
				gl_FragColor = toSRGB( vec4(preGammaColor, 1.0) );
			#else
				gl_FragColor = vec4(preGammaColor, 1.0);
			#endif

			//follow jK advise to avoid discard
			#if defined(GET_PARALLAXMAP) && defined(HAS_TANGENTS)
				gl_FragColor = mix(gl_FragColor, vec4(0.0), float(any(badTexCoords)));
			#endif

			#if   (DEBUG == DEBUG_BASECOLOR)
				gl_FragColor = vec4(baseColor.rgb, 1.0);
			#elif (DEBUG == DEBUG_WORLDNORMALS)
				gl_FragColor = vec4(n, 1.0);
			#elif (DEBUG == DEBUG_VIEWNORMALS)
				gl_FragColor = vec4(normalize((camera * vec4(n, 0.0)).rgb), 1.0);
			#elif (DEBUG == DEBUG_TANGENTNORMALS)
				gl_FragColor = vec4(GET_NORMALMAP, 1.0);
			#elif (DEBUG == DEBUG_PARALLAXSHIFT)
				float tdl = length(texDiff.xy) * 10.0;
				gl_FragColor = vec4( normalize(vec3(texDiff.x, texDiff.y, 0.0)) * vec3(tdl) , 1.0);
			#elif (DEBUG == DEBUG_DIFFUSECOLOR)
				gl_FragColor = vec4(diffuseColor, 1.0);
			#elif (DEBUG == DEBUG_SPECULARCOLOR)
				gl_FragColor = vec4(specularColor, 1.0);
			#elif (DEBUG == DEBUG_SPECULARANDDIFFUSECOLOR)
				gl_FragColor = vec4(diffuseColor + specularColor, 1.0);
			#elif (DEBUG == DEBUG_EMISSIONCOLOR)
				gl_FragColor = vec4(emissive, 1.0);
			#elif (DEBUG == DEBUG_TEAMCOLOR)
				gl_FragColor = vec4(teamColor.rgb * vec3(baseColor.a), 1.0);
			#elif (DEBUG == DEBUG_OCCLUSION)
				gl_FragColor = vec4(vec3(occlusion), 1.0);
			#elif (DEBUG == DEBUG_ROUGHNESS)
				gl_FragColor = vec4(vec3(roughness), 1.0);
			#elif (DEBUG == DEBUG_METALLIC)
				gl_FragColor = vec4(vec3(metallic), 1.0);
			#elif (DEBUG == DEBUG_REFLECTIONDIR)
				gl_FragColor = vec4(reflection, 1.0);
			#elif (DEBUG == DEBUG_REFLECTIONLENGTH)
				gl_FragColor = vec4(reflection.ggg, 1.0);
			#elif (DEBUG == DEBUG_SPECWORLDREFLECTION)
				#ifdef HAS_SPECULARMAP
					gl_FragColor = vec4( sampleEquiRectLod(specularEnvTex, n, 0.0).rgb, 1.0 );
				#else
					gl_FragColor = vec4( textureLod(specularEnvTex, n, 0.0).rgb, 1.0 );
				#endif
			#elif (DEBUG == DEBUG_SPECVIEWREFLECTION)
				#ifdef HAS_SPECULARMAP
					gl_FragColor = vec4( sampleEquiRectLod(specularEnvTex, reflection, 0.0).rgb, 1.0 );
				#else
					gl_FragColor = vec4( textureLod(specularEnvTex, reflection, 0.0).rgb, 1.0 );
				#endif
			#elif (DEBUG == DEBUG_IRRADIANCEWORLDREFLECTION)
				#ifdef HAS_IRRADIANCEMAP
					gl_FragColor = vec4( sampleEquiRectLod(irradianceEnvTex, n, 0.0).rgb, 1.0 );
				#else
					gl_FragColor = vec4( textureLod(irradianceEnvTex, n, 0.0).rgb, 1.0 );
				#endif
			#elif (DEBUG == DEBUG_IBLSPECULARCOLOR)
				gl_FragColor = vec4( iblSpecular, 1.0 );
			#elif (DEBUG == DEBUG_IBLDIFFUSECOLOR)
				gl_FragColor = vec4( iblDiffuse, 1.0 );
			#elif (DEBUG == DEBUG_IBLSPECULARANDDIFFUSECOLOR)
				gl_FragColor = vec4( iblSpecular + iblDiffuse, 1.0 );
			#elif (DEBUG == DEBUG_SHADOWCOEFF1)
				//float offset_scale_N = sqrt(1 - NdotL * NdotL); // sin(acos(L·N))
				//gl_FragColor = vec4( vec3(offset_scale_N), 1.0 );
				//gl_FragColor = vec4( vec3( (1.0 - LdotV) * (1.0 - NdotL) ), 1.0 );
				//gl_FragColor = vec4( vec3( sqrt(1.0 - pbrInputs.LdotV * pbrInputs.LdotV) ), 1.0 );
				//float v1 = sqrt(1.0 - pbrInputs.LdotV * pbrInputs.LdotV) * clamp(tan(acos(pbrInputs.NdotL)), 0.0, 1.0);
				//float v1 = sqrt(1.0 - dot(v, vec3(0.0, 1.0, 0.0))) * clamp(tan(acos(pbrInputs.NdotL)), 0.0, 1.0);
				//gl_FragColor = vec4( vec3( v1 ), 1.0);
				gl_FragColor = vec4( vec3( tan(acos(pbrInputs.NdotL)) ), 1.0);
			#elif (DEBUG == DEBUG_SHADOWCOEFF2)
				//float offset_scale_N = sqrt(1 - NdotL * NdotL); // sin(acos(L·N))
				//float offset_scale_L = offset_scale_N / NdotL;    // tan(acos(L·N))
				//offset_scale_L = min(2.0, offset_scale_L);
				//gl_FragColor = vec4( vec3(offset_scale_L/2.0), 1.0 );
				float v1 = dot(cross(v, l), vec3(0.0, 1.0, 0.0));
				v1 = abs( v1 );
				gl_FragColor = vec4( vec3( v1 ), 1.0 );
			#elif (DEBUG == DEBUG_SHADOW)
				gl_FragColor = vec4( vec3(shadow), 1.0 );
			#elif (DEBUG == DEBUG_PREEXPCOLOR)
				gl_FragColor = vec4(preExpColor, 1.0);
			#elif (DEBUG == DEBUG_TMCOLOR)
				gl_FragColor = vec4(tmColor, 1.0);
			#endif


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
		irradianceEnvTex = 7,
		specularEnvTex = 8,
	},
	uniformFloat = {
		sunColor = {1.0, 1.0, 1.0},
		shadowDensity = {gl.GetSun("shadowDensity" ,"unit")},
	},
	uniformMatrix = {
	},
}