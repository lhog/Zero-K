		#ifdef GL_FRAGMENT_PRECISION_HIGH
			// ancient GL3 ATI drivers confuse GLSL for GLSL-ES and require this
			precision highp float;
		#else
			precision mediump float;
		#endif

		struct LightInfo {
			vec3 lightDirection;
			vec3 lightColor;
		};
		uniform LightInfo lightInfo;

		struct MaterialInfo {
			#ifdef USE_IBL
				samplerCube diffuseEnvTex;
				samplerCube specularEnvTex;
				sampler2D brdfLUT;
			#endif

			#ifdef HAS_BASECOLORMAP
				sampler2D baseColorTex;
			#endif

			#ifdef HAS_NORMALMAP
				sampler2D normalMapTex;
				float normalScale;
			#endif

			#ifdef HAS_EMISSIVEMAP
				sampler2D emissiveTex;
				vec3 emissiveFactor;
			#endif

			#ifdef HAS_METALROUGHNESSAOMAP
				sampler2D metallicRoughnessAOTex;
			#endif

			#ifdef HAS_OCCLUSIONMAP
				sampler2D occlusionTex;
			#endif

			vec2 metallicRoughnessValues;
			vec4 baseColorFactor;
			float occlusionStrength;

			sampler2DShadow shadowTex;
			float shadowDensity;
		};
		uniform MaterialInfo materialInfo;

		struct MiscInfo {
			float curFrame;
		};
		uniform MiscInfo miscInfo;

		uniform vec4 teamColor; //set by engine

		struct PBRInfo {
			float NdotL;                  // cos angle between normal and light direction
			float NdotV;                  // cos angle between normal and view direction
			float NdotH;                  // cos angle between normal and half vector
			float LdotH;                  // cos angle between light direction and half vector
			float VdotH;                  // cos angle between view direction and half vector
			float perceptualRoughness;    // roughness value, as authored by the model creator (input to shader)
			float metalness;              // metallic value at the surface
			vec3 reflectance0;            // full reflectance color (normal incidence angle)
			vec3 reflectance90;           // reflectance color at grazing angle
			float alphaRoughness;         // roughness mapped to a more linear change in the roughness (proposed by [2])
			vec3 diffuseColor;            // color contribution from diffuse lighting
			vec3 specularColor;           // color contribution from specular lighting
		};

		const float M_PI = 3.141592653589793;
		const float MINROUGHNESS = 0.04;

		in Data {
			vec3 worldPos;
			vec3 cameraDir;
			vec2 texCoord;
			vec2 shadowTexCoord;

			#ifdef HAS_NORMALS
				#ifdef HAS_TANGENTS
					mat3 worldTBN;
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

		vec4 fromSRGB(vec4 srgbIn) {
			#ifdef FAST_RGB_TRANSFORMS
				vec3 rgbOut = pow(srgbIn.xyz, SRGB_INVERSE_GAMMA);
			#else
				vec3 bLess = step(vec3(0.04045), srgbIn.xyz);
				vec3 rgbOut1 = srgbIn.xyz * SRGB_MAGIC_NUMBER_INV;
				vec3 rgbOut2 = pow((srgbIn.xyz + SRGB_ALPHA) / (vec3(1.0) + SRGB_ALPHA), vec3(2.4));
				vec3 rgbOut = mix( rgbOut1, rgbOut2, bLess );
			#endif
			return vec4(rgbOut, srgbIn.w);
		}

		vec4 toSRGB(vec4 rgbIn) {
			#ifdef FAST_RGB_TRANSFORMS
				vec3 srgbOut = pow(srgbIn.xyz, SRGB_GAMMA);
			#else
				vec3 bLess = step(vec3(0.0031308), rgbIn.xyz);
				vec3 srgbOut1 = rgbIn * SRGB_MAGIC_NUMBER;
				vec3 srgbOut2 = (vec3(1.0) + SRGB_ALPHA) * pow(rgbIn, vec3(1.0/2.4)) - SRGB_ALPHA;
				vec3 srgbOut = mix( srgbOut1, srgbOut2, bLess );
			#endif
			return vec4(srgbOut, rgbIn.w);
		}

		vec3 getWorldFragNormal() {
			// Retrieve the tangent space matrix
			#ifndef HAS_TANGENTS
				vec3 posDx = dFdx(worldPos);
				vec3 posDy = dFdy(worldPos);
				vec3 texDx = dFdx(vec3(texCoord, 0.0));
				vec3 texDy = dFdy(vec3(texCoord, 0.0));
				vec3 worldTangent = (texDy.t * posDx - texDx.t * posDy) / (texDx.s * texDy.t - texDy.s * texDx.t);
			#ifdef HAS_NORMALS
				vec3 worldNormalN = normalize(worldNormal);
			#else
				vec3 worldNormalN = normalize(cross(posDx, posDy));
			#endif
				#ifdef DO_REORTHOGONALIZATION
					// Gram-Schmidt process to re-orthogonalize the TBN vectors
					worldTangent = normalize(worldTangent - worldNormalN * dot(worldNormalN, worldTangent));
				#endif
				vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
				mat3 worldTBN = mat3(worldTangent, worldBitangent, worldNormalN);
			#else // HAS_TANGENTS
				// TODO: Orthogonalize TBN as well?
				// Do nothing, got worldTBN from vertex shader
			#endif

			#ifdef HAS_NORMALMAP
				vec3 normalMapVal = texture(normalMapTex, texCoord).rgb;
				normalMapVal = vec3(2.0) * normalMapVal - vec3(1.0); // [0:1] -> [-0.5:0.5]

				vec3 worldFragNormal = normalize(worldTBN * (normalMapVal * vec3(normalScale, normalScale, 1.0)));
			#else
				// The tbn matrix is linearly interpolated, so we need to re-normalize
				// TODO: figure out WTF is the following
				vec3 worldFragNormal = normalize(worldTBN[2].xyz);
			#endif

			return worldFragNormal;
		}

		#ifdef USE_IBL
			vec3 getIBLContribution(PBRInfo pbrInputs, vec3 n, vec3 reflection) {
				//TODO Implement!!!
				return vec3(0.0);
			}
		#endif

		vec3 diffuseLambert(PBRInfo pbrInputs) {
			return pbrInputs.diffuseColor / M_PI;
		}

		vec3 specularReflection(PBRInfo pbrInputs) {
			return pbrInputs.reflectance0 + (pbrInputs.reflectance90 - pbrInputs.reflectance0) * pow( clamp(1.0 - pbrInputs.VdotH, 0.0, 1.0), 5.0 );
		}

		float geometricOcclusion(PBRInfo pbrInputs) {
			float NdotL = pbrInputs.NdotL;
			float NdotV = pbrInputs.NdotV;
			float r = pbrInputs.alphaRoughness;

			float attenuationL = 2.0 * NdotL / (NdotL + sqrt(r * r + (1.0 - r * r) * (NdotL * NdotL)));
			float attenuationV = 2.0 * NdotV / (NdotV + sqrt(r * r + (1.0 - r * r) * (NdotV * NdotV)));
			return attenuationL * attenuationV;
		}

		float microfacetDistribution(PBRInfo pbrInputs) {
			float roughnessSq = pbrInputs.alphaRoughness * pbrInputs.alphaRoughness;
			float f = (pbrInputs.NdotH * roughnessSq - pbrInputs.NdotH) * pbrInputs.NdotH + 1.0;
			return roughnessSq / (M_PI * f * f);
		}

		%%FRAGMENT_GLOBAL_NAMESPACE%%

		void main(void) {
			%%FRAGMENT_PRE_SHADING%%

			float perceptualRoughness = metallicRoughnessValues.y;
			float metallic = metallicRoughnessValues.x;

			float ao = 1.0;

			// R = optional AO
			// G = roughness
			// B = metallness
			#ifdef HAS_METALROUGHNESSAOMAP
				vec4 mraoVal = texture(metallicRoughnessAOTex, texCoord);
				#ifdef SRGB_METALROUGHNESSAOMAP //define to linearize mraoVal in case it's defined in sRGB
					mraoVal = fromSRGB(mraoVal);
				#endif

				perceptualRoughness = mraoVal.g * perceptualRoughness;
				metallic = mraoVal.b * metallic;

				ao = mraoVal.r; //optional, might be rewritten later
			#endif

			perceptualRoughness = clamp(perceptualRoughness, MINROUGHNESS, 1.0);
			metallic = clamp(metallic, 0.0, 1.0);

			float alphaRoughness = perceptualRoughness * perceptualRoughness;

			// The albedo may be defined from a base texture or a flat color
			#ifdef HAS_BASECOLORMAP
				vec4 baseColor = texture(baseColorTex, texCoord)) * baseColorFactor;
				#ifdef SRGB_BASECOLORMAP //define to linearize baseColor in case it's defined in sRGB
					baseColor = fromSRGB(baseColor);
				#endif
			#else
				vec4 baseColor = baseColorFactor;
			#endif

			vec3 f0 = vec3(0.04);
			vec3 diffuseColor = baseColor.rgb * (vec3(1.0) - f0);
			diffuseColor *= 1.0 - metallic;
			vec3 specularColor = mix(f0, baseColor.rgb, metallic);

			// Compute reflectance.
			float reflectance = max(max(specularColor.r, specularColor.g), specularColor.b);

			// For typical incident reflectance range (between 4% to 100%) set the grazing reflectance to 100% for typical fresnel effect.
			// For very low reflectance range on highly diffuse objects (below 4%), incrementally reduce grazing reflecance to 0%.
			float reflectance90 = clamp(reflectance * 25.0, 0.0, 1.0);
			vec3 specularEnvironmentR0 = specularColor.rgb;
			vec3 specularEnvironmentR90 = vec3(1.0, 1.0, 1.0) * reflectance90;

			vec3 n = getWorldFragNormal();					// normal at surface point
			vec3 v = normalize(cameraDir);					// Vector from surface point to camera, might need to flip it
			vec3 l = normalize(lightInfo.lightDirection);	// Vector from surface point to light
			vec3 h = normalize(l+v);						// Half vector between both l and v
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
				perceptualRoughness,
				metallic,
				specularEnvironmentR0,
				specularEnvironmentR90,
				alphaRoughness,
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
			vec3 color = NdotL * lightInfo.lightColor * (diffuseContrib + specContrib);

			// Calculate lighting contribution from image based lighting source (IBL)
			#ifdef USE_IBL
				color += getIBLContribution(pbrInputs, n, reflection);
			#endif

			// Apply optional PBR terms for additional (optional) shading
			// R = occlusion value
			#ifdef HAS_OCCLUSIONMAP
				vec4 aoTexVal = texture(occlusionTex, texCoord);
				#ifdef SRGB_OCCLUSIONMAP //define to linearize aoTexVal in case it's defined in sRGB
					aoTexVal = fromSRGB(aoTexVal);
				#endif
				ao = aoTexVal.r;
			#endif

			//this shouldn't cause any harm as if AO value is not defined in textures, it's defaulted to 1.0;
			color = mix(color, color * ao, occlusionStrength);

			#ifdef HAS_EMISSIVEMAP
				vec4 emTexVal = texture(emissiveTex, texCoord);
				#ifdef SRGB_EMISSIVEMAP //define to linearize emissive in case it's defined in sRGB
					emTexVal = fromSRGB(emTexVal);
				#endif
				vec3 emissive = emTexVal.rgb * emissiveFactor;
				color += emissive;
			#endif
			
			// This section uses mix to override final color for reference app visualization
			// of various parameters in the lighting equation.
			color = mix(color, F, u_ScaleFGDSpec.x);
			color = mix(color, vec3(G), u_ScaleFGDSpec.y);
			color = mix(color, vec3(D), u_ScaleFGDSpec.z);
			color = mix(color, specContrib, u_ScaleFGDSpec.w);

			color = mix(color, diffuseContrib, u_ScaleDiffBaseMR.x);
			color = mix(color, baseColor.rgb, u_ScaleDiffBaseMR.y);
			color = mix(color, vec3(metallic), u_ScaleDiffBaseMR.z);
			color = mix(color, vec3(perceptualRoughness), u_ScaleDiffBaseMR.w);
			
			#ifdef GAMMA_CORRECTION
				gl_FragColor = toSRGB( vec4(color, baseColor.a) );
			#else
				gl_FragColor = vec4(color, baseColor.a);
			#endif

			%%FRAGMENT_POST_SHADING%%
		}