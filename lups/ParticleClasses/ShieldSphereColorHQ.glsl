local container = {}
container.oitFillShaderVertex = [[
	#version 150 compatibility
	#line 10004

	//uniform mat4 worldMat;
	uniform vec4 translationScale;
	uniform vec3 rotPYR;

	uniform mat4 viewMat;
	#if 1
		uniform mat4 projMat;
	#else
		#define projMat gl_ProjectionMatrix
	#endif

	out vec4 modelPos;
	out vec4 worldPos;
	out vec4 viewPos;

	vec4 RotationQuat(vec3 axis, float angle)
	{
		//axis = normalize(axis);
		float c = cos(0.5 * angle);
		float s = sqrt(1.0 - c * c);
		return vec4(axis.x * s, axis.y * s, axis.z * s, c);
	}

	vec3 Rotate(vec3 p, vec4 q)
	{
		return p + 2.0 * cross(q.xyz, cross(q.xyz, p) + q.w * p);
	}

	vec3 Rotate(vec3 p, vec3 axis, float angle)
	{
		return Rotate(p, RotationQuat(axis, angle));
	}

	void main() {
		modelPos = gl_Vertex;

		//worldPos = worldMat * modelPos;
		worldPos = vec4(modelPos.xyz * vec3(translationScale.w), 1.0);			//scaling
		worldPos.xyz = Rotate(worldPos.xyz, vec3(0.0, 0.0, 1.0), rotPYR.y);		//rotation around Yaw axis
		worldPos.xyz += translationScale.xyz; 									//translation in world space

		viewPos = viewMat * worldPos;

		gl_Position = projMat * viewPos;
		//gl_Position = ftransform();
	}
]]

container.oitFillShaderFragment = [[
	#version 150 compatibility
	#line 20020

	#define MSAA_LEVEL ###MSAA_LEVEL###
	#if (MSAA_LEVEL > 1)
		uniform sampler2DMS depthTex;
	#else
		uniform sampler2D depthTex;
	#endif

	#if 1
		uniform mat4 projMat;
	#else
		#define projMat gl_ProjectionMatrix
	#endif
	
	//uniform mat4 inverseViewMat;

	uniform vec2 depthRangeSpring;

	in vec4 modelPos;
	in vec4 worldPos;
	in vec4 viewPos;

	#if (MSAA_LEVEL > 1)
		vec4 texelFetchMS(sampler2DMS tex, ivec2 coord) {
			vec4 result = vec4(0.0);
			for (int sample = 0; sample < MSAA_LEVEL; ++sample) {
				result += texelFetch(tex, coord, sample);
			}
			return result / vec4(MSAA_LEVEL);
		}
	#endif

	#define ClipToNDC(clipVal) (2.0 * clipVal - 1.0)

	float GetViewSpaceDepth(float depthNDC) {
		float A = projMat[2][2];
		float B = projMat[3][2];
		return B / (A + depthNDC);
	}

	const vec2 depthRangeTarget = vec2(0.001, 2.5);
	float RescaleToTargetRange(float viewDepth) {
		float vdNorm = (viewDepth - depthRangeSpring.x) / (depthRangeSpring.y - depthRangeSpring.x);
		return depthRangeTarget.x + vdNorm * (depthRangeTarget.y - depthRangeTarget.x);
	}

	#define DO_OIT ###DO_OIT###

	void main() {

		vec4 color = vec4(0.0, 0.0, 0.5, 0.2);

		vec2 uv = gl_FragCoord.xy / vec2(2560.0, 1080.0);

		#if (MSAA_LEVEL > 1)
			float opaqueDepth = texelFetchMS( depthTex, ivec2(gl_FragCoord.xy) ).r;
		#else
			float opaqueDepth = texelFetch( depthTex, ivec2(gl_FragCoord.xy), 0 ).r;
		#endif

		//color.r = abs(gl_FragCoord.z - opaqueDepth);
		color.a = 1.0;
		float d;
		//d = smoothstep(texture(depthTex, uv).r, 0.95, 0.98);
		//d = smoothstep(texelFetch(depthTex, ivec2(gl_FragCoord.xy), 0).r, 0.95, 0.98);
		//d = smoothstep(texelFetch(depthTex, ivec2( uv * textureSize(depthTex, 0) ), 0).r, 0.95, 0.98);
		//d = smoothstep(gl_FragCoord.z, 0.95, 0.98);

		//d = 1.0 - smoothstep( abs(texture(depthTex, uv).r - gl_FragCoord.z), 0.1, 0.2 );

		//d = GetViewSpaceDepth( opaqueDepth );
		//d = -viewPos.z / 8000.0;
		
		float depthOpaqueView = GetViewSpaceDepth( opaqueDepth );
		
		//vec4 opaqueDepthWorldPos = inverseViewMat * vec4(viewPos.xy, -depthOpaqueView, viewPos.w);
		
		//vec4 invWorldPos = inverseViewMat * vec4(viewPos.xy, viewPos.z, viewPos.w); --same as worldPos!
		
		//d = 1.0 - smoothstep( worldPos.y - opaqueDepthWorldPos.y, 0.0, 8.0 );
		//d = opaqueDepthWorldPos.y / 100.0;
		//d = worldPos.y / 100.0;
		//d = invWorldPos.y / 100.0;
		//d = 1.0 - smoothstep( abs(worldPos.y - opaqueDepthWorldPos.y), 0.0, 8.0 );
		
		// abs((viewPos.z) - (-depthOpaqueView)) == abs(viewPos.z + depthOpaqueView)
		
		// Doing this in view space is probably wrong, but cheaper.
		d = 1.0 - smoothstep( abs(viewPos.z + depthOpaqueView), 0.0, 2.0 );

		color.rgb = vec3(d);

		#if DO_OIT
			// Looks like to give same result
			#if 1
				// Assuming that the projection matrix is a perspective projection
				// gl_FragCoord.w returns the inverse of the oPos.w register from the vertex shader
				float viewDepth = abs(1.0 / gl_FragCoord.w);
			#else
				float viewDepth = abs(viewPos.z);
			#endif

			// Tuned to work well with FP16 accumulation buffers and 0.001 < linearDepth < 2.5
			// See Equation (9) from http://jcgt.org/published/0002/02/09/
			float linearTargetDepth = RescaleToTargetRange(viewDepth);

			float weight = clamp(0.03 / (1e-5 + pow(linearTargetDepth, 4.0)), 1e-2, 3e3);

			gl_FragData[0] = vec4(color.rgb * color.a * weight, color.a);
			gl_FragData[1].r = color.a * weight;
		#else
			gl_FragData[0] = color;
		#endif

		//gl_FragData[0] = color;
		//gl_FragColor = color;
	}
]]

container.blitShaderVertex = [[
	#version 150 compatibility
	#line 30003

	void main() {
		gl_Position = ftransform();
	}
]]

container.blitShaderFragment = [[
	#version 150 compatibility
	#line 40115

	#define MSAA_LEVEL ###MSAA_LEVEL###
	#if (MSAA_LEVEL > 1)
		uniform sampler2DMS texA;
		uniform sampler2DMS texB;
	#else
		uniform sampler2D texA;
		uniform sampler2D texB;
	#endif

	#define DO_OIT ###DO_OIT###

	#if (MSAA_LEVEL > 1)
		vec4 texelFetchMS(sampler2DMS tex, ivec2 coord) {
			vec4 result = vec4(0.0);
			for (int sample = 0; sample < MSAA_LEVEL; ++sample) {
				result += texelFetch(tex, coord, sample);
			}
			return result / vec4(MSAA_LEVEL);
		}
	#endif

	void main() {
		#if (MSAA_LEVEL > 1)
			vec4 accum = texelFetchMS(texA, ivec2(gl_FragCoord.xy));
		#else
			vec4 accum = texelFetch(texA, ivec2(gl_FragCoord.xy), 0);
		#endif

		#if DO_OIT
			float revealage = accum.a;
			if (revealage == 1.0) {
				// Save the blending and color texture fetch cost
				discard;
			}

			#if (MSAA_LEVEL > 1)
				accum.a = texelFetchMS(texB, ivec2(gl_FragCoord.xy)).r;
			#else
				accum.a = texelFetch(texB, ivec2(gl_FragCoord.xy), 0).r;
			#endif

			if ( any(isinf(abs(accum))) ) {
				accum.rgb = vec3(accum.a);
			}

			vec3 averageColor = accum.rgb / max(accum.a, 0.00001);

			gl_FragColor = vec4(averageColor, 1.0 - revealage);

			//gl_FragColor.rgb *= gl_FragColor.a; //alpha pre-multiply
			//gl_FragColor.a = pow(gl_FragColor.a, 1.5);
		#else
			gl_FragColor = accum;
		#endif
	}
]]

return container