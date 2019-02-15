local container = {}
container.oitFillShaderVertex = [[
	#version 150 compatibility
	#line 10004

	uniform mat4 worldMat;
	uniform mat4 viewMat;
	#if 1
		uniform mat4 projMat;
	#else
		#define projMat gl_ProjectionMatrix
	#endif

	out vec4 modelPos;
	out vec4 worldPos;
	out vec4 viewPos;

	struct EffectInfoVS {
		float sphereScale;
		//float driftFactor;
	};

	#define NUM_EFFECTS 1
	const EffectInfoVS EffectsInfoVS[NUM_EFFECTS] = EffectInfoVS[NUM_EFFECTS](
		EffectInfoVS(350.0)
	);
	
	void main() {
		modelPos = gl_Vertex;
		worldPos = worldMat * modelPos;
		viewPos = viewMat * worldPos;

		gl_Position = projMat * viewPos;
		//gl_Position = ftransform();
	}
]]

container.oitFillShaderFragment = [[
	#version 150 compatibility
	#line 20020

	uniform vec2 depthRangeSpring;

	const vec2 depthRangeTarget = vec2(0.001, 2.5);

	float RescaleToTargetRange(float viewDepth) {
		float vdNorm = (viewDepth - depthRangeSpring.x) / (depthRangeSpring.y - depthRangeSpring.x);
		return depthRangeTarget.x + vdNorm * (depthRangeTarget.y - depthRangeTarget.x);
	}

	void main() {

		vec4 color = vec4(0.0, 0.0, 0.5, 0.2);

		/*
		// Assuming that the projection matrix is a perspective projection
		// gl_FragCoord.w returns the inverse of the oPos.w register from the vertex shader
		float viewDepth = abs(1.0 / gl_FragCoord.w);

		// Tuned to work well with FP16 accumulation buffers and 0.001 < linearDepth < 2.5
		// See Equation (9) from http://jcgt.org/published/0002/02/09/
		float linearTargetDepth = RescaleToTargetRange(viewDepth);

		float weight = clamp(0.03 / (1e-5 + pow(linearTargetDepth, 4.0)), 1e-2, 3e3);

		gl_FragData[0] = vec4(color.rgb * color.a * weight, color.a);
		gl_FragData[1].r = color.a * weight;
		*/
		//gl_FragData[0] = color;
		gl_FragColor = color;
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
	#line 40003

	uniform sampler2D texA;
	uniform sampler2D texB;

	void main() {
		/*
		vec4 accum = texelFetch(texA, ivec2(gl_FragCoord.xy), 0);
		float r = accum.a;
		accum.a = texelFetch(BTexture, ivec2(gl_FragCoord.xy), 0).r;

		gl_FragColor = vec4(accum.rgb / clamp(accum.a, 1e-4, 5e4), r);
		*/
		vec4 accum = texelFetch(texA, ivec2(gl_FragCoord.xy), 0);
		gl_FragColor = vec4(accum);
		//gl_FragColor = vec4(gl_FragCoord.xy / 1024.0, 1.0, 1.0);
	}

	/*
	void main() {
    int2 C = int2(gl_FragCoord.xy);
    float  revealage = texelFetch(revealageTexture, C, 0).r;
    if (revealage == 1.0) {
        // Save the blending and color texture fetch cost
        discard;
    }

    float4 accum     = texelFetch(accumTexture, C, 0);
    // Suppress overflow
    if (isinf(maxComponent(abs(accum)))) {
        accum.rgb = float3(accum.a);
    }

    float3 averageColor = accum.rgb / max(accum.a, 0.00001);


    // dst =  (accum.rgb / accum.a) * (1 - revealage) + dst * revealage
    gl_FragColor = float4(averageColor, 1.0 - revealage);
	}
	*/
]]

return container