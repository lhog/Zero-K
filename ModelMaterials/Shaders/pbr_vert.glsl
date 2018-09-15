		%%VERTEX_GLOBAL_NAMESPACE%%

		uniform mat4 camera;		//ViewMatrix (gl_ModelViewMatrix is ModelMatrix!)
		uniform vec3 cameraPos;

		out Data {
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

		//For a moment pretend we have passed OpenGL 2.0 era
		#define modelMatrix gl_ModelViewMatrix			// don't trust the ModelView name, it's modelMatrix in fact
		#define viewMatrix camera						// viewMatrix is mat4 uniform supplied by Gadget(?)
		#define projectionMatrix gl_ProjectionMatrix	// just because I don't like gl_BlaBla

		void main(void)	{
			vec4 modelPos = gl_Vertex;
			vec3 modelNormal = gl_Normal;

			%%VERTEX_PRE_TRANSFORM%%

			#ifdef HAS_NORMALS
				worldNormal = normalize(gl_NormalMatrix * modelNormal);
				#ifdef HAS_TANGENTS
					// The use of gl_MultiTexCoord[5,6] is a hack due to lack of support of proper attributes
					// TexCoord5 and TexCoord6 are defined and filled in engine. See: rts/Rendering/Models/AssParser.cpp
					vec3 modelTangent = gl_MultiTexCoord5.xyz;
					vec3 worldTangent = normalize(vec3(modelMatrix * vec4(modelTangent, 0.0)));

					#if 0 //take modelBitangent from attributes
						vec3 modelBitangent = gl_MultiTexCoord6.xyz;
						vec3 worldBitangent = normalize(vec3(modelMatrix * vec4(modelBitangent, 0.0)));
					#else //calculate worldBitangent
						#ifdef DO_REORTHOGONALIZATION
							// Gram-Schmidt process to re-orthogonalize the TBN vectors
							worldTangent = normalize(worldTangent - worldNormalN * dot(worldNormalN, worldTangent));
						#endif
						vec3 worldBitangent = normalize( cross(worldNormalN, worldTangent) );
					#endif

					//TODO check handedness: see http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-13-normal-mapping/ (Handedness)

					worldTBN = mat3(worldTangent, worldBitangent, worldNormal);
				#endif
			#endif
			#line 100062
			vec4 worldPos4 = modelMatrix * modelPos;
			worldPos = worldPos4.xyz / worldPos4.w; //doesn't make much sense (?)

			//cameraDir = worldPos - cameraPos;
			cameraDir = cameraPos - worldPos;

			// TODO: multiply by gl_TextureMatrix[0] ?
			texCoord = gl_MultiTexCoord0.xy;

			//TODO:
			// 1) shadows: shadowTexCoord
			// 2) flashlights ?
			// 3) treadoffset ?

			gl_Position = projectionMatrix * (viewMatrix * worldPos4);

			%%VERTEX_POST_TRANSFORM%%
		}