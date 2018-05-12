-- $Id: ShieldSphereColorHQ.lua 3171 2008-11-06 09:06:29Z det $
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local ShieldSphereColorHQParticle = {}
ShieldSphereColorHQParticle.__index = ShieldSphereColorHQParticle

local sphereList = {}
local shieldShader

local methodUniform
local timerUniform
local color1Uniform
local color2Uniform
local colorMultUniform
local colorMixUniform
local shieldPosUniform
local shieldSizeUniform
local shieldSizeDriftUniform
local marginUniform
local uvMulUniform

local viewInvUniform

local hitPointCountUniform
local hitPointsUniform


-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle.GetInfo()
	return {
		name		= "ShieldSphereColorHQ",
		backup		= "ShieldSphereColor", --// backup class, if this class doesn't work (old cards,ati's,etc.)
		desc		= "",

		layer		= -23, --// extreme simply z-ordering :x

		--// gfx requirement
		fbo			= false,
		shader		= true,
		rtt			= false,
		ctt			= false,
	}
end

ShieldSphereColorHQParticle.Default = {
	pos				= {0, 0, 0}, -- start pos
	layer			= -23,

	life			= math.huge,

	size			= 10,
	margin			= 1,

	colormap1	= { {0, 0, 0, 0} },
	colormap2	= { {0, 0, 0, 0} },

	repeatEffect = false,
	shieldSize = "large",
}

-- (dx, dy, dz, mag, AoE) x 8
local MAX_POINTS = 8

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local glCallList = gl.CallList

function ShieldSphereColorHQParticle:Visible()
	return self.visibleToMyAllyTeam
end

local PACE = 1200

local lastTexture = ""

function ShieldSphereColorHQParticle:BeginDraw()
	--gl.Clear(GL.STENCIL_BUFFER_BIT, 0)
	gl.DepthMask(false)
	gl.UseShader(shieldShader)

	local low, high = Spring.GetDrawFrame()
	gl.Uniform(timerUniform,	(high * 65535 + low) / PACE)
	gl.UniformMatrix(viewInvUniform, "viewinverse")
end

function ShieldSphereColorHQParticle:EndDraw()
	gl.DepthMask(false)
	gl.UseShader(0)

	gl.Texture(0, false)
	gl.Texture(1, false)
	lastTexture = ""

	gl.Culling(false)
end

function ShieldSphereColorHQParticle:Draw()

	gl.Culling(GL.FRONT)
	if not self.texture then
		gl.UniformInt(methodUniform, 0)
	else
		gl.UniformInt(methodUniform, 1)
		if (lastTexture ~= self.texture) then
			gl.Texture(0, self.texture)
			lastTexture = self.texture
		end
	end
	
	gl.Texture(1, "$heightmap")

	local col1, col2 = GetShieldColor(self.unit, self)

	local hitTable
	if (GG and GG.GetShieldHitPositions) then --means high quality shield rendering is in place
		hitTable = GG.GetShieldHitPositions(self.unit)
	end

	gl.Uniform(color1Uniform, col1[1], col1[2], col1[3], col1[4])
	gl.Uniform(color2Uniform, col2[1], col2[2], col2[3], col2[4])
	gl.Uniform(colorMultUniform, 1, 1, 1, 1)

	local mix = self.mix
	gl.Uniform(colorMixUniform, mix[1], mix[2], mix[3], mix[4])

	local pos = self.pos
	gl.Uniform(shieldPosUniform, pos[1], pos[2], pos[3], 0)

	gl.Uniform(shieldSizeUniform, self.size)
	gl.Uniform(shieldSizeDriftUniform, self.sizeDrift)
	gl.Uniform(marginUniform, self.marginHQ)
	gl.Uniform(uvMulUniform, self.uvMul)

	if hitTable then
		local hitPointCount = math.min(#hitTable, MAX_POINTS)
		gl.UniformInt(hitPointCountUniform, hitPointCount)

		local hitArray = {}
		if hitPointCount > 0 then
			--Spring.Echo("hitPointCount", hitPointCount)
			for i = 1, hitPointCount do
				table.insert(hitArray, hitTable[i].dx)
				table.insert(hitArray, hitTable[i].dy)
				table.insert(hitArray, hitTable[i].dz)
				table.insert(hitArray, hitTable[i].mag)
				table.insert(hitArray, hitTable[i].aoe)
			end
		end
		gl.UniformArray(hitPointsUniform, 2, hitArray)
	end

	gl.PushMatrix()
	--gl.MatrixMode(GL.MODELVIEW);
	--gl.LoadIdentity()
	gl.LoadMatrix("view")
	gl.Scale(1,1,1)
	local x, y, z = Spring.GetUnitPosition(self.unit)
	gl.Translate(x, y, z)
	glCallList(sphereList[self.shieldSize])
	gl.PopMatrix()
	
	--[[

	if self.drawBackHQ then
		gl.Culling(GL.BACK)

		gl.Uniform(colorMultUniform, self.drawBackHQ[1], self.drawBackHQ[2], self.drawBackHQ[3], self.drawBackHQ[4])

		if self.drawBackMargin then
			gl.Uniform(marginUniform, self.drawBackMargin)
		end

		glCallList(sphereList[self.shieldSize])
	end
	]]--
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle:Initialize()
	shieldShader = gl.CreateShader({
		vertex = [[
		uniform vec4 pos;
		uniform float margin;
		uniform float size;

		uniform float uvMul;

		uniform float timer;

		uniform float sizeDrift;

		varying float opac;

		varying vec3 normal;
		
		varying vec3 curVertex;

		#define DRIFT_FREQ 25.0

		#define PI 3.141592653589793
		
		uniform mat4 viewMatrixI;

		#define nsin(x) (0.5 * sin(x) + 0.5)
		
		float rand(float p)
		{
			return fract(sin(p) * 43758.5453123);
		}

		void main()
		{
			gl_TexCoord[0] = gl_MultiTexCoord0;

			float r = length(gl_Vertex.xyz);
			float theta = acos(gl_Vertex.z / r);
			float phi = atan(gl_Vertex.y, gl_Vertex.x);

			//r += sizeDrift * r * nsin(2.0 * theta + 3.0 * phi + timer * DRIFT_FREQ);
			r += 0.0 * r * nsin(2.0 * theta + 3.0 * phi + timer * 0.0);

			vec4 myVertex;
			myVertex = vec4(r * sin(theta) * cos(phi), r * sin(theta) * sin(phi), r * cos(theta), 1.0f);

			vec4 size4 = vec4(size, size, size, 1.0f);
			gl_Position = gl_ModelViewProjectionMatrix * (myVertex * size4 + pos);
			curVertex = (gl_Position * viewMatrixI).xyz;

			//normal = normalize(gl_NormalMatrix * gl_Normal);
			normal = normalize(gl_Normal);

			vec3 vertex = vec3(gl_ModelViewMatrix * gl_Vertex);
			float angle = dot(normal, vertex) * inversesqrt( dot(normal, normal) * dot(vertex, vertex) ); //dot(norm(n), norm(v))
			opac = pow( abs( angle ) , margin);
		}
		]],
		fragment = [[
		varying float opac;
		varying vec3 normal;

		uniform float timer;

		uniform mat4 viewMatrixI;

		uniform vec4 color1;
		uniform vec4 color2;
		uniform vec4 colorMult;
		uniform vec4 colorMix;

		uniform float uvMul;

		uniform float sizeDrift;

		uniform int hitPointCount;
		uniform float hitPoints[5 * MAX_POINTS];

		uniform sampler2D tex0;
		
		uniform sampler2D heightMap;

		uniform int method;
		
		uniform int mapSizeX;
		uniform int mapSizeZ;
		
		varying vec3 curVertex;

		#define PI 3.141592653589793

		#define HEXSCALE 80.0

		#define SZDRIFTTOUV 7.0

		#define nsin(x) (0.5 * sin(x) + 0.5)

		float hex(vec2 p, float width, float coreSize)
		{
			p.x *= 0.57735 * 2.0;
			p.y += mod(floor(p.x), 2.0)*0.5;
			p = abs((mod(p, 1.0) - 0.5));
			float val = abs(max(p.x*1.5 + p.y, p.y*2.0) - 1.0);
			return smoothstep(coreSize, width, val);
		}

		vec2 GetRippleLinearFallOffCoord(vec2 uv, vec2 point, float mag, float waveFreq, float waveSpeed, float waveDist, float time)
		{
			vec2 dir = uv - point;
			float dist = distance(uv, point);
			float magMult = (1.0 - smoothstep(0.0, waveDist, dist));
			vec2 offset = dir * (nsin(dist * waveFreq - time * waveSpeed)) * mag * magMult;
			return offset;
		}

		vec2 GetRippleCoord(vec2 uv, vec2 point, float mag, float waveFreq, float waveSpeed, float time)
		{
			vec2 dir = uv - point;
			float dist = distance(uv, point);
			vec2 offset = dir * (nsin(dist * waveFreq - time * waveSpeed)) * mag;
			return offset;
		}


		vec2 RadialCoords(vec3 a_coords)
		{
			vec3 a_coords_n = normalize(a_coords);
			float lon = atan(a_coords_n.z, a_coords_n.x);
			float lat = acos(a_coords_n.y);
			vec2 sphereCoords = vec2(lon, lat) / PI;
			return vec2(sphereCoords.x * 0.5 + 0.5, 1.0 - sphereCoords.y);
		}

/* Magic angle that equalizes projected area of squares on sphere. */
#define MAGIC_ANGLE 0.883475248536 // radians

float warp_theta = MAGIC_ANGLE;
float tan_warp_theta = tan(warp_theta);

float N = 16.0;

/* Return a permutation matrix whose first two columns are u and v basis 
   vectors for a cube face, and whose third column indicates which axis 
   (x,y,z) is maximal. */
mat3 getPT(in vec3 p) {

    vec3 a = abs(p);
    float c = max(max(a.x, a.y), a.z);    

    vec3 s = c == a.x ? vec3(1.,0,0) : c == a.y ? vec3(0,1.,0) : vec3(0,0,1.);

    s *= sign(dot(p, s));
    vec3 q = s.yzx;
    return mat3(cross(q,s), q, s);

}

/* Warp to go cube -> sphere */
vec2 warp(vec2 x) {
    return tan(warp_theta*x)/tan_warp_theta;
}

/* Unwarp to go sphere -> cube */
vec2 unwarp(vec2 x) {
    return atan(x*tan_warp_theta)/warp_theta; 
}

/* Return squared great circle distance of two points projected onto sphere. */
float sphereDist2(vec3 a, vec3 b) {
	// Fast-ish approximation for acos(dot(normalize(a), normalize(b)))^2
    return 2.0-2.0*dot(normalize(a),normalize(b));
}

/* Just used to visualize distance from spherical Voronoi cell edges. */
float bisectorDistance(vec3 p, vec3 a, vec3 b) {
    vec3 n1 = cross(a,b);
    vec3 n2 = normalize(cross(n1, 0.5*(normalize(a)+normalize(b))));
    return abs(dot(p, n2));             
}

/* Get index (0-5) for axis. */
float axisToIdx(vec3 axis) {
    
    float idx = dot(abs(axis), vec3(0.0, 2.0, 4.0));
    if (dot(axis, vec3(1.0)) < 0.0) { idx += 1.0; }
    
    return idx;
    
}

/* From https://www.shadertoy.com/view/4djSRW */

#define HASHSCALE3 vec3(.1031, .1030, .0973)

vec3 hash33(vec3 p3) {
	p3 = fract(p3 * HASHSCALE3);
    p3 += dot(p3, p3.yxz+19.19);
    return fract((p3.xxy + p3.yxx)*p3.zyx);

}

bool wrapCube(in mat3 PT, 
              inout vec2 uvn,
              out mat3 PTn) {
    
    // new uv location might have gone off edge of cube face
    // ...see if it has by comparing to clamped version
    vec2 uvn_clamp = clamp(uvn, -1.0, 1.0);
    vec2 extra = abs(uvn_clamp - uvn);

    // it doesn't make sense to go over both corners so only allow
    // overflow/underflow in u or v but not both
    if (min(extra.x, extra.y) > 0.0) {
        
        return false;
        
    } else {            

        // check if we have gone off starting face
        float esum = extra.x + extra.y;

        if (esum > 0.0) {
            // need to re-establish what face we are on
            vec3 p = PT * vec3(uvn_clamp, 1.0 - esum);
            PTn = getPT(p);
            uvn = (p * PTn).xy;
        } else {
            // same as starting face
            PTn = PT;
        }

        return true;
        
    }
    
}


/* Color the sphere/cube points. */
vec3 gcolor(vec3 pos) {

    // get permutation matrix 
    mat3 PT = getPT(pos);
    
    // project to cube face
    vec3 cf = pos * PT; 
    
    // UV is in [-1, 1] range
    vec2 uv = cf.xy / cf.z; 
    
    // unwarp from sphere -> cube (approximtion of atan)
    uv = unwarp(uv);      
    
    // for viz only
    //pos /= (dot(pos, PT[2]));
    
    // quantize uv of nearest cell
    vec2 uv_ctr = (floor(0.5*N*uv + 0.5) + 0.5)*2.0/N;

    // store distance, material & point for 1st, 2nd closest
    float d1 = 1e4, d2 = 1e4;

    vec3 p1 = vec3(0.0), p2 = vec3(0.0);

    // for neighbors in 4x4 neighborhood
    for (int du=-1; du<=1; ++du) {
        for (int dv=-1; dv<=1; ++dv) {
            
            mat3 PTn;
            
            // any time you see 2.0/N it maps from [-1, 1] to [0, N]
            vec2 uvn = uv_ctr + vec2(float(du), float(dv))*2.0/N;
            
            if (wrapCube(PT, uvn, PTn)) {

                // now generate a unique id for the cell
                vec2 ssn = floor((uvn*0.5 + 0.5)*N);
                float faceid = axisToIdx(PTn[2]);
                vec3 id = vec3(ssn, faceid);
                
                // generate 3 random #'s from id
                //vec3 r = hash33(id);
				vec3 r = vec3(0.5, 0.5, 0.0);
                //r.xy = nsin(5.0*timer + r.xy * 2.0 * PI);
				
                // randomize dot position within cell
                //uvn += (r.xy-0.5)*2.0/N;
				
				//uvn = nsin(timer + 2.0*PI*uvn);

                // warp cube -> sphere
                uvn = warp(uvn);

                // can save 1 multiplication over general matrix mult.
                // because we know last coord is 1
                vec3 pn = PTn[0]*uvn.x + PTn[1]*uvn.y + PTn[2];

                // update distances if closer
                float dn = sphereDist2(pn, pos);
 
                if (dn < d1) {
                    d2 = d1; p2 = p1;
                    d1 = dn; p1 = pn;
                } else if (dn < d2) {
                    d2 = dn; p2 = pn;
                }

            }
            
        }
            
    }

    // get distance to voronoi boundary
    float b = bisectorDistance(pos, p2, p1);

	vec3 c = vec3(0.0);
	
	//voronoi isolines
	//c = mix(vec3(1.0), c, smoothstep(0.01, 0.3, fract(b * 50.0)));
	
    // voronoi lines    
    c = mix(vec3(1.0), c, smoothstep(0.001, 0.01, b));

    return c;
    
    
}

		vec3 GetRippleLinearFallOffCoord3(vec3 point, vec3 center, float mag, float waveFreq, float waveSpeed, float waveDist, float time)
		{		
			vec3 dir = normalize(point - center);
			float dist = sphereDist2(point, center);
			float magMult = (1.0 - smoothstep(0.0, waveDist, dist));
			vec3 offset = dir * (nsin(dist * waveFreq - time * waveSpeed)) * mag * magMult;
			return offset;
		}
		

		void main(void)
		{
			//vec3 color = 1.0 - gcolor(normal);
			mat3 PT;
			vec2 g;

			// Get grid coords
			//posToGrid(normal, PT, g);
			//float color = hex(g, 0.1, 0.01);
			
			//gl_FragColor = vec4(color, color, color, 0.5);
			
			vec3 offset3 = vec3(0.0);

			for (int hitPointIdx = 0; hitPointIdx < MAX_POINTS; ++hitPointIdx) {
				if (hitPointIdx < hitPointCount) {
					vec3 impactPoint = vec3(hitPoints[5 * hitPointIdx + 0], hitPoints[5 * hitPointIdx + 1], hitPoints[5 * hitPointIdx + 2]);
					vec3 impactPointAdj = normalize((vec4(impactPoint, 1.0) * viewMatrixI).xyz);
					
					float mag = hitPoints[5 * hitPointIdx + 3];
					float aoe = hitPoints[5 * hitPointIdx + 4];
					offset3 += GetRippleLinearFallOffCoord3(normal, impactPointAdj, mag, 100.0, -120.0, aoe, timer);
				}
			}

			vec3 pointAdj = normalize(normal + offset3); //this is to trick GLSL compiler, otherwise shot-induced ripple is not drawn. Silly....
			
			
			
			vec3 color = gcolor(pointAdj);
			float col = color.x;
			//col *= (0.2 + 1.0 * nsin(5.0 * pointAdj.x + 7.0 * pointAdj.y + 11.0 * pointAdj.z + timer * 15.0));
			//col *= (0.5 + 0.5 * nsin(25.0 * (pointAdj.z) + timer * 25.0));
			vec4 texel = vec4(col);
			
			//gl_FragColor = vec4(0.5);
			
			vec4 colorMultAdj = colorMult * (1.0 + length(offset3) * 1.0);
			vec4 color1M = color1 * colorMultAdj;
			vec4 color2M = color2 * colorMultAdj;
			vec4 color1Tex = mix(color1, texel, colorMix);
			vec4 color1TexM = color1Tex * colorMultAdj;
			gl_FragColor = mix(color1TexM, color2M, opac);
			//gl_FragColor = mix(color1Tex, color2M, opac);
			
			vec2 hmuv = curVertex.xz / vec2(mapSizeX, mapSizeZ);
			vec4 height = texture2D(heightMap, hmuv);
			
			float hDiff = abs(height.x - curVertex.y);
			
			if (hDiff < 3.0) texel = vec4(1.0);
			
			//texel *= 1.0 + 4.0 * (1.0 - step(0.5, hDiff));
			//texel *= 1.0 + 4.0 * (1.0 - step(0.5, hDiff));
			
			gl_FragColor = texel;		
			
		}
	]],
		uniformInt = {
			tex0 = 0,
			heightMap = 1,
			mapSizeX = Game.mapSizeX,
			mapSizeZ = Game.mapSizeZ,			
		},
		definitions = {
			string.gsub("#define MAX_POINTS _PTS_NUM_ \n", "_PTS_NUM_", tostring(MAX_POINTS)),
		},
	})

	local shLog = gl.GetShaderLog()
	if (string.len(shLog or "") > 0) then
		print(PRIO_MAJOR, "LUPS->Shield: shader warnings & errors: "..shLog)
		
	end
	
	if not shieldShader then
		return false
	end

	timerUniform = gl.GetUniformLocation(shieldShader, 'timer')
	viewInvUniform = gl.GetUniformLocation(shieldShader, 'viewMatrixI')

	methodUniform = gl.GetUniformLocation(shieldShader, 'method')

	color1Uniform = gl.GetUniformLocation(shieldShader, 'color1')
	color2Uniform = gl.GetUniformLocation(shieldShader, 'color2')
	colorMultUniform = gl.GetUniformLocation(shieldShader, 'colorMult')
	colorMixUniform = gl.GetUniformLocation(shieldShader, 'colorMix')
	shieldPosUniform = gl.GetUniformLocation(shieldShader, 'pos')
	shieldSizeUniform = gl.GetUniformLocation(shieldShader, 'size')
	shieldSizeDriftUniform = gl.GetUniformLocation(shieldShader, 'sizeDrift')
	marginUniform = gl.GetUniformLocation(shieldShader, 'margin')
	uvMulUniform = gl.GetUniformLocation(shieldShader, 'uvMul')

	hitPointCountUniform = gl.GetUniformLocation(shieldShader, 'hitPointCount')
	hitPointsUniform = gl.GetUniformLocation(shieldShader, 'hitPoints')

	sphereList = {
		large = gl.CreateList(DrawSphere, 0, 0, 0, 1, 60),
		medium = gl.CreateList(DrawSphere, 0, 0, 0, 1, 50),
		small = gl.CreateList(DrawSphere, 0, 0, 0, 1, 40),
	}
end

function ShieldSphereColorHQParticle:Finalize()
	gl.DeleteShader(shieldShader)
	for _, list in pairs(sphereList) do
		gl.DeleteList(list)
	end
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle:CreateParticle()
	self.dieGameFrame = Spring.GetGameFrame() + self.life
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle:Update()
end

-- used if repeatEffect=true;
function ShieldSphereColorHQParticle:ReInitialize()
	self.dieGameFrame = self.dieGameFrame + self.life
end

function ShieldSphereColorHQParticle.Create(Options)
	local newObject = MergeTable(Options, ShieldSphereColorHQParticle.Default)
	setmetatable(newObject, ShieldSphereColorHQParticle)	-- make handle lookup
	newObject:CreateParticle()
	return newObject
end

function ShieldSphereColorHQParticle:Destroy()
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

return ShieldSphereColorHQParticle