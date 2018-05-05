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

	glCallList(sphereList[self.shieldSize])

	if self.drawBackHQ then
		gl.Culling(GL.BACK)

		gl.Uniform(colorMultUniform, self.drawBackHQ[1], self.drawBackHQ[2], self.drawBackHQ[3], self.drawBackHQ[4])

		if self.drawBackMargin then
			gl.Uniform(marginUniform, self.drawBackMargin)
		end

		--glCallList(sphereList[self.shieldSize])
	end
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

		#define DRIFT_FREQ 25.0

		#define PI 3.141592653589793

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

			r += sizeDrift * r * nsin(2*theta + 3*phi + timer * DRIFT_FREQ);

			vec4 myVertex;
			myVertex = vec4(r * sin(theta) * cos(phi), r * sin(theta) * sin(phi), r * cos(theta), 1.0f);

			vec4 size4 = vec4(size, size, size, 1.0f);
			gl_Position = gl_ModelViewProjectionMatrix * (myVertex * size4 + pos);

			normal = normalize(gl_NormalMatrix * gl_Normal);

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

		uniform int method;

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

/* Try to restrict branching? Don't know if this has any effect... */
#define RESTRICT_BRANCHING

float warp_theta = MAGIC_ANGLE;
float tan_warp_theta = tan(warp_theta);

const float N = 15;

/* Return a permutation matrix whose first two columns are u and v basis 
   vectors for a cube face, and whose third column indicates which axis 
   (x,y,z) is maximal. */
mat3 getPT(in vec3 p) {

    vec3 a = abs(p);
    float c = max(max(a.x, a.y), a.z);    
#ifdef RESTRICT_BRANCHING
    vec3 s = step(vec3(c), a);
    s.yz -= vec2(s.x*s.y, (s.x + s.y - s.x*s.y)*s.z);
#else
    vec3 s = c == a.x ? vec3(1.,0,0) : c == a.y ? vec3(0,1.,0) : vec3(0,0,1.);
#endif
    s *= sign(dot(p, s));
    vec3 q = s.yzx;
    return mat3(cross(q,s), q, s);

}

/* For any point in 3D, obtain the permutation matrix, as well as grid coordinates
   on a cube face. */
void posToGrid(in vec3 pos, out mat3 PT, out vec2 g) {
    
    // Get permutation matrix and cube face id
    PT = getPT(pos);
    
    // Project to cube face
    vec3 c = pos * PT;     
    vec2 p = c.xy / c.z;      
    
    // Unwarp through arctan function
    vec2 q = atan(p*tan_warp_theta)/warp_theta; 
    
    // Map [-1,1] interval to [0,N] interval
    g = (q*0.5 + 0.5)*N;
    
}


/* For any grid point on a cube face, along with projection matrix, 
   obtain the 3D point it represents. */
vec3 gridToPos(in mat3 PT, in vec2 g) {
    
    // Map [0,N] to [-1,1]
    vec2 q = g/N * 2.0 - 1.0;
    
    // Warp through tangent function
    vec2 p = tan(warp_theta*q)/tan_warp_theta;

    // Map back through permutation matrix to place in 3D.
    return PT * vec3(p, 1.0);
    
}


/* Return whether a neighbor can be identified for a particular grid cell.
   We do not allow moves that wrap more than one face. For example, the 
   bottom-left corner (0,0) on the +X face may get stepped by (-1,0) to 
   end up on the -Y face, or, stepped by (0,-1) to end up on the -Z face, 
   but we do not allow the motion (-1,-1) from that spot. If a neighbor is 
   found, the permutation/projection matrix and grid coordinates of the 
   neighbor are computed.
*/
bool gridNeighbor(in mat3 PT, in vec2 g, in vec2 delta, out mat3 PTn, out vec2 gn) {

    vec2 g_dst = g.xy + delta;
    vec2 g_dst_clamp = clamp(g_dst, 0.0, N);

    vec2 extra = abs(g_dst_clamp - g_dst);
    float esum = extra.x + extra.y;
 
#ifdef RESTRICT_BRANCHING    
        
    vec3 pos = PT * vec3(g_dst_clamp/N*2.0-1.0, 1.0 - 2.0*esum/N);
    PTn = getPT(pos);
    gn = ((pos*PTn).xy*0.5 + 0.5) * N;
    
    return min(extra.x, extra.y) == 0.0 && esum < N;
    
#else
    
    if (max(extra.x, extra.y) == 0.0) {
        PTn = PT;
        gn = g_dst;
        return true;
    } else if (min(extra.x, extra.y) == 0.0 && esum < N) {
        // Magic stuff happens here.
        vec3 pos = PT * vec3(g_dst_clamp/N*2.0-1.0, 1.0 - 2.0*esum/N);
        PTn = getPT(pos);
        gn = ((pos * PTn).xy*0.5 + 0.5) * N;
        return true;	        
    } else {
        return false;
    }
    
#endif

}

/* From https://www.shadertoy.com/view/Xd23Dh */
vec3 hash3( vec2 p )
{
    vec3 q = vec3( dot(p,vec2(127.1,311.7)), 
                  dot(p,vec2(269.5,183.3)), 
                  dot(p,vec2(419.2,371.9)) );
    return fract(sin(q)*43758.5453);
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

/* Color the sphere/cube points. */
vec3 gcolor(vec3 pos) {

    mat3 PT;
    vec2 g;

    // Get grid coords
    posToGrid(pos, PT, g);
    
    // Snap to cube face - note only needed for visualization.
    pos /= dot(pos, PT[2]);

    const float farval = 1e5;
    
    // Distances/colors/points for Voronoi
    float d1 = farval;
    float d2 = farval;

    float m1 = -1.0;
    float m2 = -1.0;

    vec3 p1 = vec3(0);
    vec3 p2 = vec3(0);

	// For drawing grid lines below
    vec2 l = abs(fract(g+0.5)-0.5);

    // Move to center of grid cell for neighbor calculation below.
    g = floor(g) + 0.5;

    // For each potential neighbor
    for (float u=-1.0; u<=1.0; ++u) {
        for (float v=-1.0; v<=1.0; ++v) {
            
            vec2 gn;
            mat3 PTn;

            // If neighbor exists
            if (gridNeighbor(PT, g, vec2(u,v), PTn, gn)) {
                
                float face = dot(PTn[2], vec3(1.,2.,3.));
                
                // Perturb based on grid cell ID
                gn = floor(gn);
                vec3 rn = hash3(gn*0.123 + face);
                //gn += 0.5 + (rn.xy * 2.0 - 1.0)*1.0*0.5;
				//gn += vec2(0.5);
				//gn += hex(vec2 p, float width, float coreSize)

                // Get the 3D position
                vec3 pos_n = gridToPos(PTn, gn);
                
                // Compute squared distance on sphere
                float dp = sphereDist2(pos, pos_n);
                
                // See if new closest point (or second closest)
                if (dp < d1) {
                    d2 = d1; p2 = p1;
                    d1 = dp; p1 = pos_n;
                } else if (dp < d2) {
                    d2 = dp; p2 = pos_n;
                }
                
            }
        }
    }

    vec3 c = vec3(1.0);

    // voronoi lines    
    c = mix(c, vec3(0.0),
            smoothstep(0.005, 0.001, bisectorDistance(pos, p2, p1)));

    // goodbye
    return c;

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
			vec3 color = 1.0 - gcolor(normal);
			gl_FragColor = vec4(color, 0.5);
		}
	]],
		uniformInt = {
			tex0 = 0,
		},
		definitions = {
			string.gsub("#define MAX_POINTS _PTS_NUM_ \n", "_PTS_NUM_", tostring(MAX_POINTS)),
		},
	})

	local shLog = gl.GetShaderLog()
	if (shieldShader == nil or string.len(shLog or "") > 0) then
		print(PRIO_MAJOR, "LUPS->Shield: shader warnings & errors: "..shLog)
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