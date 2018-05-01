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

		void main()
		{
			gl_TexCoord[0] = gl_MultiTexCoord0;

			float r = length(gl_Vertex.xyz);
			float theta = acos(gl_Vertex.z / r);
			float phi = atan(gl_Vertex.y, gl_Vertex.x);

			r += 0 * r * nsin(theta + phi + timer * 0);

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

		#define HEXSCALE 50.0

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


		float rand(float n){
			return fract(sin(n) * 43758.5453123);
		}

		float rand(vec2 n) { 
			return fract(sin(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
		}

		float noise(float p){
			float fl = floor(p);
			float fc = fract(p);
			return mix(rand(fl), rand(fl + 1.0), fc);
		}
			
		float noise(vec2 n) {
			const vec2 d = vec2(0.0, 1.0);
			vec2 b = floor(n), f = smoothstep(vec2(0.0), vec2(1.0), fract(n));
			return mix(mix(rand(b), rand(b + d.yx), f.x), mix(rand(b + d.xy), rand(b + d.yy), f.x), f.y);
		}

		#define VOROPACE 5.0

		vec3 voronoi( in vec2 x ) {
			vec2 n = floor(x);
			vec2 f = fract(x);

			// first pass: regular voronoi
			vec2 mg, mr;
			float md = 8.0;
			for (int j= -1; j <= 1; j++) {
				for (int i= -1; i <= 1; i++) {
					vec2 g = vec2(float(i),float(j));
					vec2 o = rand( n + g );
					o = 0.5 + 0.5*sin( timer*VOROPACE + 6.2831*o );

					vec2 r = g + o - f;
					float d = dot(r,r);

					if( d<md ) {
						md = d;
						mr = r;
						mg = g;
					}
				}
			}

			// second pass: distance to borders
			md = 8.0;
			for (int j= -2; j <= 2; j++) {
				for (int i= -2; i <= 2; i++) {
					vec2 g = mg + vec2(float(i),float(j));
					vec2 o = rand( n + g );
					o = 0.5 + 0.5*sin( timer*VOROPACE + 6.2831*o );

					vec2 r = g + o - f;

					if ( dot(mr-r,mr-r)>0.00001 ) {
						md = min(md, dot( 0.5*(mr+r), normalize(r-mr) ));
					}
				}
			}
			return vec3(md, mr);
		}

		void main(void)
		{
			vec3 n = normalize(normal);
			vec2 uv = RadialCoords(n) * (1.0, 0.5);
			// Scale
			uv *= 50.;
			vec3 c = voronoi(uv);
			
			vec4 texel = vec4(0.0);
			texel = mix( vec4(1.0), texel, smoothstep( 0.01, 0.1, c.x ) );

			vec4 color1Tex = mix(color1, texel, colorMix);
			float colorMultAdj = colorMult;
			vec4 color2M = color2 * colorMultAdj;
			vec4 color1TexM = color1Tex * colorMultAdj;

			gl_FragColor = mix(color1TexM, color2M, opac);
			
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