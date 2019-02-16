-- $Id: ShieldSphereColorHQ.lua 3171 2008-11-06 09:06:29Z det $
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local ShieldSphereColorHQParticle = {}
ShieldSphereColorHQParticle.__index = ShieldSphereColorHQParticle

local sphereList = {}

local LuaShader = VFS.Include("LuaRules/Gadgets/Include/LuaShader.lua")


-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle.GetInfo()
	return {
		name		= "ShieldSphereColorHQ",
		backup		= "ShieldSphereColor", --// backup class, if this class doesn't work (old cards,ati's,etc.)
		desc		= "",

		layer		= -23, --// extreme simply z-ordering :x

		--// gfx requirement
		fbo			= true,
		shader		= true,
		rtt			= false,
		ctt			= true,
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

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local GL_RGBA16F = 0x881A
local GL_RGBA32F = 0x8814
local texAFormats = {
	GL_RGBA32F,
	GL_RGBA16F,
}

--local GL_R8 = 0x8229
local GL_R16F = 0x822D
local GL_R32F = 0x822E
local texBFormats = {
	GL_R32F,
	GL_R16F,
}

local GL_TEXTURE_2D_MULTISAMPLE = 0x9100
local GL_TEXTURE_2D = 0x0DE1

local GL_DEPTH_COMPONENT16 = 0x81A5
local GL_DEPTH_COMPONENT32 = 0x81A7
local GL_DEPTH_COMPONENT24 = 0x81A6
local depthTexFormats = {
	GL_DEPTH_COMPONENT32,
	(Platform.glSupport24bitDepthBuffer and GL_DEPTH_COMPONENT24) or GL_DEPTH_COMPONENT16,
}

local GL_COLOR_ATTACHMENT0_EXT = 0x8CE0
local GL_COLOR_ATTACHMENT1_EXT = 0x8CE1

local GL_FUNC_ADD = 0x8006
local GL_MIN = 0x8007
local GL_MAX = 0x8008

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local ShieldDrawer = setmetatable ({},{
	__call = function(self, options)
		local opt = options or {}
		return setmetatable(
		{
			betterPrecision = (opt.betterPrecision == nil and false) or opt.betterPrecision,
			msaaLevel = opt.msaaLevel or 1,
			doOIT = (opt.doOIT == nil and true) or opt.doOIT,

			vpx = nil,
			vpy = nil,
			vsx = nil,
			vsy = nil,

			shaderFile = nil,

			opaqueDepthTex = nil,
			texA = nil,
			texB = nil,

			oitFBO = nil,

			oitFillShader = nil,
			blitShader = nil,

			effectsList = {},
			maxEffectIndex = 3, -- ^^^^

			geometryLists = {},
		}, self)
	end })

ShieldDrawer.__index = ShieldDrawer

function ShieldDrawer:Initialize()
	--self.vsx, self.vsy = gl.GetViewSizes()
	self.vsx, self.vsy, self.vpx, self.vpy = Spring.Orig.GetViewGeometry()
	local vsx, vsy = self.vsx, self.vsy

	self.shaderFile = VFS.Include("lups/ParticleClasses/ShieldSphereColorHQ.glsl")
	if not self.shaderFile then
		Spring.Echo("Error!")
	end

	self.shaderFile.blitShaderFragment =
	self.shaderFile.blitShaderFragment:gsub("###MSAA_LEVEL###", tostring(self.msaaLevel))
	self.shaderFile.blitShaderFragment =
	self.shaderFile.blitShaderFragment:gsub("###DO_OIT###", "1")

	
	self.shaderFile.oitFillShaderFragment =
	self.shaderFile.oitFillShaderFragment:gsub("###MSAA_LEVEL###", tostring(self.msaaLevel))
	self.shaderFile.oitFillShaderFragment =
	self.shaderFile.oitFillShaderFragment:gsub("###DO_OIT###", "1")

	local commonTexOpts = {
		target = ((self.msaaLevel > 1) and GL_TEXTURE_2D_MULTISAMPLE) or GL_TEXTURE_2D,
		samples = ((self.msaaLevel > 1) and self.msaaLevel) or nil,

		border = false,
		--min_filter = GL.LINEAR,
		--mag_filter = GL.LINEAR,
		min_filter = GL.NEAREST,
		mag_filter = GL.NEAREST,
		wrap_s = GL.CLAMP_TO_EDGE,
		wrap_t = GL.CLAMP_TO_EDGE,
	}

	local fmtStartIdx = (self.betterPrecision and 1) or 2

	for fmtIdx = fmtStartIdx, 2 do
		local fmt = depthTexFormats[fmtIdx]
		commonTexOpts.format = fmt
		self.opaqueDepthTex = gl.CreateTexture(vsx, vsy, commonTexOpts)
		if self.opaqueDepthTex then
			break
		end
	end
	if not self.opaqueDepthTex then
		Spring.Echo("Error3!")
	end

	for fmtIdx = fmtStartIdx, 2 do
		local fmt = texAFormats[fmtIdx]
		commonTexOpts.format = fmt
		self.texA = gl.CreateTexture(vsx, vsy, commonTexOpts)
		if self.texA then
			break
		end
	end
	if not self.texA then
		Spring.Echo("Error4!")
	end

	for fmtIdx = fmtStartIdx, 2 do
		local fmt = texBFormats[fmtIdx]
		commonTexOpts.format = fmt
		self.texB = gl.CreateTexture(vsx, vsy, commonTexOpts)
		if self.texB then
			break
		end
	end
	if not self.texB then
		Spring.Echo("Error5!")
	end

	self.oitFBO = gl.CreateFBO({
		depth = self.opaqueDepthTex,
		color0 = self.texA,
		color1 = self.texB,
		drawbuffers = {GL_COLOR_ATTACHMENT0_EXT, GL_COLOR_ATTACHMENT1_EXT},
	})


	if not gl.IsValidFBO(self.oitFBO) then
		Spring.Echo("Error2!")
	end

	self.oitFillShader = LuaShader({
		vertex = self.shaderFile.oitFillShaderVertex,
		fragment = self.shaderFile.oitFillShaderFragment,
		uniformInt = {
			depthTex = 30,
		},		
	}, "ShieldHQ/3D Transparency Pass")
	self.oitFillShader:Initialize()

	self.blitShader = LuaShader({
		vertex = self.shaderFile.blitShaderVertex,
		fragment = self.shaderFile.blitShaderFragment,
		--[[uniformFloat = {
			screenSize = {vsx, vsy},
		},]]--
		uniformInt = {
			texA = 30,
			texB = 31,
		},
	}, "ShieldHQ/2D Compositing Pass")
	self.blitShader:Initialize()

	self.geometryLists = {
		huge = gl.CreateList(DrawSphere, 0, 0, 0, 1, 140),
		large = gl.CreateList(DrawSphere, 0, 0, 0, 1, 60),
		medium = gl.CreateList(DrawSphere, 0, 0, 0, 1, 50),
		small = gl.CreateList(DrawSphere, 0, 0, 0, 1, 40),
	}
end

function ShieldDrawer:ViewResize()
	self:Finalize()
	self:Initialize()
end

-- http://casual-effects.blogspot.com/2014/03/weighted-blended-order-independent.html
function ShieldDrawer:BeginRenderPass()
	--copy depth texture from default FBO
--	if self.msaaLevel == 1 then
		--gl.CopyToTexture(self.opaqueDepthTex, 0, 0, self.vpx, self.vpy, self.vsx, self.vsy)
--	else
		--[[gl.BlitFBO(
			nil, 			0, 0, self.vsx, self.vsy, -- fboSrc , int x0Src,y0Src,x1Src,y1Src,
			self.oitFBO, 	0, 0, self.vsx, self.vsy, -- fboDst , int x0Dst,y0Dst,x1Dst,y1Dst
			GL.DEPTH_BUFFER_BIT, GL.NEAREST -- [, number mask = GL_COLOR_BUFFER_BIT [, number filter = GL_NEAREST ] ]
		)]]--
	--end

	gl.ActiveFBO(self.oitFBO, function()
		gl.DepthTest(true)
		--gl.DepthTest(false)
		--gl.DepthMask(false)
		gl.DepthMask(true)
		gl.Clear(GL.DEPTH_BUFFER_BIT)
		
		gl.Clear(GL.COLOR_BUFFER_BIT, 0, 0, 0, 1)
		gl.Blending(true)
		gl.BlendFuncSeparate(GL.ONE, GL.ONE, GL.ZERO, GL.ONE_MINUS_SRC_ALPHA)
		--self.oitFillShader:Activate()
	end)

	self.oitFillShader:Activate()

	local gf = Spring.GetGameFrame()
	self.oitFillShader:SetUniformFloatAlways("gameFrame", gf)

	local near, far = gl.GetViewRange(0) -- CAMTYPE_PLAYER = 0
	self.oitFillShader:SetUniformFloat("depthRangeSpring", near, far)

	self.oitFillShader:SetUniformMatrix("viewMat", "view")
	self.oitFillShader:SetUniformMatrix("projMat", "projection")

	--gl.UnsafeSetFBO(self.oitFBO)
	self.effectsList = {
		[0] = {}, -- small
		[1] = {}, -- medium
		[2] = {}, -- large
		[3] = {}, -- huge
	}
end

function ShieldDrawer:DoRenderPass(info)
	local effectIndex = 0 --TODO
	table.insert(self.effectsList[effectIndex], info)
end

local debug = true
function ShieldDrawer:EndRenderPass()

	gl.ActiveFBO(self.oitFBO, function()
		--from smaller shields to larger (kinda back to front in stacked shields environment)
		for effectIndex = 0, self.maxEffectIndex do

			if #self.effectsList[effectIndex] > 0 then
				self.oitFillShader:SetUniformInt("effectIndex", effectIndex)
				for _, info in pairs(self.effectsList[effectIndex]) do
					local posx, posy, posz = Spring.GetUnitPosition(info.unitID)
					posx, posy, posz = posx + info.pos[1], posy + info.pos[2], posz + info.pos[3]

					local pitch, yaw, roll = Spring.GetUnitRotation(info.unitID)

					self.oitFillShader:SetUniformFloat("translationScale", posx, posy, posz, info.size)
					self.oitFillShader:SetUniformFloat("rotPYR", pitch, yaw, roll)

					self.oitFillShader:SetUniformInt("effectIndex", 0)

					gl.CallList(self.geometryLists[info.shieldSize])
				end
			end

		end
	end)

	self.oitFillShader:Deactivate()

	if debug then
		gl.PushPopMatrix(function()
			gl.MatrixMode(GL.PROJECTION); gl.LoadIdentity();
			gl.MatrixMode(GL.MODELVIEW); gl.LoadIdentity();
			gl.ActiveFBO(self.oitFBO, function()
				gl.SaveImage( 0, 0, self.vsx, self.vsy, string.format("texA_%s.png", select(1, Spring.GetGameFrame())) )
			end)
		end)
		debug = false
	end

	gl.BlendEquationSeparate(GL_FUNC_ADD, GL_MIN)
	--gl.BlendEquationSeparate(GL_FUNC_ADD, GL_MAX)

	-- As per revised article it should be: SRC_ALPHA, ONE_MINUS_SRC_ALPHA
	gl.BlendFunc(GL.SRC_ALPHA, GL.ONE_MINUS_SRC_ALPHA) --alpha NO pre-multiply
	--gl.BlendFunc(GL.ONE, GL.ONE_MINUS_SRC_ALPHA) --alpha pre-multiply

	gl.PushPopMatrix(function()
		gl.MatrixMode(GL.PROJECTION); gl.LoadIdentity();
		gl.MatrixMode(GL.MODELVIEW); gl.LoadIdentity();

		self.blitShader:ActivateWith( function ()
			gl.Texture(30, self.texA)
			gl.Texture(31, self.texB)

			gl.TexRect(-1, -1, 1, 1)
		end)
	end)

	gl.BlendEquation(GL_FUNC_ADD)

end


function ShieldDrawer:Finalize()
	self.vsx, self.vsy = nil, nil

	gl.DeleteTexture(self.opaqueDepthTex)
	gl.DeleteTexture(self.texA)
	gl.DeleteTexture(self.texB)

	gl.DeleteFBO(self.oitFBO)

	self.oitFillShader:Finalize()
	self.blitShader:Finalize()

	for _, list in pairs(self.geometryLists) do
		gl.DeleteList(list)
	end
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

local shieldDrawer

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle:Visible()
	return self.visibleToMyAllyTeam
end

function ShieldSphereColorHQParticle:BeginDraw()
	shieldDrawer:BeginRenderPass()
end

function ShieldSphereColorHQParticle:EndDraw()
	shieldDrawer:EndRenderPass()
end

function ShieldSphereColorHQParticle:Draw()
	shieldDrawer:DoRenderPass(self)
end

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

function ShieldSphereColorHQParticle:Initialize()
	local newEngine = Script.IsEngineMinVersion(104, 0, 1000) --TODO figure out commit number
	local opt = {
		betterPrecision = false,
		--msaaLevel = (newEngine and Spring.GetConfigInt("MSAALevel", 1)) or 1,
		msaaLevel = 1,
	}
	shieldDrawer = shieldDrawer or ShieldDrawer(opt)
	shieldDrawer:Initialize()
end

function ShieldSphereColorHQParticle:Finalize()
	shieldDrawer:Finalize()
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