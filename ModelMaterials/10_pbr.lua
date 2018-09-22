local normalDraw = 1
local shadowDraw = 2
local reflectionDraw = 3
local refractionDraw = 4
local gameDeferredDraw = 5

local function DrawUnit(unitID, material, drawMode)
	if drawMode == normalDraw and material.customStandardUniforms then
		local curShader = material.standardShader
		for _, uniformData in pairs(material.customStandardUniforms) do
			if not uniformData.location then
				uniformData.location = gl.GetUniformLocation(curShader, uniformData.name)
			end
			local valType = type(uniformData.value)
			if valType == "table" then
				gl.Uniform(uniformData.location, unpack(uniformData.value))
			elseif valType == "number" then
				gl.Uniform(uniformData.location, uniformData.value)
			else
				Spring.Echo("Wrong 10_pbr shader uniform type for uniform " .. uniformData.name)
			end
		end
		local gf = Spring.GetGameFrame()
		gl.UniformInt(gl.GetUniformLocation(curShader, "simFrame"), gf)
	end
end

function myHash(str)
	local hash = 0

	local rem = #str % 3
	if rem > 0 then
		str = str .. string.rep(" ", rem)
	end

	for i = 1, #str / 3 do
		local idx = (i - 1) * 3 + 1
		local b1, b2, b3 = string.byte(str, idx, idx + 2)
		hash = math.bit_xor(hash, b1, b2 * 256, b3 * 65536)
	end

	return hash
end

local function getNumberOfChannels(val)
	if not val then
		return nil
	end
	local texUnitNum = string.match(val, "%[(%d-)%]")
	if texUnitNum then
		local texChannel = string.match(val, "%.(%a+)")
		if texChannel then
			return #texChannel
		end
	end
	return nil
end

local pbrMaterialValues = {
	["flipUV"] = function(pbr) return ((pbr.flipUV == nil) and true) or pbr.flipUV end,
	["fastGamma"] = function(pbr) return ((pbr.fastGamma == nil) and false) or pbr.fastGamma end,
	["pbrWorkflow"] = function(pbr) return pbr.pbrWorkflow or "metallic" end,
	["tbnReortho"] = function(pbr) return ((pbr.tbnReortho == nil) and true) or pbr.tbnReortho end,

	["baseColorMap.scale"] = function(pbr) return (pbr.baseColorMap or {}).scale or {1.0, 1.0, 1.0, 1.0} end,
	["baseColorMap.get"] = function(pbr) return (pbr.baseColorMap or {}).get or nil end,
	["baseColorMap.gammaCorrection"] = function(pbr) return ((pbr.baseColorMap or {}).gammaCorrection == nil and true) or pbr.baseColorMap.gammaCorrection end,

	["normalMap.scale"] = function(pbr) return (pbr.normalMap or {}).scale or {1.0, 1.0, 1.0} end,
	["normalMap.get"] = function(pbr) return (pbr.normalMap or {}).get or nil end,
	["normalMap.gammaCorrection"] = function(pbr) return ((pbr.normalMap or {}).gammaCorrection == nil and false) or pbr.normalMap.gammaCorrection end,
	["normalMap.hasTangents"] = function(pbr) return ((pbr.normalMap or {}).hasTangents == nil and false) or pbr.normalMap.hasTangents end,

	["parallaxMap.fast"] = function(pbr) return ((pbr.parallaxMap or {}).fast == nil and true) or pbr.parallaxMap.fast end,
	["parallaxMap.perspective"] = function(pbr) return ((pbr.parallaxMap or {}).perspective == nil and false) or pbr.parallaxMap.perspective end,
	["parallaxMap.limits"] = function(pbr) return (pbr.parallaxMap or {}).limits or nil end,
	["parallaxMap.scale"] = function(pbr) return (pbr.parallaxMap or {}).scale or 0.01 end,
	["parallaxMap.get"] = function(pbr) return (pbr.parallaxMap or {}).get or nil end,
	["parallaxMap.gammaCorrection"] = function(pbr) return ((pbr.parallaxMap or {}).gammaCorrection == nil and false) or pbr.parallaxMap.gammaCorrection end,

	["emissiveMap.scale"] = function(pbr)
			if (pbr.emissiveMap or {}).scale then
				return (pbr.emissiveMap or {}).scale
			end
			local emMapGet = (pbr.emissiveMap or {}).get
			if emMapGet then
				local nCh = getNumberOfChannels(emMapGet)
				if nCh == 1 then
					return 1.0
				elseif nCh == 3 then
					return {1.0, 1.0, 1.0}
				end
			end
			return nil
		end,
	["emissiveMap.get"] = function(pbr) return (pbr.emissiveMap or {}).get or nil end,
	["emissiveMap.gammaCorrection"] = function(pbr)
			if (pbr.emissiveMap or {}).gammaCorrection then
				return (pbr.emissiveMap or {}).gammaCorrection
			end
			local emMapGet = (pbr.emissiveMap or {}).get
			if emMapGet then
				local nCh = getNumberOfChannels(emMapGet)
				if nCh == 1 then
					return false -- multiplier workflow, no need for gamma correction
				elseif nCh == 3 then
					return true -- color workflow, needs gamma correction
				end
			end
			return nil
		end,

	["occlusionMap.scale"] = function(pbr) return (pbr.occlusionMap or {}).scale or 1.0 end,
	["occlusionMap.get"] = function(pbr) return (pbr.occlusionMap or {}).get or nil end,
	["occlusionMap.gammaCorrection"] = function(pbr) return ((pbr.occlusionMap or {}).gammaCorrection == nil and false) or pbr.occlusionMap.gammaCorrection end,

	["roughnessMap.scale"] = function(pbr) return (pbr.roughnessMap or {}).scale or 1.0 end,
	["roughnessMap.get"] = function(pbr) return (pbr.roughnessMap or {}).get or nil end,
	["roughnessMap.gammaCorrection"] = function(pbr) return ((pbr.roughnessMap or {}).gammaCorrection == nil and false) or pbr.roughnessMap.gammaCorrection end,

	["metallicMap.scale"] = function(pbr) return (pbr.metallicMap or {}).scale or 1.0 end,
	["metallicMap.get"] = function(pbr) return (pbr.metallicMap or {}).get or nil end,
	["metallicMap.gammaCorrection"] = function(pbr) return ((pbr.metallicMap or {}).gammaCorrection == nil and false) or pbr.metallicMap.gammaCorrection end,

	["iblMap.invToneMapExp"] = function(pbr) return (pbr.iblMap or {}).invToneMapExp or nil end,
	["iblMap.scale"] = function(pbr) return (pbr.iblMap or {}).scale or 1.0 end,
	["iblMap.get"] = function(pbr) return (pbr.iblMap or {}).get or nil end,
	["iblMap.lod"] = function(pbr) return ((pbr.iblMap or {}).lod == nil and false) or pbr.iblMap.lod end,
	["iblMap.gammaCorrection"] = function(pbr) return ((pbr.iblMap or {}).gammaCorrection == nil and true) or pbr.iblMap.gammaCorrection end,

	["exposure"] = function(pbr) return pbr.exposure or 1.0 end,
	["toneMapping"] = function(pbr) return pbr.toneMapping or nil end,
	["gammaCorrection"] = function(pbr) return ((pbr.gammaCorrection == nil) and true) or pbr.gammaCorrection end,

	["texUnits"] = function(pbr) return pbr.texUnits or nil end,
}

local pbrDebug = { -- Debug output. Will replace output color if enabled
	baseColor = false,
	worldNormals = false,
	viewNormals = false,
	tangentNormals = false,
	parallaxShift = false,
	diffuseColor = false,
	specularColor = false,
	specularAndDiffuseColor = false,
	emissionColor = false,
	teamColor = false,
	occlusion = false,
	roughness = false,
	metallic = false,
	reflectionDir = false,
	reflectionLength = false,
	specWorldReflection = false,
	specViewReflection = false,
	diffuseWorldReflection = false,
	iblSpecularColor = false,
	iblDiffuseColor = false,
	iblSpecularAndDiffuseColor = false,
	shadowCoeff1 = false,
	shadowCoeff2 = false,
	shadow = false,
	preExpColor = false,
	tmColor = false,
}

local unitMaterials = {}
local materials = {}

local function camelToUnderline(str)
	local upperIdx = string.find(str, "%u")
	local underString = string.sub(str, 1, upperIdx - 1) .. "_" .. string.sub(str, upperIdx)
	return string.upper(underString)
end

local function parseNewMatTexUnits(pbr)
	local boundTexUnits = {}
	local texUnitDefs = pbrMaterialValues["texUnits"](pbr)

	for tu, fileSpec in pairs(texUnitDefs) do
		local s, e = string.find(fileSpec, ":.-:")
		local texOpt = ""
		if s and e then
			texOpt = string.sub(fileSpec, s, e)
		end
		local fileName = string.gsub(fileSpec, ":.-:", "")
		local newFilePath = "unittextures/" .. fileName
		if VFS.FileExists(newFilePath) then
			boundTexUnits[tu] = texOpt .. newFilePath --keep :{opts}:
		end
	end

	return boundTexUnits
end

function tableConcat(dest, source)
    for i, data in ipairs(source) do
        table.insert(dest, data)
    end
    return dest
end

local function parsePbrMatParams(pbr)

	local shaderDefinitions = {
		"#version 150 compatibility",
		"#define deferred_mode 0",
	}
	local customStandardUniforms = {}

	local deferredDefinitions = {
		"#version 150 compatibility",
		"#define deferred_mode 1",
	}
	local customDefferedUniforms = {}

	for key, valFunc in pairs(pbrMaterialValues) do
		local val = valFunc(pbr)
		local valType = type(val)
		local pntIdx = string.find(key, "%.")

		local define = {}
		local uniform = {}

		if pntIdx then
			local first  = string.sub(key, 1, pntIdx - 1)
			local second = string.sub(key, pntIdx + 1)
			Spring.Echo(key, first, second, val)
			if first == "parallaxMap" and second == "get" and val then
				local texUnitNum = string.match(val, "%[(%d-)%]")
				local texChannel = string.match(val, "%.(%a)")
				table.insert(define, "#define GET_" .. string.upper(first) .. string.format(" texture(tex%d, texCoord).%s", texUnitNum, texChannel))
			elseif first == "parallaxMap" and second == "limits" and val and valType == "table" then
				table.insert(define, "#define PARALLAXMAP_LIMITS PARALLAXMAP_LIMITS_MANUAL")
			elseif first == "parallaxMap" and second == "limits" and val and valType == "boolean" then
				table.insert(define, "#define PARALLAXMAP_LIMITS PARALLAXMAP_LIMITS_AUTO")
			else
				if second == "get" and val then
					if first == "emissiveMap" then
						local numCh = getNumberOfChannels(pbrMaterialValues["emissiveMap.get"](pbr))
						if numCh and numCh == 1 then
							table.insert(define, "#define EMISSIVEMAP_TYPE EMISSIVEMAP_TYPE_MULT")
							table.insert(define, "#define GET_" .. string.upper(first) .. " texels" .. val)
						elseif numCh and numCh > 1 then
							table.insert(define, "#define EMISSIVEMAP_TYPE EMISSIVEMAP_TYPE_VAL")
							table.insert(define, "#define GET_" .. string.upper(first) .. " texels" .. val)
						end
					elseif valType == "string" then
						table.insert(define, "#define GET_" .. string.upper(first) .. " texels" .. val)
					elseif valType == "boolean" then
						table.insert(define, "#define GET_" .. string.upper(first))
					end
				elseif second == "gammaCorrection" and val then
					table.insert(define, "#define SRGB_" .. string.upper(first))
				elseif second == "hasTangents" and val then
					table.insert(define, "#define " .. camelToUnderline(second))
				elseif second == "invToneMapExp" and valType == "number" then
					table.insert(define, "#define IBL_INVTONEMAP")
				elseif second == "lod" and val then
					if valType == "boolean" then
						table.insert(define, "#define IBL_TEX_LOD IBL_TEX_LOD_AUTO") --automatic definition of max LOD
					else
						table.insert(define, "#define IBL_TEX_LOD IBL_TEX_LOD_MANUAL") --manual definition of max LOD
					end
				elseif second == "fast" and val then
					table.insert(define, "#define " .. "PARALLAXMAP_FAST")
				elseif second == "perspective" and val then
					table.insert(define, "#define " .. "PARALLAXMAP_PERSPECTIVE")
				end
			end

			if (second == "scale" or second == "strength") and val then
				table.insert(uniform, {
					name = first .. second:gsub("^%l", string.upper),
					value = val,
					location = nil,
				})
			elseif second == "lod" and valType == "number" then
				table.insert(uniform, {
					name = first .. second:gsub("%l", string.upper),
					value = val,
					location = nil,
				})
			elseif second == "limits" and val and valType == "table" then
				table.insert(uniform, {
					name = first .. second:gsub("^%l", string.upper),
					value = val,
					location = nil,
				})
			elseif second == "invToneMapExp" and valType == "number" then
				Spring.Echo("invToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExpinvToneMapExp", first .. second:gsub("^%l", string.upper), val)
				table.insert(uniform, {
					name = first .. second:gsub("^%l", string.upper),
					value = val,
					location = nil,
				})
			end
		else
			if key == "texUnits" then
				for tu, fileSpec in pairs(val) do
					local fileName = string.gsub(fileSpec, ":.-:", "")
					local newFilePath = "unittextures/" .. fileName
					if VFS.FileExists(newFilePath) then
						table.insert(shaderDefinitions, "#define HAS_" .. tu)
					end
				end
			elseif valType == "number" then
				table.insert(uniform, {
					name = key,
					value = val,
					location = nil,
				})
			else
				if valType == "boolean" and val then
					table.insert(define, "#define " .. camelToUnderline(key))
				elseif valType == "string" then
					table.insert(define, string.format("#define %s %s_%s", string.upper(key), string.upper(key), string.upper(val)))
				end
			end
		end
		if #define > 0 then
			tableConcat(shaderDefinitions, define)
			--Spring.Echo(key, val, "define", define)
		end

		if #uniform > 0 then
			tableConcat(customStandardUniforms, uniform)
			Spring.Echo(key, val, "uniform", uniform.name, uniform.value)
		end
	end

	local debugIdx = 1
	for k, v in pairs(pbrDebug) do

		local def = string.format("#define DEBUG_%s %d", string.upper(k), debugIdx)
		table.insert(shaderDefinitions, def)
		table.insert(deferredDefinitions, def)
		debugIdx = debugIdx + 1

		if v then
			local def = string.format("#define DEBUG DEBUG_%s", string.upper(k))
			table.insert(shaderDefinitions, def)
			table.insert(deferredDefinitions, def)
		end

	end

	return shaderDefinitions, deferredDefinitions, customStandardUniforms, customDefferedUniforms
end

-- Take only non-uniform parameters
local function getPbrMaterialIndex(pbr)
	local propString = ""

	local shaderDefinitions, deferredDefinitions = parsePbrMatParams(pbr)

	propString = propString .. "\nshaderDefinitions:\n"
	propString = propString .. table.concat(shaderDefinitions, "\n")
	propString = propString .. "\ndeferredDefinitions:\n"
	propString = propString .. table.concat(deferredDefinitions, "\n")

	local hashValue = myHash(propString)

	Spring.Echo(propString, hashValue)

	return hashValue
end

local MAPSIDE_MAPINFO = "mapinfo.lua"
local mapInfo = VFS.FileExists(MAPSIDE_MAPINFO) and VFS.Include(MAPSIDE_MAPINFO) or false

local irradianceMap, specularMap
local function getScenePBRParams()
	local pbrMap = (mapInfo.custom or {}).pbr
	if pbrMap then

	end
end

local function createNewMatDef(pbr)
	local shaderDefinitions, deferredDefinitions, customStandardUniforms, customDefferedUniforms = parsePbrMatParams(pbr)

	local newMat = {
		shaderDefinitions = shaderDefinitions,
		--deferredDefinitions = deferredDefinitions,
		shader    = include("ModelMaterials/Shaders/pbr.lua"),
		usecamera = false,
		culling   = GL.BACK,
		predl  = nil,
		postdl = nil,
		force = true,
		texunits = {
			[0] = "%TEX0",
			[1] = "%TEX1",
			[2] = "%TEX2",
			[3] = "%TEX3",
			--[4] = "%TEX4",
			[5] = "%BRDF",
			[6] = "$shadow",
			[7] = "$reflection", --TODO replace with radiance map!!!
			[8] = "$reflection",
		},
		DrawUnit = DrawUnit,
		--UnitCreated = UnitCreated,
		--UnitDestroyed = UnitDestroyed,

		customStandardUniforms = customStandardUniforms,
		customDefferedUniforms = customDefferedUniforms,
	}
	return newMat
end

for i = 1, #UnitDefs do
	local udef = UnitDefs[i]
	local modelFilename = string.format("Objects3d/%s.lua", udef.modelname)
	if VFS.FileExists(modelFilename) then
		local model = VFS.Include(modelFilename)
		if model and model.pbr then
			local pbr = model.pbr
			local pbrIndex = getPbrMaterialIndex(pbr)
			local pbrMatName = "pbr" .. tostring(pbrIndex)
			if not materials[pbrMatName] then
				local pbrMatDef = createNewMatDef(pbr)
				materials[pbrMatName] = pbrMatDef
			end
			local boundTexUnits = parseNewMatTexUnits(pbr)
			local matDef = {pbrMatName}
			for tkey, tval in pairs(boundTexUnits) do
				matDef[tkey] = tval
			end
			unitMaterials[i] = matDef
		end
	end
end

return materials, unitMaterials