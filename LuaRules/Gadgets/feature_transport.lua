function gadget:GetInfo()
   return {
      name      = "Transportable features",      
      author    = "ivand",
      date      = "2017",
      license   = "GNU GPL, v2 or later",
      layer     = 0,
      enabled   = true,
   }
end


------------------------------------------------------
--DEBUG
------------------------------------------------------
function ePrint (tbl, indent)
	if not indent then indent = 0 end
	for k, v in pairs(tbl) do
		formatting = string.rep(" ", indent) .. k .. ": "
		
		if type(v) == "table" then
			Spring.Echo(formatting)
			ePrint(v, indent+1)
		else
			if type(v) == "boolean" or type(v) == "function" then			
				Spring.Echo(formatting .. tostring(v))
			else
				Spring.Echo(formatting .. v)
			end
		end
	end
end

function ePrintEx (val, indent)
	if val==nil then Spring.Echo("nil")
	else
		if type(val) == "table" then ePrint (val, indent)			
		else
			Spring.Echo(tostring(val))
		end
	end
end

function ePrintCMDQueue(commandQueueTable)
	for _, cmd in pairs(commandQueueTable) do
		ePrintCMD(cmd)
	end
end

function ePrintCMD(cmd)
	if cmd.id>=0 then
		Spring.Echo( "id: "..(CMD[cmd.id] or tostring(cmd.id)) )
	else
		Spring.Echo( "id: BUILD "..UnitDefs[-cmd.id].name )
	end	
	if cmd.tag then Spring.Echo("tag: "..cmd.tag) end
	ePrintEx({params=cmd.params})
	ePrintEx({options=cmd.options})
end

function ePrintUnitDef(unitDef)
	for name,param in unitDef:pairs() do
		ePrintEx({name=name, param=param})
	end
end

function ePrintWeaponDef(weaponDef)
	for name,param in weaponDef:pairs() do
		ePrintEx({name=name, param=param})
	end
end

------------------------------------------------------
--/DEBUG
------------------------------------------------------


--SYNCED
if (gadgetHandler:IsSyncedCode()) then

------------------------------------------------------
------------------------------------------------------
--local fakeunitID = UnitDefNames["fakeunit_los"].id
local fakeunitID = UnitDefNames["featureunit2"].id
local gaiaTeamID = Spring.GetGaiaTeamID()

local featuresInfo = nil
local transportedFakes = nil
local featuresByunitID = nil
------------------------------------------------------
------------------------------------------------------


function gadget:Initialize()
	featuresInfo = {}
	transportedFakes = {}
	featuresByunitID = {}
	local features = Spring.GetAllFeatures()
	for _, featureID in pairs(features) do
		gadget:FeatureCreated(featureID)
	end
end

function gadget:Shutdown()
	local features = Spring.GetAllFeatures()
	for _, featureID in pairs(features) do
		gadget:FeatureDestroyed(featureID)
	end
	featuresInfo = nil
	transportedFakes = nil
	featuresByunitID = nil
end


local SIZE_MULT = 1.2

function gadget:FeatureCreated(featureID)
	local fx, fy, fz = Spring.GetFeaturePosition(featureID)

	local wantedheight = Spring.GetFeatureHeight(featureID)
	wantedheight = 0
	--wantedheight = 100
	
	local uID = Spring.CreateUnit(fakeunitID, fx, fy + wantedheight, fz, 0, gaiaTeamID) --create unit on top of the feature
	Spring.SetUnitNeutral(uID, true)
	--Spring.SetUnitMidAndAimPos(0, -wantedheight, 0, 0, 0 ,0, false)
	
	local mass = Spring.GetFeatureMass(featureID)
	Spring.SetUnitMass(uID, mass)
	Spring.SetUnitRulesParam(uID, "massOverride", mass, INLOS)

	--Spring.SetUnitMass(uID, 0)
	
	Spring.SetUnitSensorRadius(uID, "los", 0)
	Spring.SetUnitSensorRadius(uID, "airLos", 0)
	Spring.SetUnitSensorRadius(uID, "radar", 0)
	Spring.SetUnitSensorRadius(uID, "sonar", 0)
	Spring.SetUnitSensorRadius(uID, "seismic", 0)
	Spring.SetUnitSensorRadius(uID, "radarJammer", 0)
	Spring.SetUnitSensorRadius(uID, "sonarJammer", 0)
	
	local radius = Spring.GetFeatureRadius(featureID)
	local height = Spring.GetFeatureHeight(featureID)
	
	--height = 0
	
	Spring.SetUnitRadiusAndHeight(uID, radius * SIZE_MULT, height * SIZE_MULT)
	local scaleX, scaleY, scaleZ, offsetX, offsetY, offsetZ, 
		volumeType, testType, primaryAxis = Spring.GetFeatureCollisionVolumeData(featureID)		

	Spring.SetUnitSelectionVolumeData(uID, scaleX*SIZE_MULT, scaleY*SIZE_MULT, scaleZ*SIZE_MULT, offsetX*SIZE_MULT, height, offsetZ*SIZE_MULT, volumeType, testType, primaryAxis)
	Spring.SetUnitCollisionVolumeData(uID, scaleX*SIZE_MULT, scaleY*SIZE_MULT, scaleZ*SIZE_MULT, offsetX*SIZE_MULT, height, offsetZ*SIZE_MULT, volumeType, testType, primaryAxis)
	--Spring.SetUnitCollisionVolumeData(uID, scaleX, scaleY, scaleZ, offsetX, -height, offsetZ, volumeType, testType, primaryAxis)
	
	local collidable, crushable, blocking = Spring.GetFeatureBlocking(featureID)
	
	featuresInfo[featureID] = {
		fakeunit = uID,
		--x = fx, y = fy, z = fz
		wantedheight = wantedheight,
		
		collidable = collidable,
		crushable = crushable,
		blocking = blocking,
		
		radius = radius,
		height = height,
	}
	
	featuresByunitID[uID] = featureID	
end

function gadget:FeatureDestroyed(featureID)
	local uID = featuresInfo[featureID].fakeunit
	if uID and Spring.ValidUnitID(uID) then
		featuresByunitID[uID] = nil
		transportedFakes[uID] = nil
		Spring.DestroyUnit(uID)
	end
	featuresInfo[featureID] = nil
end

function gadget:AllowCommand_GetWantedCommand()	
	return {[CMD.ATTACK] = true}
end

function gadget:AllowCommand_GetWantedUnitDefID()	
	return true
end

function gadget:AllowCommand(unitID, unitDefID, unitTeam, cmdID, cmdParams, cmdOptions)
	if cmdID == CMD.ATTACK and #cmdParams == 1 and featuresByunitID[ cmdParams[1] ] then --something is planning on attack on fakeunit
		return false
	end
end

function gadget:GameFrame(f)
	--local features = Spring.GetAllFeatures()
	
	--update fake unit position for features in movement
	for fakeunit, featureID in pairs(featuresByunitID) do
		local _, _, _, vel = Spring.GetFeatureVelocity(featureID)
		if vel > 0 then
			local fx, fy, fz = Spring.GetFeaturePosition(featureID)
			local wantedheight = featuresInfo[featureID].wantedheight
			--Spring.SetUnitPosition(featuresInfo[featureID].fakeunit, fx, fy, fz)
			--local fakeunit = featuresInfo[featureID].fakeunit
			Spring.MoveCtrl.Enable(fakeunit)
			Spring.MoveCtrl.SetPosition(fakeunit, fx, fy + wantedheight, fz)
			Spring.MoveCtrl.SetRotation(fakeunit, Spring.GetFeatureRotation(featureID))
			Spring.MoveCtrl.Disable(fakeunit)
		end
	end
	
	--update features position for fake units being transported
	for uID, fID in pairs(transportedFakes) do
		local x, y, z = Spring.GetUnitPosition(uID)
		local wantedheight = featuresInfo[fID].wantedheight
		Spring.SetFeaturePosition(fID, x, y - wantedheight, z)
		Spring.SetFeatureRotation(fID, Spring.GetUnitRotation(uID))
	end
	
	
	--update features position for fake units being transported
	for uID, fID in pairs(featuresByunitID) do
		local _, _, _, vel = Spring.GetUnitVelocity(uID)
		--if vel > 0 then
			local x, y, z = Spring.GetUnitPosition(uID)
			local wantedheight = featuresInfo[fID].wantedheight
			Spring.SetFeaturePosition(fID, x, y - wantedheight, z)
			Spring.SetFeatureRotation(fID, Spring.GetUnitRotation(uID))
		--end
	end	
end

function gadget:UnitPreDamaged(unitID, unitDefID, unitTeam)
	if unitDefID == fakeunitID then
		return 0, 0
	end
end

function gadget:UnitLoaded(unitID, unitDefID, unitTeam, transportID, transportTeam)
	if unitDefID == fakeunitID then
		Spring.Echo("UnitLoaded")
		local fID = featuresByunitID[unitID]
		transportedFakes[unitID] = fID
		
		Spring.SetFeatureBlocking(fID, false, false, false)
		Spring.SetFeatureRadiusAndHeight(fID, 0, 0)
		--Spring.SetUnitRadiusAndHeight(unitID, 0, 0)		
	end
end

function gadget:UnitUnloaded(unitID, unitDefID, unitTeam, transportID, transportTeam)
	if unitDefID == fakeunitID then
		Spring.Echo("UnitUnloaded")
		local fID = featuresByunitID[unitID]
		local fInfoItem = featuresInfo[fID]
		
		Spring.SetFeatureBlocking(fID, fInfoItem.collidable, fInfoItem.crushable, fInfoItem.blocking)
		Spring.SetUnitRadiusAndHeight(unitID, fInfoItem.radius * SIZE_MULT, fInfoItem.height * SIZE_MULT)
		Spring.SetFeatureRadiusAndHeight(fID, fInfoItem.radius, fInfoItem.height)
		
		transportedFakes[unitID] = nil
	end
end

--UNSYNCED
else

include("LuaRules/Configs/customcmds.h.lua")

--local fakeunitID = SYNCED.fakeunitID
local fakeunitID = UnitDefNames["featureunit2"].id
local fakeUnits = {}

function gadget:Initialize()
	for _, unitID in ipairs(Spring.GetAllUnits()) do
		local unitDefID = Spring.GetUnitDefID(unitID)
		local teamID = Spring.GetUnitTeam(unitID)
		gadget:UnitCreated(unitID, unitDefID, teamID)
	end
end

function gadget:DefaultCommand(targetType, targetID)
	if (targetType == "unit") and fakeUnits[targetID] then
		return CMD.LOAD_UNITS
	end
end


function gadget:UnitCreated(unitID, unitDefID, teamID)
	if unitDefID == fakeunitID then
		--Spring.SetUnitNoDraw(unitID, true)
		--Spring.SetUnitNoSelect(unitID, true) --leave commented out
		Spring.Echo("UnitCreated")
		fakeUnits[unitID] = true
		Spring.UnitRendering.SetUnitLuaDraw(unitID, true)		
	end
end

function gadget:UnitDestroyed(unitID, unitDefID)
	if fakeUnits[unitID] then
		fakeUnits[unitID] = nil
	end
end

function gadget:DrawUnit(unitID, drawMode)
	if fakeUnits[unitID] then
		return true --supress engine drawing
	end
end

end