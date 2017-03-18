function gadget:GetInfo()
   return {
      name      = "Transportable features",      
      author    = "ivand",
      date      = "2017",
      license   = "GNU GPL, v2 or later",
      layer     = 0,
      enabled   = true
   }
end

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


local SIZE_MULT = 1.0

function gadget:FeatureCreated(featureID)
	local fx, fy, fz = Spring.GetFeaturePosition(featureID)

	local wantedheight = Spring.GetFeatureHeight(featureID)
	--wantedheight = 0
	
	local uID = Spring.CreateUnit(fakeunitID, fx, fy, fz, 0, gaiaTeamID) --create unit on top of the feature	
	Spring.SetUnitMidAndAimPos(0, wantedheight, 0, 0 ,wantedheight ,0, false)
	
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
	
	Spring.SetUnitRadiusAndHeight(uID, radius * SIZE_MULT, height * SIZE_MULT)
	
	local collidable, crushable, blocking = Spring.GetFeatureBlocking(featureID)
	
	featuresInfo[featureID] = {
		fakeunit = uID,
		--x = fx, y = fy, z = fz
		wantedheight = 0,
		
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

function gadget:GameFrame(f)
	local features = Spring.GetAllFeatures()
	
	--update fake unit position for features in movement
	for _, featureID in pairs(features) do
		local _, _, _, vel = Spring.GetFeatureVelocity(featureID)
		if vel > 0 then
			local fx, fy, fz = Spring.GetFeaturePosition(featureID)
			local wantedheight = featuresInfo[featureID].wantedheight
			--Spring.SetUnitPosition(featuresInfo[featureID].fakeunit, fx, fy, fz)
			local fakeunit = featuresInfo[featureID].fakeunit
			Spring.MoveCtrl.Enable(fakeunit)
			Spring.MoveCtrl.SetPosition(fakeunit, fx, fy + wantedheight, fz)
			Spring.MoveCtrl.Disable(fakeunit)
		end
	end
	
	--update features position for fake units being transported
	for uID, fID in pairs(transportedFakes) do
		local x, y, z = Spring.GetUnitPosition(uID)
		local wantedheight = featuresInfo[fID].wantedheight
		Spring.SetFeaturePosition(fID, x, y - wantedheight, z)
	end
	
	
	--update features position for fake units being transported
	for uID, fID in pairs(featuresByunitID) do
		local _, _, _, vel = Spring.GetUnitVelocity(uID)
		if vel > 0 then
			local x, y, z = Spring.GetUnitPosition(uID)
			local wantedheight = featuresInfo[fID].wantedheight
			Spring.SetFeaturePosition(fID, x, y - wantedheight, z)
		end
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


--local fakeunitID = SYNCED.fakeunitID
local fakeunitID = UnitDefNames["featureunit2"].id

function gadget:UnitCreated(unitID, unitDefID, teamID)
	if unitDefID == fakeunitID then
		--Spring.SetUnitNoDraw(unitID, true)
		Spring.SetUnitNoSelect(unitID, true) --leave commented out
		--Spring.SetUnitNoMinimap(unitID, true)
		--Spring.SetUnitNoDraw(unitID, false)
	end
end

end