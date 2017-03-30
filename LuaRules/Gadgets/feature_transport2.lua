function gadget:GetInfo()
   return {
      name      = "Transportable features2",      
      author    = "ivand",
      date      = "2017",
      license   = "GNU GPL, v2 or later",
      layer     = 1000,
      enabled   = false,
   }
end

include("LuaRules/Configs/customcmds.h.lua")

local loadWreckCmdDesc = {
  id      = CMD_LOAD_WRECK,
  type    = CMDTYPE.ICON_UNIT_FEATURE_OR_AREA,
  name    = 'loadwreck',
  cursor  = 'Load units',
  action  = 'loadwreck',
  tooltip = 'Load Wreck',
}

local unloadWreckCmdDesc = {
  id      = CMD_UNLOAD_WRECK,
  type    = CMDTYPE.ICON_AREA,
  name    = 'unloadwreck',
  cursor  = 'Unload units',
  action  = 'unloadwreck',
  tooltip = 'Unload Wreck',
}


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



if (gadgetHandler:IsSyncedCode()) then
------------------------------------------------------
--SYNCED
------------------------------------------------------

local wreckTransporters = {	
	--[UnitDefNames.corbtrans.id] = true, -- Vindicator
	[UnitDefNames.corvalk.id] = true, -- Valkyrie
}

local wreckTransCmds = {
	[CMD_LOAD_WRECK] = true,
	[CMD_UNLOAD_WRECK] = true,
}

local MASS_LIMIT = 1200
local MOVE_PRECISION = 16
local PICK_RANGE = 2 * MOVE_PRECISION
local PICK_RANGE2 = PICK_RANGE * PICK_RANGE

local DEG_TO_HEADING = 32768 / 180
local HEADING_TO_RAD = 1 / 32768 * math.pi
local RADIANS_PER_COBANGLE = math.pi / 32768

local ATTACH_RANGE2 = 16^2
local ATTACH_ROT2 = 0.05^2
local ATTACH_HEAD = 2 * DEG_TO_HEADING --degrees

local MAX_UNITS = Game.maxUnits

local allyIDByTeamID
local function makeAllyIDByTeamID()
	allyIDByTeamID = {}
	local allyTeamList = Spring.GetAllyTeamList()
	for _, atID in ipairs(allyTeamList) do
		local teamsInAlliance = Spring.GetTeamList(atID)
		for _, tID in ipairs(teamsInAlliance) do
			allyIDByTeamID[tID] = atID
		end
	end
end

function gadget:Initialize()
	makeAllyIDByTeamID()
	-- register command
	gadgetHandler:RegisterCMDID(loadWreckCmdDesc.id)
	gadgetHandler:RegisterCMDID(unloadWreckCmdDesc.id)
	
	Spring.Echo("Synced")
	
	for _, unitID in ipairs(Spring.GetAllUnits()) do
		local unitDefID = Spring.GetUnitDefID(unitID)
		local teamID = Spring.GetUnitTeam(unitID)
		gadget:UnitCreated(unitID, unitDefID, teamID)
	end
end

function gadget:Shutdown()
	for _, unitID in ipairs(Spring.GetAllUnits()) do
		local unitDefID = Spring.GetUnitDefID(unitID)
		local teamID = Spring.GetUnitTeam(unitID)
		gadget:UnitDestroyed(unitID, unitDefID, teamID)
	end
end

-------------------------------------------------------

function gadget:TeamChanged(teamID)	
	makeAllyIDByTeamID()
end

function gadget:PlayerChanged(playerID)
	makeAllyIDByTeamID()
end

function gadget:PlayerAdded(playerID)
	makeAllyIDByTeamID()
end

function gadget:PlayerRemoved(playerID)
	makeAllyIDByTeamID()
end

function gadget:TeamDied(teamID)
	makeAllyIDByTeamID()
end

function gadget:TeamChanged(teamID)
	makeAllyIDByTeamID()
end

-------------------------------------------------------


local piecesByUnit = {}
local physicsByUnit = {}

local function LoadWreckEnable(unitID, enable)
	local cmdDescID = Spring.FindUnitCmdDesc(unitID, loadWreckCmdDesc.id)
	if (cmdDescID) then
		local cmdArray = {disabled = not(enable)}
		Spring.EditUnitCmdDesc(unitID, cmdDescID, cmdArray)
	end
end

local function UnloadWreckEnable(unitID, enable)
	local cmdDescID = Spring.FindUnitCmdDesc(unitID, unloadWreckCmdDesc.id)
	if (cmdDescID) then
		local cmdArray = {disabled = not(enable)}
		Spring.EditUnitCmdDesc(unitID, cmdDescID, cmdArray)
	end
end

function gadget:UnitCreated(unitID, unitDefID, unitTeam, builderID)
	if wreckTransporters[unitDefID] then
		Spring.InsertUnitCmdDesc(unitID, loadWreckCmdDesc)
		Spring.InsertUnitCmdDesc(unitID, unloadWreckCmdDesc)
		if (Spring.GetUnitRulesParam(unitID, "loadedWrecksMass") or 0) == 0 then
			Spring.SetUnitRulesParam(unitID, "loadedWrecksMass", 0)
			---UnloadWreckEnable(unitID, false)
		end
		
		local env = Spring.UnitScript.GetScriptEnv(unitID)
		if env and env.QueryTransport then
			local linkPiece = Spring.UnitScript.CallAsUnit(unitID, env.QueryTransport)
			piecesByUnit[unitID] = linkPiece
			Spring.Echo("piecesByUnit = linkPiece")
		else
			piecesByUnit[unitID] = nil
			Spring.Echo("piecesByUnit = nil")
		end
		
		physicsByUnit[unitID] = {
			turnRate = UnitDefs[unitDefID].turnRate * HEADING_TO_RAD,
			--verticalSpeed = UnitDefs[unitDefID].verticalSpeed / 30,
			--turnRate = math.sqrt(ATTACH_ROT2) / 4,
			--headingChangeRate = DEG_TO_HEADING * 10,
			headingChangeRate = UnitDefs[unitDefID].turnRate, --COB angle units per frame
			verticalSpeed = 8,
		}
		--ePrintUnitDef(UnitDefs[unitDefID])
		ePrintEx({physicsByUnit_uid = physicsByUnit[unitID]})
	end
end

function gadget:UnitDestroyed(unitID, unitDefID, unitTeam)
	if wreckTransporters[unitDefID] then
		local cmdDescID
		
		cmdDescID = Spring.FindUnitCmdDesc(unitID, CMD_LOAD_WRECK)
		if (cmdDescID) then
			Spring.RemoveUnitCmdDesc(unitID, cmdDescID)
		end
		
		cmdDescID = Spring.FindUnitCmdDesc(unitID, CMD_UNLOAD_WRECK)
		if (cmdDescID) then
			Spring.RemoveUnitCmdDesc(unitID, cmdDescID)
		end
		
		piecesByUnit[unitID] = nil
		physicsByUnit[unitID] = nil
		
	end
end

function gadget:FeatureCreated(featureID)
	
end

function gadget:FeatureDestroyed(featureID)

end

local gaiaTeamID = Spring.GetGaiaTeamID()

local function findBestMetalWreck(x, y, z, radius, capacityLeft, unitTeam)
	--assert(unitTeam and unitTeam~=-1)
	local allyTeamID = allyIDByTeamID[unitTeam]
	local features = Spring.GetFeaturesInCylinder(x, z, radius)	
	
	local maxMetal = 0
	local maxMetalFID = nil
	local maxAllowedMetalFID = nil
	
	for _, fID in ipairs(features) do
		if Spring.ValidFeatureID(fID) then
			local x, y, z = Spring.GetFeaturePosition(fID)
			local los = Spring.IsPosInLos(x, y, z, allyTeamID)
			local aLos = Spring.IsPosInAirLos(x, y, z, allyTeamID)
			local gaiaVisible = Spring.GetFeatureTeam(fID) == gaiaTeamID --assume featureVisibility modrule doesn't change from status quo
			if (los and aLos) or gaiaVisible then
				local metal = Spring.GetFeatureResources(fID)
				local mass = Spring.GetFeatureMass(fID)
				if metal > maxMetal then				
					maxMetal = metal
					maxMetalFID = fID
					if mass <= capacityLeft then
						maxAllowedMetalFID = maxMetalFID
					end
				end
			end
		end
	end
	return maxAllowedMetalFID, maxMetalFID
end

local function Distance2D2(x1, z1, x2, z2)
	return (x1-x2)^2 + (z1-z2)^2
end

local function Distance3D2(x1, y1, z1, x2, y2, z2)
	local dx, dy, dz = x2 - x1, y2 - y1, z2 - z1
	return dx^2 + dy^2 + dz^2, dx, dy, dz
end

local function math_sign(x)
	if x<0 then
		return -1
	elseif x>0 then
		return 1
	else
		return 0
	end
end



local function ReportStatus(unitID, featureID)
	return true, false
end




local pickingUp = {}

function gadget:CommandFallback(unitID, unitDefID, unitTeam, cmdID, cmdParams, cmdOptions, cmdTag)
	if wreckTransCmds[cmdID] and wreckTransporters[unitDefID] and cmdParams then		
		Spring.Echo("CommandFallback")
		--ePrintCMD({id=cmdID, params=cmdParams, options=cmdOptions, tag=cmdTag})

		if cmdID == CMD_LOAD_WRECK then		
			if #cmdParams == 4 and pickingUp[unitID] == nil then --area command and not yet picking stuff up

				local capacityLeft = MASS_LIMIT - (Spring.GetUnitRulesParam(unitID ,"loadedWrecksMass") or 0)
				local mAMfID, mMfID = findBestMetalWreck(cmdParams[1], cmdParams[2], cmdParams[3], cmdParams[4], capacityLeft, unitTeam)

				if mAMfID then --found light enough metal wreck					
					Spring.GiveOrderToUnit(unitID, CMD.INSERT, {cmdTag, CMD_LOAD_WRECK, CMD.OPT_INTERNAL, MAX_UNITS + mAMfID}, {} )
					return true, false					
				elseif mMfID then --didn't find light enough metal wreck, but there are other wrecks around
					--TODO, GO UNLOAD
					Spring.Echo("Need to unload!!!!")
					return true, false --no wrecks left or no 
				else
					return true, true
				end

			elseif #cmdParams == 1 then --pick up a wreck command
				local fID = cmdParams[1] - MAX_UNITS
				pickingUp[unitID] = fID  -- unitID --> featureID
				return ReportStatus(unitID, fID)
			else
				-- log error
			end
		elseif cmdID == CMD_UNLOAD_WRECK and #cmdParams == 4 then
			Spring.Echo("CMD_UNLOAD_WRECK!!!!")
		end
	end
	
	--return: bool used, bool finished
	return false --something is wrong or we don't care	
end

function gadget:AllowCommand_GetWantedCommand()
	return wreckTransCmds
	--return true
end

function gadget:AllowCommand_GetWantedUnitDefID()
	return wreckTransporters
end

--[[
function gadget:UnitCommand(unitID, unitDefID, unitTeam, cmdID, cmdParams, cmdOpts, cmdTag)
	if wreckTransCmds[cmdID] then
		Spring.Echo("Printing out command queue for unit "..unitID)
		ePrintCMDQueue(Spring.GetUnitCommands(unitID))
	end
end
]]--



--TODO remove commands because it's full or empty
function gadget:AllowCommand(unitID, unitDefID, unitTeam, cmdID, cmdParams, cmdOptions)
	if wreckTransCmds[cmdID] then
		Spring.Echo("AllowCommand")
		--ePrintCMD({id=cmdID, params=cmdParams, options=cmdOptions})
	end
	return true
end


local unitMoveControlled = {}
local featuresCarried = {}

local function Rot(A, x, y, z)
	local R = {
		[1] = A[1][1] * x + A[1][2] * y + A[1][3] * z,
		[2] = A[2][1] * x + A[2][2] * y + A[2][3] * z,
		[3] = A[3][1] * x + A[3][2] * y + A[3][3] * z,
	}
	return R[1], R[2], R[3]
end

local function RotX(x, y, z, angle) --Pitch
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {1, 0, 0},
		[2] = {0,  cosA, sinA},
		[3] = {0, -sinA, cosA},
	}

	return Rot(A, x, y, z)
end

local function RotY(x, y, z, angle) --Yaw
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {cosA, 0, -sinA},
		[2] = {0, 1, 0},
		[3] = {sinA, 0,  cosA},
	}

	return Rot(A, x, y, z)
end

local function RotZ(x, y, z, angle) --Roll
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = { cosA, sinA, 0},
		[2] = {-sinA, cosA, 0},
		[3] = {0, 0, 1},
	}

	return Rot(A, x, y, z)
end

local function RotZXY(x, y, z, pPitch, pYaw, pRoll)	
	x, y, z = RotZ(x, y, z, pRoll)
	x, y, z = RotY(x, y, z, pYaw)
	x, y, z = RotX(x, y, z, pPitch)	
	
	return x, y, z
end

local pickUpPaceFactor = 1
function gadget:GameFrame(f)
	local skipList = {}
	for unitID, featureID in pairs(pickingUp) do
		local valid = true
		
		if not (Spring.ValidFeatureID(featureID)) then
			skipList[unitID] = featureID
		end
		
		if not(Spring.ValidUnitID(unitID) and Spring.GetUnitIsDead(unitID) == false) then
			valid = false		
			skipList[unitID] = featureID
		end
		
		if valid and (f % pickUpPaceFactor == 0) then
		
			local fx, fy, fz = Spring.GetFeaturePosition(featureID)
			local fHeight = Spring.GetFeatureHeight(featureID) or 0
			fy = math.max(fy, 0) + 	fHeight			
			
			local px, py, pz
			if piecesByUnit[unitID] then
				px, py, pz = Spring.GetUnitPiecePosition(unitID, piecesByUnit[unitID])
			else
				px, py, pz = Spring.GetUnitPosition(unitID)
			end

			
			
			if Distance2D2(fx, fz, px, pz) <= PICK_RANGE2 then				
				local dist2, dx, dy, dz = Distance3D2(fx, fy, fz, px, py, pz)
				
				pPitch, pYaw, pRoll = Spring.GetUnitRotation(unitID)
				fPitch, fYaw, fRoll = Spring.GetFeatureRotation(featureID)
				--fYaw = -fYaw + math.pi
				--fPitch = - fPitch
				--fRoll = fRoll
				
				--local dPitch, dYaw, dRoll = 
				local rot2, dPitch, dYaw, dRoll = Distance3D2(fPitch, fYaw, fRoll, pPitch, pYaw, pRoll)
				--local rot2, dPitch, dYaw, dRoll = Distance3D2(0, fYaw, 0, 0, pYaw, 0)
				--rot2 = 0

				pHeading = Spring.GetUnitHeading(unitID)
				fHeading = Spring.GetFeatureHeading(featureID)
				
				local dHeading = pHeading - fHeading				
				--ePrintEx({dHeading=dHeading, pHeading=pHeading, fHeading=fHeading})
				
				--ePrintEx({pPitch=pPitch, pYaw=pYaw, pRoll=pRoll, fPitch=fPitch, fYaw=fYaw, fRoll=fRoll, dPitch=dPitch, dYaw=dYaw, dRoll=dRoll})
				--ePrintEx({pYaw=pYaw, fYaw=fYaw})
				
				if dist2 <= ATTACH_RANGE2 and rot2 <= ATTACH_ROT2 then
				--if dist2 <= ATTACH_RANGE2 and rot2 <= ATTACH_ROT2 and math.abs(dHeading) <= ATTACH_HEAD then
				--if dist2 <= ATTACH_RANGE2 and math.abs(dHeading) <= ATTACH_HEAD then
					--do attach
					Spring.Echo("ATTACH!!!")
					Spring.MoveCtrl.Disable(unitID)
					--pickingUp[unitID] = nil
					skipList[unitID] = featureID
					unitMoveControlled[unitID] = false
					featuresCarried[featureID] = unitID
				else
					if not unitMoveControlled[unitID] then
						Spring.MoveCtrl.Enable(unitID)
						unitMoveControlled[unitID] = true
					end
					Spring.Echo("-==================-")
					if dist2 > ATTACH_RANGE2 then						
						local velNormMult = physicsByUnit[unitID].verticalSpeed / math.sqrt(dist2) * pickUpPaceFactor
						local vx, vy, vz = dx * velNormMult, dy * velNormMult, dz * velNormMult
						local ux, uy, uz = Spring.GetUnitPosition(unitID)
						Spring.MoveCtrl.SetPosition(unitID, ux - vx, uy - vy, uz - vz)    
					end

					
					if rot2 > ATTACH_ROT2 then
						local rotNormMult = physicsByUnit[unitID].turnRate / math.sqrt(rot2) * pickUpPaceFactor
						local rPitch, rYaw, rRoll = dPitch * rotNormMult, dYaw * rotNormMult, dRoll * rotNormMult
						--ePrintEx({pPitch=pPitch, pYaw=pYaw, pRoll=pRoll, fPitch=fPitch, fYaw=fYaw, fRoll=fRoll})
						--ePrintEx({dPitch=dPitch, dYaw=dYaw, dRoll=dRoll})
						--ePrintEx({rPitch=rPitch, rYaw=rYaw, rRoll=rRoll})
						--ePrintEx({rot2=rot2, rotNormMult=rotNormMult, rPitch=rPitch, rYaw=rYaw, rRoll=rRoll})
						
						--ePrintEx({pYaw=pYaw, fYaw=fYaw})
						--ePrintEx({rot2=rot2, rotNormMult=rotNormMult, rYaw=rYaw, dYaw=dYaw })
						
						Spring.MoveCtrl.SetRotation(unitID, -(pPitch - rPitch), -(pYaw - rYaw), -(pRoll - rRoll))
					end	
				end
			else
				if f % 15 == 0 then --pace it to twice a sim second
					if unitMoveControlled[unitID] then
						Spring.MoveCtrl.Disable(unitID)
						unitMoveControlled[unitID] = false
					end

					Spring.SetUnitMoveGoal(unitID, fx, fy + fHeight, fz, MOVE_PRECISION)
				end
			end			
		end
		
	end
	
	for unitID, featureID in pairs(skipList) do
		pickingUp[unitID] = nil
	end
	
	skipList = {}
	
	---CARRY FEATURES---
	for featureID, unitID in pairs(featuresCarried) do
		local valid = true
		
		if not (Spring.ValidFeatureID(featureID)) then
			skipList[unitID] = featureID
		end
		
		if not(Spring.ValidUnitID(unitID) and Spring.GetUnitIsDead(unitID) == false) then
			valid = false		
			skipList[unitID] = featureID
		end
		
		if valid then
			
			Spring.Echo("CARRYING!!!")
		
			local px, py, pz
			if piecesByUnit[unitID] then
				px, py, pz = Spring.GetUnitPiecePosition(unitID, piecesByUnit[unitID])
			else
				px, py, pz = Spring.GetUnitPosition(unitID)
			end
			local pPitch, pYaw, pRoll = Spring.GetUnitRotation(unitID)
			local fHeight = Spring.GetFeatureHeight(featureID) or 0
			
			Spring.SetFeatureRotation(featureID, pPitch, pYaw, pRoll)
			
			local fxd, fyd, fzd = RotZXY(0, 0 - fHeight, 0, pPitch, pYaw, pRoll)			
			
			Spring.SetFeaturePosition(featureID, px + fxd, py + fyd, pz + fzd)			
			Spring.SetUnitCOBValue(unitID, COB.BUSY, 1);
			--local mt = Spring.GetUnitMoveTypeData(unitID)
			--Spring.Echo("MoveType = "..(mt or "nil"))
			--Spring.MoveCtrl.SetGunshipMoveTypeData(unitID, {wantedHeight = 140})
			--Spring.GiveOrderToUnit(unitID, CMD.IDLEMODE, {0}, { })
		else
			skipList[featureID] = unitID
		end		
	end
	
	for unitID, featureID in pairs(skipList) do
		featuresCarried[featureID] = nil
	end
end

function gadget:UnitLoaded(unitID, unitDefID, unitTeam, transportID, transportTeam)

end

function gadget:UnitUnloaded(unitID, unitDefID, unitTeam, transportID, transportTeam)
end

else
------------------------------------------------------
-- UNSYNCED
------------------------------------------------------


function gadget:Initialize()
	Spring.Echo("UNsynced&&&")
	--ePrintEx(loadWreckCmdDesc)
	--Spring.AssignMouseCursor(loadWreckCmdDesc.name, "cursorpickup", true)
	--Spring.AssignMouseCursor(unloadWreckCmdDesc.name, "cursorunload", true)	
	--load        0.3  1.0  1.0  0.4
	--unload      1.0  1.0  0.0  0.4		
	Spring.SetCustomCommandDrawData(loadWreckCmdDesc.id, loadWreckCmdDesc.cursor, {0.3, 1.0, 1.0, 0.4}, true)
	Spring.SetCustomCommandDrawData(unloadWreckCmdDesc.id, unloadWreckCmdDesc.cursor, {1.0, 1.0, 0.0, 0.4}, true)	

	
	--TODO figure out what it does
	
	--Spring.SetCustomCommandDrawData(CMD_LOAD_WRECK, CMD.LOAD_UNITS, {0.3, 1.0, 1.0, 1.0}, true)
	--Spring.SetCustomCommandDrawData(CMD_UNLOAD_WRECK, CMD.UNLOAD_UNITS, {1.0, 1.0, 0.0, 1.0}, true)	
	--gadgetHandler:RemoveGadget()
end

--[[
function gadget:DefaultCommand(targetType, targetID)
end



function gadget:UnitCreated(unitID, unitDefID, teamID)
end

function gadget:UnitDestroyed(unitID, unitDefID)
end

function gadget:DrawUnit(unitID, drawMode)
end

]]--

end