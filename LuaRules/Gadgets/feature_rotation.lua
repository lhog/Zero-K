function gadget:GetInfo()
   return {
      name      = "Feature rotation",      
      author    = "ivand",
      date      = "2017",
      license   = "GNU GPL, v2 or later",
      layer     = 1001,
      enabled   = false,
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



if (gadgetHandler:IsSyncedCode()) then
------------------------------------------------------
--SYNCED
------------------------------------------------------


--Quarternion LOL
local function YRPToQuarternion(pYaw, pRoll, pPitch)
	local t0 = math.cos(pYaw * 0.5)
	local t1 = math.sin(pYaw * 0.5)
	local t2 = math.cos(pRoll * 0.5)
	local t3 = math.sin(pRoll * 0.5)
	local t4 = math.cos(pPitch * 0.5)
	local t5 = math.sin(pPitch * 0.5)
	
	local q = {
		[0] = t0 * t2 * t4 + t1 * t3 * t5, --w
		[1] = t0 * t3 * t4 - t1 * t2 * t5, --x
		[2] = t0 * t2 * t5 + t1 * t3 * t4, --y
		[3] = t1 * t2 * t4 - t0 * t3 * t5, --z
	}
	return q
end

local function QuarternionMult(q,r)
    return {
		[0] = r[0] * q[0] - r[1] * q[1] - r[2] * q[2] - r[3] * q[3],
		[1] = r[0] * q[1] + r[1] * q[0] - r[2] * q[3] + r[3] * q[2],
		[2] = r[0] * q[2] + r[1] * q[3] + r[2] * q[0] - r[3] * q[1],
		[3] = r[0] * q[3] - r[1] * q[2] + r[2] * q[1] + r[3] * q[0],	
	}
end
	

local function RotatePointQuarternion(px, py, pz, q)
	local r = {
		[0] = 0,
		[1] = px,
		[2] = py,
		[3] = pz,
	}
	
	q_conj = {
		[0] = q[0],
		[1] = -1*q[1],
		[2] = -1*q[2],
		[3] = -1*q[3],
	}
	
	local p = QuarternionMult( QuarternionMult(q, r), q_conj )
	--local p = QuarternionMult(q, r)

	return p[1], p[2], p[3]
end

---pYaw2, -pRoll2, -pPitch2
local function RotatePoint(px, py, pz, pPitch, pYaw, pRoll)
	local q = YRPToQuarternion(pYaw, pRoll, pPitch)
	return RotatePointQuarternion(px, py, pz, q)	
end



local function RotatePoint2(px, py, pz, pPitch, pYaw, pRoll)
	local cosa = math.cos(pRoll)
	local sina = math.sin(pRoll)
	
	local cosb = math.cos(pYaw)
	local sinb = math.sin(pYaw)

	local cosc = math.cos(pPitch)
	local sinc = math.sin(pPitch)

	local Axx = cosa*cosb
	local Axy = cosa*sinb*sinc - sina*cosc
	local Axz = cosa*sinb*cosc + sina*sinc

	local Ayx = sina*cosb
	local Ayy = sina*sinb*sinc + cosa*cosc
	local Ayz = sina*sinb*cosc - cosa*sinc

	local Azx = -sinb
	local Azy = cosb*sinc
	local Azz = cosb*cosc
	
	return  --xyz
		Axx*px + Axy*py + Axz*pz,
		Ayx*px + Ayy*py + Ayz*pz,
		Azx*px + Azy*py + Azz*pz

end
--[[
local function Rot(A, x, y, z, w, angle)
	local R = {
		[1] = A[1][1] * x + A[1][2] * y + A[1][3] * z + A[1][4] * w,
		[2] = A[2][1] * x + A[2][2] * y + A[2][3] * z + A[2][4] * w,
		[3] = A[3][1] * x + A[3][2] * y + A[3][3] * z + A[3][4] * w,
		[4] = A[4][1] * x + A[4][2] * y + A[4][3] * z + A[4][4] * w,
	}
	return R[1], R[2], R[3], R[4]
end

local function RotX(x, y, z, angle) -- Pitch
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {1,	0,		0,		0},
		[2] = {0,	cosA,	-sinA,	0},
		[3] = {0,	sinA,	cosA,	0},
		[4] = {0,	0, 		0,		1},
	}

	return Rot(A, x, y, z, 1, angle)
end
]]--

local function Rot(A, x, y, z)
	local R = {
		[1] = A[1][1] * x + A[1][2] * y + A[1][3] * z,
		[2] = A[2][1] * x + A[2][2] * y + A[2][3] * z,
		[3] = A[3][1] * x + A[3][2] * y + A[3][3] * z,
	}
	return R[1], R[2], R[3]
end

--[[
local function RotX(x, y, z, angle) --Pitch
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {1, 0, 0},
		[2] = {0, cosA, -sinA},
		[3] = {0, sinA,	cosA},
	}

	return Rot(A, x, y, z)
end

local function RotY(x, y, z, angle) --roll
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {cosA, -sinA, 0},
		[2] = {sinA, cosA, 0},
		[3] = {0, 0, 1},
	}

	return Rot(A, x, y, z)
end

local function RotZ(x, y, z, angle) --Yaw
	local cosA = math.cos(angle)
	local sinA = math.sin(angle)
	local A = {
		[1] = {cosA, 0, sinA},
		[2] = {0, 1, 0},
		[3] = {-sinA, 0, cosA},
	}

	return Rot(A, x, y, z)
end
]]--

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


local wreckID = nil
local unitID = nil

local fHeight = 100

function gadget:Initialize()
	Spring.Echo("START!!!!")
	local x, z = Game.mapSizeX/2, Game.mapSizeZ/2
	local y = Spring.GetGroundHeight(x, z)
	unitID = Spring.CreateUnit(UnitDefNames["corcrw"].id, x, y + 200, z, 0, Spring.GetGaiaTeamID())
	wreckID = Spring.CreateFeature(FeatureDefNames["armzeus_dead"].id, x, y + 200 - fHeight, z)
	ePrintEx({unitID=unitID, wreckID=wreckID})
	Spring.MoveCtrl.Enable(unitID)
	Spring.MoveCtrl.SetVelocity(0, 0, 0)
	Spring.MoveCtrl.SetPosition(x, y + 200, z)
	Spring.MoveCtrl.SetRotation(unitID, math.pi/2 , 0, 0)
end

function gadget:Shutdown()
	Spring.MoveCtrl.Disable(unitID)
	Spring.DestroyUnit(unitID, true, true)
	Spring.DestroyFeature(wreckID)
end


local pace = 0.01
local DirPace = 0
function gadget:GameFrame(f)
	local _, _, _, px, py, pz = Spring.GetUnitPosition(unitID, true)
	local pPitch, pYaw, pRoll = Spring.GetUnitRotation(unitID)
	local pPitch2, pYaw2, pRoll2 = pPitch + pace, pYaw, pRoll + pace
	
	--Spring.MoveCtrl.SetRotation(unitID, -pPitch2, -pYaw2, -pRoll2)

	local normx, normy, normz = 0, 1, 0
	
	normx, normy, normz = RotZXY(normx, normy, normz, pPitch2, pYaw2, pRoll2)
	
	SendToUnsynced("DrawData", normx, normy, normz, px, py, pz, fHeight)
	
	ePrintEx({normx = normx, normy = normy, normz = normz})
	
	

	--local fxd, fyd, fzd = RotX(0, 0 - fHeight, 0, -pPitch)
	--local fxd, fyd, fzd = RotY(0, 0 - fHeight, 0, -pRoll2)
	local fxd, fyd, fzd = RotZXY(0, 0 - fHeight, 0, pPitch2, pYaw2, pRoll2)
	--local fxd, fyd, fzd = RotatePoint(0, 0 - fHeight, 0, pPitch2, pYaw2, pRoll2)

	--local fxd, fyd, fzd = RotatePoint2(0, 0 - fHeight, 0, -pYaw2, -pRoll2, -pPitch2)
	
	
	Spring.SetFeatureMoveCtrl(wreckID, true,
											0, 0, 0,
											0, 0 ,0)
	
	--ePrintEx({pPitch2 = -pPitch2, pYaw2 = -pYaw2, pRoll2 = -pRoll2})
	--ePrintEx({fxd=fxd, fyd=fyd, fzd=fzd})
	Spring.SetFeaturePosition(wreckID, px + fxd, py + fyd, pz + fzd)
	--Spring.SetFeatureMidAndAimPos(wreckID, px + fxd, py + fyd, pz + fzd, px + fxd, py + fyd, pz + fzd, false)
	--Spring.SetFeatureRotation(wreckID, pPitch2, pYaw2, pRoll2)
	local fPitch, fYaw, fRoll = Spring.GetFeatureRotation(wreckID)
	local dirx, diry, dirz = Spring.GetFeatureDirection(wreckID)
	
	--local fPitch2, fYaw2, fRoll2 = fPitch + pace, fYaw, fRoll + pace
	local fPitch2, fYaw2, fRoll2 = fPitch, fYaw, fRoll
	local dirx2, diry2, dirz2 = dirx + DirPace, diry, dirz
	ePrintEx({fPitch2 = -fPitch2, fYaw2 = -fYaw2, fRoll2 = -fRoll2})
	--Spring.SetFeatureDirection(wreckID, -fPitch2, -fYaw2, -fRoll2)
	Spring.SetFeatureDirection(wreckID, dirx2, diry2, dirz2)
	
end


else
------------------------------------------------------
-- UNSYNCED
------------------------------------------------------

local x1, y1, z1, x2, y2, z2
local x0, y0, z0
local function DrawData(_, normx, normy, normz, x00, y00, z00, leng)
	--Spring.Echo("DrawData")
	x1, y1, z1 = normx * leng, normy * leng, normz * leng
	x2, y2, z2 = -normx * leng, -normy * leng, -normz * leng
	x0, y0, z0 = x00, y00, z00
	x1, y1, z1 = x1 + x0, y1 + y0, z1 + z0
	x2, y2, z2 = x2 + x0, y2 + y0, z2 + z0
end

function gadget:Initialize()
	gadgetHandler:AddSyncAction("DrawData", DrawData)
end

function gadget:Shutdown()
	gadgetHandler.RemoveSyncAction("DrawData")
end

local function DoLine()
	gl.Color(1, 0, 0, 1)	
	gl.Vertex(x1, y1, z1)
	gl.Vertex(x0, y0, z0)
	gl.Color(0, 0, 1, 1)
	gl.Vertex(x0, y0, z0)
	gl.Vertex(x2, y2, z2)
	gl.Color(1, 1, 1, 1)
end

function gadget:DrawWorld()
	--gl.PushMatrix()
	if x0 then
		gl.DepthTest(true)
		--gl.Translate(x0, y0, z0)
		gl.LineWidth(8)
		gl.PointSize(8)
		gl.Color(1, 1, 1, 1)	
		gl.BeginEnd(GL.LINE_STRIP, DoLine)
		gl.PointSize(1)
		gl.LineWidth(1)		
	end
	--gl.PopMatrix()
end

end