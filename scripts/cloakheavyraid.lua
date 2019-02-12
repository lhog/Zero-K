
include "constants.lua"

-- pieces
local head = piece "head"
local hips = piece "hips"
local chest = piece "chest"

-- left arm
local lshoulder = piece "lshoulder"
local lforearm = piece "lforearm"
local halberd = piece "halberd"
local blade = piece "blade"

-- right arm
local rshoulder = piece "rshoulder"
local rforearm = piece "rforearm"

-- left leg
local lthigh = piece "lthigh"
local lshin = piece "lshin"
local lfoot = piece "lfoot"

-- right leg
local rthigh = piece "rthigh"
local rshin = piece "rshin"
local rfoot = piece "rfoot"

local smokePiece = {head, hips, chest}


--constants
local runspeed = 8.5  -- run animation rate
local steptime = 40  -- how long legs stay extended during stride
local hangtime = 50  -- how long it takes for "gravity" to accelerate stride descent
local stride_top = 1.0  -- how high hips go during stride
local stride_bottom = -1.5  -- how low hips go during stride

-- variables
local firing = 0

--signals
local SIG_Restore = 1
local SIG_Walk = 2
local SIG_Aim = 4

function script.Create()
	StartThread(SmokeUnit, smokePiece)
	Turn(lforearm, x_axis, 0, 2)
	Turn(lshoulder, z_axis, - 0.9, 6)
	Turn(lshoulder, x_axis, - 0.8, 6)
	Turn(lforearm, y_axis, - 1, 5)
	Turn(halberd, z_axis, 0, 5)
end

local function Walk()
	Signal(SIG_Walk)
	SetSignalMask(SIG_Walk)

	Turn(chest, x_axis, 0.25, 0.08)

	local speedmod = 1
	local truespeed = runspeed
	while (true) do
		speedmod = (Spring.GetUnitRulesParam(unitID, "totalMoveSpeedChange") or 1)
		truespeed = runspeed * speedmod

		Turn(lshoulder, x_axis, -1.2, truespeed*0.2)
		Turn(hips, z_axis, 0.1, truespeed*0.05)
		Turn(hips, y_axis, -0.1, truespeed*0.05)
		Turn(rshoulder, x_axis, 0.5, truespeed*0.3)

		Turn(rthigh, x_axis, -1.5, truespeed*1)
		Turn(rshin, x_axis, 1.3, truespeed*1)
		Turn(rfoot, x_axis, -0.2, truespeed*0.25)

		Turn(lshin, x_axis, 0.2, truespeed*1)
		Turn(lthigh, x_axis, 1.2, truespeed*1)
		Turn(lfoot, x_axis, 0.2, truespeed*0.25)

		Move(hips, y_axis, stride_bottom, truespeed*3)

		WaitForMove(hips, y_axis)

		Move(hips, y_axis, stride_top, truespeed*4)

		WaitForMove(hips, y_axis)
		Sleep(hangtime)

		Move(hips, y_axis, stride_bottom, truespeed*1)

		WaitForTurn(rthigh, x_axis)

		Sleep(steptime)

		Turn(lshoulder, x_axis, -0.6, truespeed*0.2)
		Turn(hips, z_axis, -0.1, truespeed*0.05)
		Turn(hips, y_axis, 0.1, truespeed*0.05)
		Turn(rshoulder, x_axis, -0.5, truespeed*0.3)

		Turn(lthigh, x_axis, -1.5, truespeed*1)
		Turn(lshin, x_axis, 1.3, truespeed*1)
		Turn(lfoot, x_axis, -0.2, truespeed*0.25)

		Turn(rshin, x_axis, 0.2, truespeed*1)
		Turn(rthigh, x_axis, 1.2, truespeed*1)
		Turn(rfoot, x_axis, 0.2, truespeed*0.25)

		Move(hips, y_axis, stride_bottom, truespeed*3)

		WaitForMove(hips, y_axis)

		Move(hips, y_axis, stride_top, truespeed*4)

		WaitForMove(hips, y_axis)
		Sleep(hangtime)

		Move(hips, y_axis, stride_bottom, truespeed*1)

		WaitForTurn(lthigh, x_axis)

		Sleep(steptime)

	end
end

local function StopWalk()
	Signal(SIG_Walk)
	SetSignalMask(SIG_Walk)

	Move(hips, y_axis, 0, 3.0)
	Turn(chest, x_axis, 0, 1.5)

	Turn(hips, z_axis, 0, 1.5)
	Turn(hips, y_axis, 0, 1.5)
	Turn(rshoulder, x_axis, 0, 1.5)

	Turn(lthigh, x_axis, 0, 4)
	Turn(lshin, x_axis, 0, 4)
	Turn(lfoot, x_axis, 0, 4)

	Turn(rthigh, x_axis, 0, 4)
	Turn(rshin, x_axis, 0, 4)
	Turn(rfoot, x_axis, 0, 4)
end

function script.StartMoving()
	StartThread(Walk)
end

function script.StopMoving()
	StartThread(StopWalk)
end

local function RestoreAfterDelay()
	Signal(SIG_Restore)
	SetSignalMask(SIG_Restore)
	Sleep(2000)
	firing = 0
	Turn(chest, y_axis, 0, 3)
	Move(halberd, z_axis, 0, 2)
end

----[[
function script.QueryWeapon1() return head end

function script.AimFromWeapon1() return head end

function script.AimWeapon1(heading, pitch)
	
	Signal(SIG_Aim)
	SetSignalMask(SIG_Aim)
	--[[ Gun Hugger
	Turn(chest, y_axis, 1.1 + heading, 12)
	Turn(lshoulder, x_axis, -1 -pitch, 12)
	Turn(rshoulder, x_axis, -0.9 -pitch, 12)
	
	Turn(rshoulder, z_axis, 0.3, 9)
	Turn(lshoulder, z_axis, -0.3, 9)
	
	Turn(head, y_axis, -0.8, 9)
	Turn(head, x_axis, -pitch, 9)--]]
	
	-- Outstreched Arm
	firing = 1
	Turn(chest, y_axis, heading, 12)
	
	WaitForTurn(chest, y_axis)
	StartThread(RestoreAfterDelay)
	return true
end

function script.FireWeapon1()
	Turn(lforearm, x_axis, 0.4, 5)
	Turn(lshoulder, z_axis, - 0, 12)
	Turn(lshoulder, x_axis, - 0.7, 12)
	Turn(lforearm, y_axis, - 0.2, 10)
	Turn(halberd, z_axis, 1, 8)
	Move(halberd, z_axis, 15, 40)
	Sleep (800)
	Turn(lforearm, x_axis, 0, 2)
	Turn(lshoulder, z_axis, - 0.9, 6)
	Turn(lshoulder, x_axis, - 0.8, 6)
	Turn(lforearm, y_axis, - 1, 5)
	Turn(halberd, z_axis, 0, 5)
end

function script.Killed(recentDamage, maxHealth)
	local severity = recentDamage / maxHealth
	if (severity <= .25) then
		Explode(hips, sfxNone)
		Explode(head, sfxNone)
		Explode(chest, sfxNone)
		return 1 -- corpsetype
	elseif (severity <= .5) then
		Explode(hips, sfxNone)
		Explode(head, sfxNone)
		Explode(chest, sfxShatter)
		return 1 -- corpsetype
	else
		Explode(hips, sfxShatter)
		Explode(head, sfxSmoke + sfxFire)
		Explode(chest, sfxSmoke + sfxFire + sfxExplode)
		return 2 -- corpsetype
	end
end
