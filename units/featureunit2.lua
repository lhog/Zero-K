unitDef = {
  unitname               = [[featureunit2]],
  name                   = [[Feature attachment]],
  description            = [[Feature attachment]],
  acceleration           = 0.000000001,
  brakeRate              = 0.000000001,
  buildCostMetal         = 0,
  buildPic               = [[levelterra.png]],
  canAttack              = false,
  canGuard               = false,
  canMove                = true,
  canPatrol              = false,
  category               = [[STUPIDTARGET]],
  collisionVolumeOffsets = [[0 0 0]],
  collisionVolumeScales  = [[34 45 27]],
  collisionVolumeType    = [[box]],

  footprintX             = 1,
  footprintZ             = 1,

  idleAutoHeal           = 0,
  idleTime               = 1800,
  
  maxDamage              = 1,
  maxSlope               = 36,
  maxVelocity            = 2.5,
  maxWaterDepth          = 5000,
  minCloakDistance       = 0,
  movementClass          = [[TKBOT1]],  
  objectName             = [[sphere.s3o]],
  script                 = [[nullscript.lua]],  
  selfDestructCountdown  = 0,
  sightDistance          = 0,
  turnRate               = 2000,
  upright                = true,
}

return lowerkeys({ featureunit2 = unitDef })
