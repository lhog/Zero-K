model = {
	--radius = 25.0,
	--height = 40,
	--tex1 = "Jeffy_Diffuse+TeamColor.dds",
	tex1 = "Jeffy_Color.png",
	--tex2 = "armtech_tex2.dds",
	--tex2 = "armtech_tex2.dds",
	midpos = {0, 0, 0},
	--rotAxisSigns = {-1, -1, -1}
	pbr = {
		flipUV = true, --flip second component of UV map
		fastGamma = true, --default is false i.e. more precise method
		tbnReortho = true, -- Re-orthogonalize TBN matrix using Gram-Schmidt process. Might behave differently depending on "hasTangents". Default is true.
		pbrWorkflow = "metallic", -- either "metallic" or "specular". "specular" is not yet implemented
		-- PBR shader will sample a certain number of supplied textures.
		-- provide a recipe to map samples to PBR inputs
		baseColorMap = {
			scale = {1.0, 1.0, 1.0, 1.0}, -- acts as a color if tex unit is unused or as a multiplier if tex unit is present. Defaults to vec4(1.0).
			get = "[0].rgba", -- take sample from first texture unit in array.
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to true.
		},
		normalMap = {
			hasTangents = true, --you somehow must know if the import of the model puts tangents and bitangents to gl_MultiTexCoord[5,6]
			scale = 1.0, -- scale for Red/X/tangent and Green/Y/bitangent parts of normal sampled from normalMapTex. Defaults to 1.0
			get = "[1].rgb",
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to false, as normals should be saved in linear RGB.
		},
		parallaxMap = { -- parallax occlusion mapping. Will be ignored if normalMap.hasTangents == false
			fast = false, --always test if fast is good enough and only switch to "precise" if quality is bad. fast=true is simple parallax, fast=false is parallax occlusion mapping
			scale = 0.01, --if you set this up and your model texturing (and everything else) looks off, try to divide scale by 10 and then find out the best value iteratively
			--get = "[1].a", -- expects linear bump map as input
			get = nil,
			gammaCorrection = false, -- don't do. A is always linear
		},
		emissiveMap = {
			scale = {1.0, 1.0, 1.0}, -- acts as a color if tex unit is unused or as a multiplier if tex unit is present. Defaults to vec3(1.0).
			get = "[2].rgb",
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to true.
		},
		occlusionMap = {
			strength = 1.0, --multiplier in case occlusionMap is present. Does NOT act as a texture stand-in
			get = "[3].r",
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to false, as ao should be saved in linear RGB.
		},
		roughnessMap = {
			scale = 1.0, --acts as a multiplier or a base value (if get is nil)
			get = "[3].g",
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to false, as roughness should be saved in linear RGB.
		},
		metallicMap = {
			scale = 1.0, --acts as a multiplier or a base value (if get is nil)
			get = "[3].b",
			gammaCorrection = true, -- do sRGB to RGB in-shader translation. Defaults to false, as roughness should be saved in linear RGB.
		},
		iblMap = {
			scale = {0.9, 0.3}, --{diffuse, specular} IBL scale. Acts as a multiplier or a base value (if get is nil)
			get = true, -- to generate GET_IBLMAP definition
			lod = true, -- can be nil, a number, or true for auto
			gammaCorrection = false, -- do sRGB to RGB in-shader translation. Defaults to false, as roughness should be saved in linear RGB.
		},
		gammaCorrection = true, -- do gamma correction (RGB-->sRGB) on the final color.
		texUnits = { -- substitute values
			--["TEX0"] = "Jeffy_Diffuse+TeamColor.dds",
			["TEX0"] = "Jeffy_Color.png",
			["TEX1"] = "Jeffy_Normal_Bump1.png",
			["TEX2"] = "Jeffy_Emissive.png",
			--["TEX2"] = "Jeffy_Diffuse+TeamColor.dds",
			["TEX3"] = "Jeffy_ORM.png",
			["BRDF"] = "brdflutTex.png"
		}
	},
}
return model