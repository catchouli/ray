{-# LANGUAGE OverloadedStrings #-}

module Main where

import Window
import qualified Linear as L
import Control.Applicative

-- | Ray
data Ray = Ray
  { origin :: L.V3 Float
  , dir :: L.V3 Float
  } deriving (Show)

-- | Hit
data Hit = Hit
  { hitPos :: L.V3 Float
  , normal :: L.V3 Float
  , dist :: Float
  , albedo :: L.V3 Float
  } deriving (Show)

-- | Light
data Light = Light
  { pos :: L.V3 Float
  , col :: L.V3 Float
  } deriving (Show)

-- | Sphere
data Sphere = Sphere (L.V3 Float) Float (L.V3 Float) deriving (Show)

-- | Our scene definition
scene :: [Ray -> Maybe Hit]
scene = map raySphereIntersection
  [ Sphere (L.V3 0 (-1) 5) 1 (L.V3 1.0 0.0 0.0)
  , Sphere (L.V3 2 0 7) 2 (L.V3 0.0 1.0 0.0)
  , Sphere (L.V3 0 (-52) (5)) 50 (L.V3 1.0 1.0 1.0)
  ]

-- | Our lighting definition
light :: Light
light = Light (L.V3 (-5) (2.5) 0) (L.V3 1.0 0.8 0.8)

-- | Intersect a ray with a scene definition to produce a ray hit
intersectScene :: Ray -> [Ray -> Maybe Hit] -> Maybe Hit
intersectScene ray scene = foldr nearestHit Nothing hits
  where
    hits = map ($ ray) scene

-- | Get the nearest of two hits
nearestHit :: Maybe Hit -> Maybe Hit -> Maybe Hit
nearestHit a@(Just (Hit _ _ t0 _)) b@(Just (Hit _ _ t1 _)) = if t0 < t1 then a else b
nearestHit a@(Just _) Nothing = a
nearestHit Nothing a@(Just _) = a
nearestHit _ _ = Nothing

-- | Ray sphere intersection
raySphereIntersection :: Sphere -> Ray -> Maybe Hit
raySphereIntersection (Sphere pos radius col) (Ray origin dir) =
  if d2 <= radius2 && valid then Just hit else Nothing
    where
      -- Calculate some of the parameters of our hit
      diff = pos - origin
      tca = L.dot diff dir
      d2 = L.dot diff diff - tca * tca
      radius2 = radius * radius
      thc = sqrt (radius2 - d2)
      -- Get the hit entry and exit
      t0 = tca - thc
      t1 = tca + thc
      -- The hit is valid if the entry or exit is in front of the ray origin
      valid = t0 > 0.0 || t1 > 0.0
      -- Calculate the hit position, min t0 t1 gives us the first hit along the ray from its origin
      hitPos = dir L.^* (min t0 t1) + origin
      -- Calculate the normal, assuming the hit is on the sphere's surface
      -- This makes sense as long as the ray origin wasn't inside the sphere
      nrm = L.normalize (hitPos - pos)
      -- The hit if our hit is valid
      hit = Hit hitPos nrm (min t0 t1) col

-- | Shade a ray hit
shade :: Hit -> Light -> L.V3 Float
shade (Hit hitPos hitNrm _ hitAlbedo) (Light lightPos lightCol) = col
  where
    -- Calculate direction to light from hit
    lightDir = L.normalize (lightPos - hitPos)
    -- Calculate basic cosine-falloff diffuse term
    diffuse = L.dot hitNrm lightDir
    -- Cast shadow ray and get whether this hit is in shadow for the light
    shadowRay = Ray (lightDir L.^* 0.1 + hitPos) (lightDir)
    shadowRayHit = intersectScene shadowRay scene
    shadow = case shadowRayHit of Just _ -> 0.25; _ -> 1.0
    -- Calculate final color
    col = (lightCol * hitAlbedo) L.^* (diffuse * shadow)

-- | Ray trace against our scene
trace :: Float -> Float -> L.V4 Float
trace x y = let L.V3 r g b = col in L.V4 r g b 1 
  where
    -- The target FOV
    fov = pi / 6.0
    -- The calculated ray direction for this pixel
    rayDir = L.normalize $ L.V3 x y (1.0 / tan fov)
    -- The ray for this pixel
    ray = Ray (L.V3 0 0 0) rayDir
    -- Intersect scene and obtain hit
    hit = intersectScene ray scene
    -- Sky color for background
    sky = L.lerp (1.0 - (y * 0.5 + 0.5)) (L.V3 0.78 0.78 0.7) (L.V3 0.3 0.4 0.5)
    -- Shade the scene if a hit was obtained
    sceneCol = shade <$> hit <*> pure light
    -- Return the sky color if nothing was hit
    Just col = sceneCol <|> Just sky

-- | Render out a test pattern and then draw it in a window
main = withTexture 500 500 trace