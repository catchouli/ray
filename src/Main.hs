{-# LANGUAGE OverloadedStrings #-}

module Main where

import Window
import qualified SDL
import Control.Monad
import qualified Linear as L
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable (poke)
import qualified Foreign.Marshal.Array as M
import Foreign.C.Types
import Data.Bits

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

-- | Sphere shape
data Sphere = Sphere (L.V3 Float) Float (L.V3 Float) deriving (Show)

-- | Our scene definition
scene :: [Ray -> Maybe Hit]
scene = map raySphereIntersection
  [ Sphere (L.V3 0 0 (-5)) 2.5 (L.V3 1.0 0.0 0.0)
  , Sphere (L.V3 4.0 0 (-10.5)) 5 (L.V3 0.0 1.0 0.0)
  , Sphere (L.V3 0 14 (-5)) 10 (L.V3 1.0 1.0 1.0)
  ]

-- | Our lighting definition
light :: Light
light = Light (L.V3 (-5) (-2.5) 0) (L.V3 1.0 0.8 0.8)

-- | Intersect scene
intersectScene :: Ray -> [Ray -> Maybe Hit] -> Maybe Hit
intersectScene ray scene = foldr nearestHit Nothing hits
  where
    hits = map ($ ray) scene

-- | Nearest hit
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
      l = pos - origin
      tca = L.dot l dir
      d2 = L.dot l l - tca * tca
      radius2 = radius * radius
      thc = sqrt (radius2 - d2)
      t0 = tca - thc
      t1 = tca + thc
      valid = t0 > 0.0 || t1 > 0.0
      hitPos = dir L.^* (min t0 t1) + origin
      nrm = L.normalize (hitPos - pos)
      hit = Hit hitPos nrm (min t0 t1) col

-- | Shade
shade :: Hit -> Light -> L.V3 Float
shade (Hit hitPos hitNrm _ hitAlbedo) (Light lightPos lightCol) = col
  where
    lightDir = L.normalize (lightPos - hitPos)
    diffuse = L.dot hitNrm lightDir
    shadowRay = Ray (lightDir L.^* 0.1 + hitPos) (lightDir)
    shadowRayHit = intersectScene shadowRay scene
    shadow = case shadowRayHit of Just _ -> 0.25; _ -> 1.0
    col = (lightCol * hitAlbedo) L.^* (diffuse * shadow)

-- | Ray trace against our scene
trace :: Float -> Float -> L.V4 Float
trace x y = case col of
    Just (L.V3 r g b) -> L.V4 r g b 1
    Nothing -> L.V4 0 0 0 1
  where
    fov = pi / 6.0 :: Float
    z = -1.0 / tan fov
    rayDir = L.normalize $ L.V3 x y (-1)
    ray = Ray (L.V3 0 0 0) rayDir
    hit = intersectScene ray scene
    col = shade <$> hit <*> pure light

-- | Standard window width and height
width, height :: Num a => a
width = 500
height = 500

-- | Render out a test pattern and then draw it in a window
main = withTexture width height trace