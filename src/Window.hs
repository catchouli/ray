{-# LANGUAGE OverloadedStrings #-}

module Window
  ( withWindow
  , withTexture
  , clamp
  )
where

import qualified SDL
import Control.Monad
import qualified Linear as L
import Data.Array.Storable
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Ptr
import Foreign.Storable (poke)
import qualified Foreign.Marshal.Array as M
import Foreign.C.Types
import Data.Bits
import Control.Monad.ST (runST)

-- | Clamp function
clamp :: (Num a, Ord a) => a -> a -> a -> a
clamp a b = max a . min b

-- | Renders to the given array using the given function
render :: Int -> Int -> (Float -> Float -> L.V4 Float) -> Ptr CUInt -> IO ()
render width height f buf = forM_ [0..height-1] $ \y -> do 
  forM_ [0..width-1] $ \x -> do 
    -- Get pointer to pixel
    let idx = width * y + x
    let pix = plusPtr buf (idx*4)
    -- Get (-1..1) xy coordinate
    let normalise max val = val / max * 2.0 - 1.0 :: Float
    let xf = normalise (fromIntegral width) (fromIntegral x)
    let yf = normalise (fromIntegral height) (fromIntegral y)
    -- Calculate color
    let col = f xf yf
    -- Store
    poke pix (encodeColor col)

-- | Convert RGBA to CUInt
encodeColor :: L.V4 Float -> CUInt
encodeColor (L.V4 r g b a) =
  let v4ToInt = truncate . (255*) . clamp 0.0 1.0
      ri = v4ToInt r
      gi = v4ToInt g
      bi = v4ToInt b
      ai = v4ToInt a
      pix = (shiftL ri 24) .|. (shiftL gi 16) .|. (shiftL bi 8) .|. ai
  in CUInt pix

-- | Uploads the given array to a texture
upload :: SDL.Texture -> Int -> Int -> Ptr CUInt -> IO ()
upload texture width height source = do
  (lockedPtr, pitch) <- SDL.lockTexture texture Nothing
  let dest = castPtr lockedPtr :: Ptr CUInt
  M.copyArray dest source (width * height)
  SDL.unlockTexture texture

-- | Create a window and run the given IO action
withWindow :: Int -> Int -> (SDL.Window -> SDL.Renderer -> IO ()) -> IO ()
withWindow width height action = do
  -- Initialise all SDL subsystems
  SDL.initializeAll

  -- Create SDL window
  window <- SDL.createWindow "boop" SDL.defaultWindow { SDL.windowInitialSize = L.V2 (fromIntegral width) (fromIntegral height) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- Do user action
  action window renderer

  -- Clean up
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

-- | Timing info
data TimingInfo = TimingInfo
  { time :: Float -- ^ The current time
  , frames :: Int -- ^ Frames that have passed
  , lastFpsUpdate :: Float -- ^ Last time the FPS was updated
  , fps :: Int -- ^ FPS at that time
  }

-- | Update timing
updateTiming :: TimingInfo -> Float -> TimingInfo
updateTiming timingInfo newTime =
  if newTime - lastFpsUpdate timingInfo >= 1.0
    then timingInfo { time = newTime, lastFpsUpdate = newTime, fps = frames timingInfo, frames = 0 }
    else timingInfo { time = newTime, frames = frames timingInfo + 1 }

-- | Create texture, run action on it, and then display it in a window
withTexture :: Int -> Int -> (Float -> Float -> L.V4 Float) -> IO ()
withTexture width height f = withWindow width height $ \window renderer -> do
  -- Create texture
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (L.V2 (fromIntegral width) (fromIntegral height))

  -- Create pixel buffer
  pixelBuffer <- newArray (0, width * height - 1) 0 :: IO (StorableArray Int CUInt)

  -- Render image
  withStorableArray pixelBuffer (render width height f)

  -- Main loop
  let loop timingInfo = do
        -- Poll window events so it doesn't just freeze up
        SDL.pollEvents

        -- Upload texture
        withStorableArray pixelBuffer (upload texture width height)

        -- Copy texture to window
        SDL.copy renderer texture Nothing Nothing

        -- Present image
        SDL.present renderer

        -- Update timing info
        newTimingInfo <- updateTiming timingInfo <$> SDL.time

        -- Print fps if changed
        when (lastFpsUpdate timingInfo /= lastFpsUpdate newTimingInfo) $
          putStrLn $ "FPS: " ++ show (fps newTimingInfo)

        -- Loop until esc is pressed
        keyState <- SDL.getKeyboardState
        unless (keyState SDL.ScancodeEscape) (loop newTimingInfo)

  time <- SDL.time
  loop (TimingInfo time 0 time 0)

  SDL.destroyTexture texture
  