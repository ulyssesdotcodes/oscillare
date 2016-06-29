{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics where

import Control.Concurrent
import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Graphics.Luminance
import Sound.OSC.Core

import qualified Control.Monad.Error.Class as CME
import qualified Graphics.UI.GLFW as G

import Osc

data Error = ErrorStage StageError | ErrorProgram ProgramError deriving (Eq, Show)
instance HasStageError Error where
  fromStageError = ErrorStage

instance HasProgramError Error where
  fromProgramError = ErrorProgram

vertices :: [V 2 Float]
vertices =
  [
    vec2 (-1) (-1)
  , vec2 (-1) 1
  , vec2 1 (-1)

  , vec2 1 (-1)
  , vec2 1 1
  , vec2 (-1) 1
  ]


run :: MVar String -> G.Window -> IO ()
run fragV mw = do
  vertexShader <- createStageFromFile "app/shaders/passthrough.vert" VertexShader
  frag <- takeMVar fragV
  createFragment mw vertexShader fragV frag

createFragment :: G.Window -> ResourceT (ExceptT Error IO) Stage -> MVar String -> String -> IO ()
createFragment mw vertexShader fragV frag = do
  fragmentShader <- createStageFromFile frag FragmentShader
  (x::Either Error String) <- runExceptT . runResourceT $ do
    p <- sequenceA [vertexShader, fragmentShader] >>= createProgram_
    quad <- createGeometry vertices Nothing Triangle
    liftIO $ loop mw fragV p quad
  either (print . show) (createFragment mw vertexShader fragV) x

loop :: G.Window -> MVar String -> Program () -> Geometry -> IO String
loop window fragV prog geo =
  let
    rcmd = renderCmd Nothing False
    sbp geo = pureDraw $ rcmd geo
    fbb prog geo = defaultFrameCmd [ShadingCmd prog (\a -> mempty) [sbp geo]]
  in
    do
      void . draw $ fbb prog geo
      G.swapBuffers window
      mf <- tryTakeMVar fragV
      maybe (loop window fragV prog geo) return mf

createStageFromFile :: (CME.MonadError e m, MonadResource m, HasStageError e) => FilePath -> StageType -> IO (m Stage)
createStageFromFile f s =
  readFile f >>= \shader -> return $ createStage s shader
