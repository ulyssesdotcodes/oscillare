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
import Data.Time.Clock
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

data ShaderProgram =
  ShaderProgram { file :: String
                , uniformNames :: [String]
                , uniformVals :: [Float]
                }

data UniformTypes =
  Float
  | Int

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


run :: MVar ShaderProgram -> G.Window -> IO ()
run fragV mw = do
  vertexShader <- createStageFromFile "app/shaders/passthrough.vert" VertexShader
  frag <- takeMVar fragV
  createFragment mw vertexShader fragV frag

createFragment :: G.Window -> ResourceT (ExceptT Error IO) Stage -> MVar ShaderProgram -> ShaderProgram -> IO ()
createFragment mw vertexShader fragV frag =
  let
    stagesR = transResourceT (\vShader -> (vShader, createStageFromFile (file frag) FragmentShader)) vertexShader
    createProgR (vertexS, fragmentS) = createProgram [vertexS, fragmentS] $ \uni -> do
        u1 <- uni (UniformName ((uniformNames frag) !! 0))
        pure [u1]
    progR = transResourceT createProgR stagesR
  in do
    time <- fmap utctDayTime getCurrentTime
    (x::Either Error ShaderProgram) <- runExceptT . runResourceT $ do
      p <- progR
      quad <- createGeometry vertices Nothing Triangle
      liftIO $ loop mw fragV frag p quad
    either (print . show) (createFragment mw vertexShader fragV) x

loop :: G.Window -> MVar ShaderProgram -> ShaderProgram -> Program [U Float] -> Geometry -> IO ShaderProgram
loop window fragV sp prog geo =
  let
    rcmd = renderCmd Nothing False
    sbp geo = pureDraw  $ rcmd geo
    fbb prog geo = defaultFrameCmd [ShadingCmd prog (\a -> mempty) [sbp geo]]
    sp' time = ShaderProgram (file sp) (uniformNames sp) [realToFrac time]
  in
    do
      time <- fmap utctDayTime getCurrentTime
      updateUniforms prog (\us -> foldr1 (<>) (zipWith (.=) us (uniformVals (sp' time))))
      void . draw $ fbb prog geo
      G.swapBuffers window
      mf <- tryTakeMVar fragV
      maybe (loop window fragV (sp' time) prog geo) return mf

createStageFromFile :: (CME.MonadError e m, MonadResource m, HasStageError e) => FilePath -> StageType -> IO (m Stage)
createStageFromFile f s =
  readFile f >>= \shader -> return $ createStage s shader
