module Tempo where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Clock

data Tempo = Tempo { at :: UTCTime, beat :: Double, cps :: Double }

newTempoMVar now = newMVar (Tempo now 0 0.5)

tempoMVar :: IO (MVar (Tempo))
tempoMVar = do now <-getCurrentTime
               mv <- newTempoMVar now
               forkIO $ clocked $ f mv
               return mv
            where f mv change _ = do swapMVar mv change
                                     return ()

beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = cps t * delta
               return $ beat t + beatDelta

clocked :: (Tempo -> Int -> IO ()) -> IO ()
clocked = clockedTick 1

clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback =
  do now <- getCurrentTime
     mTempo <- newTempoMVar now
     t <- readMVar mTempo
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = cps t * delta
         nowBeat = beat t + beatDelta
         nextTick = ceiling (nowBeat * (fromIntegral tpb))
     loop mTempo nextTick
  where loop mTempo tick =
          do tempo <- readMVar mTempo
             tick' <- doTick tempo tick
             loop mTempo tick'
        doTick tempo tick =
          do now <- getCurrentTime
             let tps = (fromIntegral tpb) * (cps tempo)
                 delta = realToFrac $ diffUTCTime now (at tempo)
                 actualTick = ((fromIntegral tpb) * beat tempo) + (tps * delta)
                 tickDelta = (fromIntegral tick) - actualTick
                 delay = tickDelta/tps
             threadDelay $ floor (delay * 1000000)
             callback tempo tick
             return $ tick + 1
