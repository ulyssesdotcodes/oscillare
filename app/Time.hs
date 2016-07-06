module Time where

type Time = Rational

type Arc = (Time, Time)

type Event a = (Arc, Arc, a)

sam :: Time -> Time
sam = fromIntegral . floor

nextSam :: Time -> Time
nextSam = (1+) . sam

cyclePos :: Time -> Time
cyclePos t = t - sam t
