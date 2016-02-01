data Barber = Barber {minutes :: Int} deriving (Show)
data State = State {barbers :: [Barber], time :: Int, line :: Int} deriving (Show)

run :: State -> (Barber, Int)
run (State barbers time line)
  | length free >= line = (free !! (line - 1), time + 1)
  | otherwise = run $ State barbers nextTime (line - length free)
  where nextTime = time + 1
        isFinished time barber = time `mod` minutes barber == 0
        free = filter (isFinished nextTime) barbers

main = print $ run $ State (fmap Barber [5, 10, 15]) 0 5
