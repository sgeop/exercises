import Data.Set

-- data types
type Board = (Set Loc)

data Dir = N | S | E | W deriving (Show)
data Color = Bl | Wh

data Loc = Loc Int Int deriving (Show, Eq)
instance Ord Loc where
  compare (Loc x1 y1) (Loc x2 y2)
    | x1 == x2 = compare y1 y2
    | otherwise = compare x1 x2

data Ant = Ant {dir :: Dir, loc :: Loc} deriving (Show)
data State = State {ant :: Ant, board:: Board} deriving (Show)

turnRight :: Dir -> Dir
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Dir -> Dir
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

nextLoc :: Ant -> Loc
nextLoc (Ant N (Loc x y)) = Loc x (y + 1)
nextLoc (Ant S (Loc x y)) = Loc x (y - 1)
nextLoc (Ant E (Loc x y)) = Loc (x + 1) y
nextLoc (Ant W (Loc x y)) = Loc (x - 1) y

turn :: State -> State
turn (State (Ant dir loc) board) = State (Ant (turnFunc dir) loc) board where
  turnFunc
    | member loc board = turnLeft 
    | otherwise = turnRight
  
flipColor :: State -> State
flipColor (State ant board)
  | member l board = State ant (delete l board)
  | otherwise = State ant (insert l board)
    where l = loc ant

step :: State -> State
step (State ant board) = State (ant { loc = nextLoc ant }) board

next :: State -> State
next = flipColor . turn . step

run :: State -> Int -> IO ()
run state 0 = return ()
run state numMoves  = do
  printBoard 10 10 state
  run (next state) (numMoves - 1)


-- functions to print out board state --

antChar :: Dir -> Char
antChar N = '^'
antChar S = 'V'
antChar W = '<'
antChar E = '>'

locChar :: Loc -> State -> Char
locChar l (State ant board)
  | l == loc ant = antChar $ dir ant
  | member l board = '#'
  | otherwise = ' '

printRow :: State -> [Loc] -> IO () 
printRow st = f
  where
    f [] = return ()
    f (x:xs) = do 
      putStr . show $ locChar x st
      f xs

printBoard n y state
  | y <= (-n) = return ()
  | otherwise = 
    let row = fmap (`Loc` y) [-n .. n] 
    in do
      print ""
      printRow state row
      printBoard n (y - 1) state
  
  
main = do
  let state = State (Ant N (Loc 0 0 )) empty
  run state 10
