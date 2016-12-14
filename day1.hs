import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

input = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"

data Direction = TurnLeft | TurnRight
  deriving (Read, Show, Eq)

data Instruction = Instruction { 
  direction :: Direction,
  distance :: Int
} deriving (Read, Show, Eq)

data Cardinal = North | East | South | West
  deriving (Read, Show, Eq)

instruction :: String -> Instruction
instruction ('R':n) = Instruction TurnRight (read n)
instruction ('L':n) = Instruction TurnLeft (read n)

clean :: String -> String
clean (x:xs) 
  | x==',' = clean xs
  | x==' ' = clean xs
  | otherwise = x:xs

instructions = map (\t -> instruction (clean (T.unpack t))) (T.split (\c -> c ==',') (T.pack input))

turn :: Cardinal -> Instruction -> Cardinal
turn North Instruction {direction=TurnLeft} = West
turn North Instruction {direction=TurnRight} = East
turn South Instruction {direction=TurnLeft} = East
turn South Instruction {direction=TurnRight} = West
turn East Instruction {direction=TurnLeft} = North
turn East Instruction {direction=TurnRight} = South
turn West Instruction {direction=TurnLeft} = South
turn West Instruction {direction=TurnRight} = North

data Steps = Steps { vertical:: Int, horizontal:: Int }
  deriving (Read, Show, Eq)
emptySteps = Steps { vertical = 0, horizontal = 0}
hashSteps steps = (vertical steps)*1000 + (horizontal steps)
stepdistance steps = (abs (vertical steps)) + (abs (horizontal steps))

addSteps :: Cardinal -> Instruction -> Steps -> Steps
addSteps facing instr Steps {vertical=v, horizontal=h}
  | turn facing instr == North = Steps {
      vertical = v + distance instr,
      horizontal = h }
  | turn facing instr == East = Steps {
      vertical = v,
      horizontal = h + distance instr }
  | turn facing instr == West = Steps {
      vertical = v,
      horizontal = h - distance instr }
  | turn facing instr == South = Steps {
      vertical = v - distance instr,
      horizontal = h }
      
addStepsa facing instr Steps {vertical=v, horizontal=h}
  | newFacing == North = Steps {
      vertical = v + distance instr,
      horizontal = h }
  | newFacing == East = Steps {
      vertical = v,
      horizontal = h + distance instr }
  | newFacing == West = Steps {
      vertical = v,
      horizontal = h - distance instr }
  | newFacing == South = Steps {
      vertical = v - distance instr,
      horizontal = h }
  where newFacing = turn facing instr

data State = State {
  stepsSoFar :: Steps,
  facing :: Cardinal,
  seen :: Map Int Bool,
  answer :: Int
} deriving (Read, Show, Eq)
emptyState = State { stepsSoFar = emptySteps, facing = North, seen = Map.empty, answer = 0}

foldl' f init xs = foldl (\x y -> f y x) init xs

foldState instr state = 
  let newSteps = addSteps (facing state) instr (stepsSoFar state) in
  let newHash = hashSteps newSteps in
  State {
    stepsSoFar = newSteps,
    facing = turn (facing state) instr,
    seen = Map.insert newHash True (seen state),
    answer = if (answer state) == 0 && (Map.findWithDefault False newHash (seen state)) then stepdistance newSteps else answer state
  }
resultState = foldl' foldState emptyState instructions

