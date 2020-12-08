module Advent.Day08 where

import qualified Data.Set as Set

part1 :: String -> Int
part1 = sAcc . pState . runProgram . loadProgram

part2 :: String -> Int
part2 = snd . head . filter fst . swapAndRun [] . loadInstr

data Op = OpNop | OpAcc | OpJmp deriving (Show, Eq)

data Instr = Instr Op Int deriving (Show)

data State = State {sAdd :: Int, sAcc :: Int} deriving (Show)

data Program = Program {pState :: State, pExecuted :: Set.Set Int, pSuccess :: Bool, pMemory :: [Instr]} deriving (Show)

parseInstr :: String -> Instr
parseInstr (op1 : op2 : op3 : ' ' : sgn : mag)
  | op == "nop" = Instr OpNop value
  | op == "acc" = Instr OpAcc value
  | op == "jmp" = Instr OpJmp value
  | otherwise = error "Invalid instruction"
  where
    op = [op1, op2, op3]
    value = if sgn == '+' then read mag else (- (read mag))
parseInstr _ = error "Invalid instruction"

newProgram :: [Instr] -> Program
newProgram = Program (State 0 0) Set.empty False

loadInstr :: String -> [Instr]
loadInstr = map parseInstr . lines

loadProgram :: String -> Program
loadProgram = newProgram . loadInstr

execInstr :: Instr -> State -> State
execInstr (Instr OpNop _) (State add acc) = State (add + 1) acc
execInstr (Instr OpAcc inc) (State add acc) = State (add + 1) (acc + inc)
execInstr (Instr OpJmp inc) (State add acc) = State (add + inc) acc

runProgram :: Program -> Program
runProgram (Program state@(State add _) exec _ mem) =
  let newState@(State newAdd _) = execInstr (mem !! add) state
      infinite = Set.member newAdd exec
      success = newAdd >= length mem
      result = Program newState (Set.insert add exec) success mem
   in if infinite || success then result else runProgram result

swapInstr :: Instr -> Instr
swapInstr (Instr OpNop value) = Instr OpJmp value
swapInstr (Instr OpJmp value) = Instr OpNop value
swapInstr instr = instr

swapAndRun :: [Instr] -> [Instr] -> [(Bool, Int)]
swapAndRun _ [] = []
swapAndRun xs (y@(Instr OpAcc _) : ys) = (False, 0) : swapAndRun (xs ++ [y]) ys
swapAndRun xs (y : ys) =
  let p = runProgram (newProgram (xs ++ [swapInstr y] ++ ys))
   in (pSuccess p, sAcc (pState p)) : swapAndRun (xs ++ [y]) ys
