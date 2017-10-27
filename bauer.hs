import Data.Bool
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Cmd = Return
         | Toggle
         | RShift
         | LShift
         | FFJump
         | BTJump

data State = State [Bool] Int Int

toggle :: [Bool] -> Int -> [Bool]
toggle []       n = toggle [False] n
toggle (x : xs) 0 = (not x) : xs
toggle (x : xs) n = x : (toggle xs (n - 1))

extend :: [Bool] -> Int -> [Bool]
extend tape     0 = tape
extend []       n = False : (extend [] (n - 1))
extend (x : xs) n = extend xs (n - 1)


jumpPairs :: [Cmd] -> Int -> [Int] -> [(Int,Int)]
jumpPairs (BTJump : cmds) ctr (s : stack) = (ctr, s) : (s, ctr) :
                                            (jumpPairs cmds (succ ctr) stack)
jumpPairs (BTJump : cmds) ctr []    = (ctr, 0) : (jumpPairs cmds (succ ctr) [])
jumpPairs (FFJump : cmds) ctr stack = jumpPairs cmds (succ ctr) (ctr : stack)
jumpPairs (_      : cmds) ctr stack = jumpPairs cmds (succ ctr) stack
jumpPairs []              ctr (s : stack) = (s, 0) : (jumpPairs [] ctr stack)
jumpPairs []              ctr []    = []


jumpMap :: [Cmd] -> Map Int Int
jumpMap cmds = Map.fromList (jumpPairs cmds 0 [])

jump :: Map Int Int -> Int -> Int
jump jumps ctr = (fromJust (Map.lookup ctr jumps))

transition :: [Cmd] -> Map Int Int -> State -> (State, Maybe Bool)
transition cmds jumps (State tape ptr ctr) =
    case (cmds !! ctr) of
      Return -> (State extendedTape      ptr       (succ ctr), value)
          where extendedTape = extend tape ptr
                value        = Just (extendedTape !! ptr)
      Toggle -> (State (toggle tape ptr) ptr       (succ ctr), Nothing)
      RShift -> (State tape              (ptr + 1) (succ ctr), Nothing)
      LShift -> (State tape              (ptr - 1) (succ ctr), Nothing)
      FFJump -> (State tape              ptr       target,     Nothing)
          where target = succ (if (tape !! ptr) then ctr else (jump jumps ctr))
      BTJump -> (State tape              ptr       target,     Nothing)
          where target = succ (if (tape !! ptr) then (jump jumps ctr) else ctr)

machineInternal :: [Cmd] -> Map Int Int -> State -> [Maybe Bool]
machineInternal cmds jumps state@(State tape ptr ctr) =
    if (ctr < (length cmds))
       then let result = transition cmds jumps state
             in  (snd result) : (machineInternal cmds jumps (fst result))
       else []

machine :: [Cmd] -> [Bool] -> [Maybe Bool]
machine cmds tape = machineInternal cmds (jumpMap cmds) (State tape 0 0)

-- Awkward to match prior implementation
getProgram :: Int -> [Cmd]
getProgram 0 = [Return]
getProgram 1 = [Toggle]
getProgram 2 = [RShift]
getProgram 3 = [LShift]
getProgram 4 = [FFJump]
getProgram 5 = [BTJump]
getProgram n = (getProgram (quot n 6)) ++ (getProgram (rem n 6))


getTape :: Int -> [Bool]
getTape 0 = [False]
getTape 1 = [True]
getTape n = (getTape (quot n 2)) ++ (getTape (rem n 2))


computableFunc :: Int -> Int -> [Maybe Bool]
computableFunc n m = machine (getProgram n) (getTape m)


complement :: [Maybe Bool] -> [Maybe Bool]
complement []                  = []
complement ((Just True)  : xs) = Just False : (complement xs)
complement ((Just False) : xs) = Just True  : (complement xs)
complement (Nothing      : xs) = Nothing    : (complement xs)


nontotalFunc :: Int -> [Maybe Bool]
nontotalFunc n = complement (computableFunc n n)


bounded :: [Maybe Bool] -> Int -> Maybe Bool
bounded []                  _ = Nothing
bounded _                   0 = Nothing
bounded ((Just True)  : xs) n = Just False
bounded ((Just False) : xs) n = Just True
bounded (Nothing      : xs) n = (bounded xs (n - 1))


boundedFunc :: Int -> Int -> Maybe Bool
boundedFunc n k = bounded (nontotalFunc n) k



kleeneRecur :: [Bool] -> [[Bool]]
kleeneRecur parent =
    let k = (length parent) + 1
     in case (boundedFunc k k) of
          Just True  -> [parent ++ [True]]
          Just False -> [parent ++ [False]]
          Nothing    -> [parent ++ [True], parent ++ [False]]

kleeneConstruct :: [[Bool]] -> [[Bool]]
kleeneConstruct (p : parents)
  = let nodes = kleeneRecur p
     in nodes ++ (kleeneConstruct (parents ++ nodes))

kleeneTree :: [[Bool]]
kleeneTree = kleeneConstruct [[]]

showPath :: [Bool] -> String
showPath []           = ""
showPath (False : xs) = "0" ++ (showPath xs)
showPath (True  : xs) = "1" ++ (showPath xs)

showSeq :: [Maybe Bool] -> String
showSeq []           = ""
showSeq (Just False : xs) = "0" ++ (showSeq xs)
showSeq (Just True  : xs) = "1" ++ (showSeq xs)
showSeq (Nothing    : xs) = "?" ++ (showSeq xs)

showTree :: [[Bool]] -> String
showTree (path : paths) = (showPath path) ++ "\n" ++ (showTree paths)


main = putStrLn (showTree kleeneTree)



