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


jumpPairs :: [Cmd] -> Int -> [Int] -> [(Int,Int)]
jumpPairs (BTJump : cmds) ctr (s : stack) = (ctr, s) : (s, ctr) :
                                            (jumpPairs cmds (succ ctr) stack)
jumpPairs (BTJump : cmds) ctr []    = (ctr, 0) : (jumpPairs cmds (succ ctr) [])
jumpPairs (FFJump : cmds) ctr stack = jumpPairs cmds (succ ctr) (ctr : stack)
jumpPairs (_      : cmds) ctr stack = jumpPairs cmds (succ ctr) stack
jumpPairs []              ctr []    = []


jumpMap :: [Cmd] -> Map Int Int
jumpMap cmds = Map.fromList (jumpPairs cmds 0 [])

jump :: Map Int Int -> Int -> Int
jump jumps ctr = (fromJust (Map.lookup ctr jumps))

transition :: Cmd -> Map Int Int -> State -> (State, Maybe Bool)
transition cmd jumps (State tape ptr ctr) = case cmd of
    Return -> (State tape              ptr       (succ ctr), Just (tape !! ptr))
    Toggle -> (State (toggle tape ptr) ptr       (succ ctr), Nothing)
    RShift -> (State tape              (ptr + 1) (succ ctr), Nothing)
    LShift -> (State tape              (ptr - 1) (succ ctr), Nothing)
    FFJump -> (State tape              ptr       target,     Nothing)
        where target = succ (if (tape !! ptr) then ctr else (jump jumps ctr))
    BTJump -> (State tape              ptr       target,     Nothing)
        where target = succ (if (tape !! ptr) then (jump jumps ctr) else ctr)

main = putStrLn "Compiled"



