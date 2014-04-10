module Automata where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub)
import Data.Maybe (fromMaybe)

data Alphabet = Symbol Char
              | Epsilon deriving (Eq, Show, Ord)

type StateLabel = Int

type Transitions = [(Alphabet, [StateLabel])]

data NFA = NFA { start :: StateLabel,
                 transitions :: M.Map StateLabel Transitions,
                 accepting :: [StateLabel]
               } deriving (Eq, Show)

move :: NFA -> Alphabet -> StateLabel -> [StateLabel]
move nfa s current
    | Just ts <- M.lookup current (transitions nfa) = fromMaybe [] $ lookup s ts
    | otherwise = error $ "unknown state " ++ show current

epsilonClosure :: NFA -> StateLabel -> [StateLabel]
epsilonClosure nfa start = S.toList $ go (S.singleton start)
    where
        go :: S.Set StateLabel -> S.Set StateLabel
        go s = let curs = S.toList s
                   neighbors = S.fromList $ concatMap (move nfa Epsilon) curs
                   new = S.union s neighbors
                   in
                       if new == s then new else go new

moveNFA :: NFA -> [StateLabel] -> Alphabet -> [StateLabel]
moveNFA nfa curr s = nub $ concatMap (move nfa s) curr >>= epsilonClosure nfa

runNFA :: NFA -> [Alphabet] -> [StateLabel]
runNFA nfa syms = foldl' (moveNFA nfa) [start nfa] syms

accepts :: NFA -> [Alphabet] -> Bool
nfa `accepts` s = any (`elem` accepting nfa) $ runNFA nfa s

testTrans :: M.Map StateLabel Transitions
testTrans = M.fromList [(0, [(Symbol 'a', [0,1]), (Symbol 'b', [0])]),
                        (1, [(Symbol 'b', [2])]),
                        (2, [(Symbol 'b', [3])]),
                        (3, [])]

tnfa :: NFA
tnfa = NFA { start = 0,
             transitions = testTrans,
             accepting = [3]
           }

