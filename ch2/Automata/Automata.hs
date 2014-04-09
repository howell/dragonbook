module Automata where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Set as S
import Data.List (nub, sort)

data Symbol = A | B | Epsilon deriving (Show, Eq)

-- data DState = DState Transition
            -- | Dead

-- type Transition = Symbol -> DState

type StateLabel = Int

data NState = NState { onA :: [StateLabel],
                       onB :: [StateLabel],
                       eps :: [StateLabel] } deriving (Eq, Show)

type NFA = M.Map StateLabel NState

move :: NState -> Symbol -> [StateLabel]
move state A = onA state
move state B = onB state

eps1 :: NFA -> StateLabel -> [StateLabel]
eps1 nfa lbl
    | Just s <- M.lookup lbl nfa = nub . sort $ lbl : eps s
    | otherwise = error $ "unknown state " ++ show lbl

epsilonClosure :: NFA -> StateLabel -> [StateLabel]
epsilonClosure nfa start = S.toList $ go (S.singleton start)
    where
        go :: S.Set StateLabel -> S.Set StateLabel
        go s = let curs = S.toList s
                   new = S.fromList $ concatMap (eps1 nfa) curs in
                       if new == s then new else go new

moveNFA :: NFA -> [StateLabel] -> Symbol -> [StateLabel]
moveNFA nfa curr sym = nub $ concatMap helper curr >>= (epsilonClosure nfa)
    where
        helper :: StateLabel -> [StateLabel]
        helper l
            | Just s <- M.lookup l nfa = move s sym
            | otherwise = error $ "unknown state " ++ show l
       -- nub $ concatMap (epsilonClosure nfa (move nfa s))

runNFA :: NFA -> StateLabel -> [Symbol] -> [StateLabel]
runNFA nfa start syms = foldl' (moveNFA nfa) [start] syms

states :: [(StateLabel, NState)]
states = [(0, NState { onA = [0,1], onB = [0], eps = [] }),
          (1, NState { onA = [], onB = [2], eps = [] }),
          (2, NState { onA = [], onB = [3], eps = []}),
          (3, NState { onA = [], onB = [], eps = [] })]

tnfa :: NFA
tnfa = M.fromList states

-- type MyState = State (NFA, StateLabel) [StateLabel]

-- ec :: MyState -- (NFA, StateLabel)    -- the NFA and current state
-- ec = do
        -- (seen, (nfa, cur)) <- get
        -- put (cur : seen)
        -- return []

-- epsilonClosure :: NFA -> NState -> [StateLabel]
-- epsilonClosure nfa state = foldl' helper (eps state) []
    -- where
        -- helper :: [StateLabel] -> StateLabel -> [StateLabel]
        -- helper acc x = undefined

-- type DFA = [(StateLabel, DState)]

-- newtype DState = Maybe ((Maybe StateLabel) (Maybe StateLabel))
                 -- Nothing represents the dead state
                 -- left Maybe is the A transition
                 -- right Maybe is the B transition

-- data DFA = DFA { start   :: StateLabel,
                 -- states  :: [(StateLabel, DState)],
                 -- accepts :: [StateLabel] } deriving (Show, Eq)

-- transition :: DFA -> DState -> Symbol -> DState
-- transition _ s Epsilon = s
-- transition dfa state A = case a of
                                    -- Nothing -> Dead
                                    -- Just s ->
-- transition dfa (DState _ b) B = undefined

-- moveDFA :: DFA -> DState -> Alphabet -> DState
-- moveDFA _ Dead           _       = Dead
-- moveDFA _ s              Epsilon = s
-- moveDFA dfa (DState _ a _) A       = a
-- moveDFA (DState _ _ b) B       = b

-- runDFA :: DFA -> [Alphabet] -> DState
-- runDFA d = foldl' moveDFA (start d)
