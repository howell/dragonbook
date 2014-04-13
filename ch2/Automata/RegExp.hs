module RegExp where

import Automata
import qualified Data.Map as M
import Control.Monad.State

data RegExp = Sym Alphabet
            | Cat RegExp RegExp
            | Or RegExp RegExp
            | Kleene RegExp deriving (Eq, Show)

toNFA :: RegExp -> NFA
toNFA = flip evalState 0 . convert

type LabelState = State Int

getLabel :: LabelState Int
getLabel = do
        label <- get
        modify (+1)
        return label

convert :: RegExp -> LabelState NFA
convert (Sym s) = do
        newStart <- getLabel
        newStop  <- getLabel
        return NFA { start = newStart,
                     transitions = M.fromList [(newStart, [(s, [newStop])])],
                     accepting = [newStop] }
convert (Cat a b) = do
        nfaA <- convert a
        nfaB <- convert b
        let link   = [(Epsilon, [start nfaB])]
            acceptA = head $ accepting nfaA
            trans' = M.insertWith (++) acceptA link $ transitions nfaA
        return NFA { start = start nfaA,
                     transitions = M.union trans' (transitions nfaB),
                     accepting = accepting nfaB }
convert (Or a b) = do
        nfaA <- convert a
        nfaB <- convert b
        start' <- getLabel
        accept'  <- getLabel
        let acceptA = head $ accepting nfaA
            acceptB = head $ accepting nfaB
            startTrans = M.fromList [(newStart,
                                    [(Epsilon, [start nfaA, start nfaB])])]
            acceptTrans = M.fromList [(acceptA, [(Epsilon, [newStop])]),
                                      (acceptB, [(Epsilon, [newStop])])]
            newStart = M.fromList [(start',
                                  [(Epsilon, [start nfaA, start nfaB])])]
            newStop = M.fromList [(acceptA, [(Epsilon, [accept'])]),
                                  (acceptB, [(Epsilon, [accept'])])]
        return NFA { start = start',
                     transitions = M.unionsWith (++) [transitions nfaA,
                                                     transitions nfaB,
                                                     newStart,
                                                     newStop],
                     accepting = [accept'] }
convert (Kleene a) = do
        nfa <- convert a
        start' <- getLabel
        accept' <- getLabel
        let accept = head $ accepting nfa
            trans' = M.fromList [(start', [(Epsilon, [start nfa, accept'])]),
                                 (accept, [(Epsilon, [start nfa,accept'])])]
        return NFA { start = start',
                       transitions = M.unionWith (++) trans' (transitions nfa),
                       accepting = [accept'] }

