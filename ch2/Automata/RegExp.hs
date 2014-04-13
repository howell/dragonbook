module RegExp where

import Automata
import qualified Data.Map as M

data RegExp = Sym Alphabet
            | Cat RegExp RegExp
            | Or RegExp RegExp
            | Kleene RegExp deriving (Eq, Show)

toNFA :: RegExp -> NFA
toNFA (Sym s) = NFA { start = 0,
                      transitions = M.fromList [(0, [(s, [1])])],
                      accepting = [1] }
toNFA _ = undefined

convert :: Int -> RegExp -> (NFA, Int)
convert x (Sym s) = (NFA { start = x,
                           transitions = M.fromList [(x, [(s, [x + 1])])],
                           accepting = [x + 1] }, x + 2)
convert x (Cat a b) =
        let (nfaA, x') = convert x a
            (nfaB, x'') = convert x' b
            link = [(Epsilon, [start nfaB])]
            trans' = M.insertWith (++) (head (accepting nfaA)) link $ transitions nfaA
            in
                (NFA { start = start nfaA,
                     transitions = M.union trans' (transitions nfaB),
                     accepting = accepting nfaB }, x'')
convert x (Or a b) =
        let (nfaA, x') = convert x a
            (nfaB, x'') = convert x' b
            acceptA = head $ accepting nfaA
            acceptB = head $ accepting nfaB
            newStart = M.fromList [(x'', [(Epsilon, [start nfaA, start nfaB])])]
            newStop = M.fromList [(acceptA, [(Epsilon, [x'' + 1])]),
                                  (acceptB, [(Epsilon, [x''+ 1])])]
            in
                (NFA { start = x'',
                       transitions = M.unionsWith (++) [transitions nfaA,
                                                        transitions nfaB,
                                                        newStart,
                                                        newStop],
                       accepting = [x'' + 1] }, x'' + 2)
convert x (Kleene a) =
        let (nfa, x') = convert x a
            prevAccept = head $ accepting nfa
            trans' = M.fromList [(x', [(Epsilon, [start nfa, x' + 1])]),
                                 (prevAccept, [(Epsilon, [start nfa, x' + 1])])]
            in
                (NFA { start = x',
                       transitions = M.unionWith (++) trans' (transitions nfa),
                       accepting = [x' + 1] }, x' + 2)

