import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

counts :: String -> Map Char Int
counts s = foldr f Map.empty s 
    where g Nothing = Just 1
          g (Just c) = Just $ c+1
          f ch counts = Map.alter g ch counts

isUnique :: String -> Bool
isUnique [] = True
isUnique (c:cs) = (not $ elem c cs) && isUnique cs

isUnique' :: String -> Bool
isUnique' s = (Set.size $ Set.fromList s) == (length s)

isUnique'' :: String -> Bool
isUnique'' s = all (==1) (Map.elems $ counts s)

isPermutation :: String -> String -> Bool
isPermutation s1 s2 = counts s1 == counts s2

urlify :: String -> String
urlify [] = []
urlify (' ':cs) = "%20" ++ urlify cs
urlify (c:cs) = c:(urlify cs)

urlify' :: String -> String
urlify' s = concatMap f s 
    where f ' ' = "%20"
          f c = [c] 

isPalindromePermutation :: String -> Bool
isPalindromePermutation s = Map.size (Map.filter odd $ counts s) <= 1

isOneReplaceAway :: String -> String -> Bool
isOneReplaceAway s1 s2 = length s1 == length s2 && length (filter (uncurry (/=)) $ zip s1 s2) == 1

isSubsequence :: String -> String -> Bool
isSubsequence "" s2 = True
isSubsequence (c:cs) s2 = 
    let tailAfterFirstCh "" = Nothing
        tailAfterFirstCh (c':cs')
         | c == c' = Just cs'
         | otherwise = tailAfterFirstCh cs'
    in case tailAfterFirstCh s2 of 
        Just cs' -> isSubsequence cs cs'
        Nothing -> False

isOneRemoveOrInsertAway :: String -> String -> Bool
isOneRemoveOrInsertAway s1 s2 = 
    let l1 = length s1
        l2 = length s2
    in if abs (l2 - l1) == 1
         then let (s1', s2') = if (l1 < l2) then (s1, s2) else (s2, s1)
              in isSubsequence s1' s2'
         else False

isOneEditAway :: String -> String -> Bool
isOneEditAway s1 s2 = isOneReplaceAway s1 s2 || isOneRemoveOrInsertAway s1 s2

compressString :: String -> String
compressString = concatMap (\(ch, cnt) -> ch:(show cnt)) . reverse . foldl join []
    where join [] ch = [(ch, 1)]
          join l@((ch', cnt):rest) ch 
            | ch' == ch = (ch', cnt+1):rest
            | otherwise = (ch, 1):l
