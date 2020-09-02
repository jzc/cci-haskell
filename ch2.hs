removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as) = a : (removeDuplicates $ remove a as)
    where remove x [] = []
          remove x (a:as)
            | a == x = remove x as
            | otherwise = a:(remove x as)

kthFromFront :: Int -> [a] -> Maybe a
kthFromFront k _ 
    | k < 1 = Nothing
kthFromFront _ [] = Nothing
kthFromFront 1 (a:_) = Just a
kthFromFront k (a:as) = kthFromFront (k-1) as

reverse' :: [a] -> [a]
reverse' = foldl (\x a -> a : x) []

kthFromLast :: Int -> [a] -> Maybe a
kthFromLast n as = kthFromFront n $ reverse' as 

partition :: Ord a => a -> [a] -> [a]
partition a as = lt ++ eq ++ gt 
    where lt = filter (<a) as
          eq = filter (==a) as
          gt = filter (>a) as

listToNumber :: [Int] -> Int
listToNumber = listToNumberR . reverse'

listToNumberR :: [Int] -> Int
listToNumberR = snd . foldl (\(b, acc) i -> (b*10, acc+i*b)) (1, 0)

numberToList :: Int -> [Int]
numberToList n | n < 0 = numberToList (-n)
numberToList 0 = [0]
numberToList n = go n []
    where go x xs
            | x == 0 = xs
            | otherwise =  go (x `div` 10) (x `mod` 10 : xs)

numberToListR :: Int -> [Int]
numberToListR = reverse' . numberToList

isPalindrome :: Eq a => [a] -> Bool
isPalindrome as = as == reverse' as