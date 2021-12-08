-- Problem source: https://wiki.haskell.org/99_questions/1_to_10

-- Problem 1
myLast :: [a] -> a
myLast [] = error "no last element"
myLast [x] = x
myLast (_:x) = myLast x

-- Problem 2
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:x) = myButLast x

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt x y = x !! (y-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:x) = 1 + myLength x

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True 
isPalindrome [x] = True
isPalindrome xs = (head xs == last xs) && isPalindrome (init (tail xs))
                    
-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs
                    then compress xs
                    else x:compress xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack rest
pack [] = []

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- pack xs]