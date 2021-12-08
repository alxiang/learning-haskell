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