--problem 1


myLast:: [a] -> a
myLast [] = error "No end for empty list!"
myLast (x:xs) | null(xs) = x
              | otherwise = myLast xs

myButLast:: [a] -> a
myButLast [] = error "No end for empty list!"
myButLast (x:xs) | length xs == 1 = x
                 | otherwise = myButLast xs

--elementAt:: [a] -> Int -> a
--elementAt [] n = error "No elements could be returned in empty list"
elementAt (x:xs) n | length (x:xs) < n = error "List is shorter than n" 
                   | n == 1 = x
                   | otherwise = elementAt xs (n-1)

myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) | null xs = 1
                | otherwise = 1 + myLength xs

isPalindrome xs | null xs = True
                | myLength xs == 1 = True
                | head xs == last xs = isPalindrome (tail(init xs))
                | otherwise = False
