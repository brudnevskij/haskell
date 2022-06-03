import Data.Char
-- head [42,13,666] -> 42
-- tail [1,43,7,9] -> []
-- null [] -> True
-- if null x then 42 else 13

-- one [42] -> True
-- one [] -> False
-- one [2,4] -> False

--1,2],[4,7],[6,7],...]

sq x = x*x

one x = if null(tail x) then True else False

qsort x | null x == False = qsort[i| i<-x, i < head x]++[i|i<-x, i == head x] ++ qsort[i| i<-x, i > head x]
        | otherwise = x

myZip x y | not (null x) && not (null y) = (head x:head y:[]):[] ++ myZip (tail x) (tail y) 
          | otherwise = []  

drop'::Int -> [a] -> [a]
drop' n (x:xs) | n > 0 = drop' (n - 1) xs
               | n < 0 = []
               | otherwise = x:xs

take' n xs | n > 0 = head xs : take' (n - 1) (tail xs)
           | otherwise = []

--sq (sq 3) = sq (3 * 3) = sq 9 = 81

--sq (sq 3) = sq 3 * sq 3 = (3 * 3) * (3 * 3) = 81

-- head x:xs = x
-- tail x:xs = xs

{-
take' 3 foo = head foo :  take'(2 tail foo) = head foo : head (tail foo) : take 1 tail (tail foo) =
= head foo : head (tail foo) : head (tail (tail foo)) : take(0 tail tail tail foo)=
= head foo : head (tail foo) : head (tail (tail foo)) : [] = head 42:foo : head (tail 42:foo) : head (tail (tail 42:foo)):[]=
= 42 : 42 : 42: []

foo = 42:foo

take 3 foo = take 3 42:foo = take 3 42:42:foo = ...
-}

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:xs) | null xs = True
                | x <= head xs = isSorted xs
                | otherwise = False

bubbleSortOne ::  Ord a => [a] -> [a]
bubbleSortOne [] = []
bubbleSortOne (x:xs) | null (tail xs) = x:xs
                     |  x > head  xs = head xs: bubbleSortOne(x:tail xs)
                     | otherwise = x: bubbleSortOne xs

bubbleSort ::  Ord a => [a] -> [a]
bubbleSort xs | isSorted xs = xs
              | otherwise = bubbleSort (bubbleSortOne xs)

maximum' :: Ord a =>[a] -> a
maximum' (x:xs) | null xs = x
                | x >= head xs = maximum' (x:tail xs)
                | x < head xs = maximum' xs


data StrInt = I Int | S [Char] | L [StrInt] deriving Show


test (I x) = x
test (S x) = length x

--[I 42,S "abcd"] -> "42 abcd"
--[I 3, I 4, S "+"] -> "3 4 +"

toStr [] = []
--toStr (x:xs) | null (x:xs) = []
toStr (I x:[]) = show x
toStr (S x:[]) = x
toStr (I x:xs) = show x ++ " " ++ toStr xs
toStr (S x:xs) = x++" "++ toStr xs

-- "13  56 +" -> [I 13, I 56, S "+"]

-- "13" -> 13
-- 379 = 3*10^2 + 7*10^1 + 9*10^0

toInt' [] n = 0
toInt' (x:xs) n  = ((ord x) - 48) * (10 ^ n) + toInt' xs (n+1)

toInt xs = toInt' (reverse xs) 0 

--isMember (x:xs) = [i | i <- x, x ]

isN [] = True
isN (x:xs) | elem x ['0'.. '9'] = isN xs
           | otherwise = False


-- [12 34 5] first -> 12
-- [13 556 34] rest -> [556 34]
-- 3 [a b] cons -> [3 a b]

-- "[a [bc de]]" -> ["a","[","bc","de","]","]"] tokenize
-- lisp: racket, SICP - scheme

tokenize' (' ':xs) "" = tokenize' xs ""
tokenize' (']':xs) "" = "]" : (tokenize' xs "")
tokenize' ('[':xs) "" = "[" : (tokenize' xs "")
tokenize' (x:xs) ys
                   | x == '[' = (reverse ys):"[" : (tokenize' xs "")
                   | x == ']' = (reverse ys):"]" : (tokenize' xs "")
                   | x == ' ' = (reverse ys) : (tokenize' xs "")
                   | otherwise = tokenize' xs (x:ys)
tokenize' "" "" = []
tokenize' "" ys = (reverse ys):[]

tokenize xs = tokenize' xs ""

{--
takeFromStack' xs c
                  | (last xs) == c = []
                  | otherwise = (last xs):takeFromStack' (init xs) c

takeFromStack xs c = reverse (takeFromStack' xs c)
--}

takeFromStack xs
               | show(last xs) == show(S "[") = []
               | otherwise = (last xs):takeFromStack (init xs) 

dropStack xs
           | show(last xs) == show(S "[") = init xs
           | otherwise = dropStack (init xs)


--parseList:: [[Char]] -> [StrInt] -> [StrInt]
parseList (x:xs) ys
               | isN x = parseList xs (ys++[(I (toInt x))])
               | x == "]" = parseList xs ((dropStack ys)++[(L (reverse(takeFromStack ys)))])
               | otherwise = parseList xs (ys++[(S x)])
parseList [] ys = ys







parse (' ':xs) "" = parse xs ""
parse (' ':xs) ys |isN ys = I (toInt (reverse ys)) : parse xs ""
                  | otherwise = S (reverse ys) : parse xs ""
parse [] ys |isN ys = [I (toInt (reverse ys))]
            |otherwise = [S (reverse ys)]
parse (x:xs) ys = parse xs (x:ys)


--[I 42, I 13, S "+"] -> "42 13 +"

strstack (I x:xs) = (show x)++" "++strstack xs
strstack (S x:xs) = x++" "++strstack xs
strstack (L x:xs) = (show x)++" "++strstack xs
strstack [] = ""

convertI (I n) = n

-- + - * /
{--
polish (I x:xs) ys = polish xs (I x:ys) 
polish (S x:xs) ys | x == "-" = polish xs ((I (convertI(head (tail ys)) - convertI(head ys))): tail (tail ys))
                   | x == "+" = polish xs ((I (convertI(head (tail ys)) + convertI(head ys))): tail (tail ys))
                   | x == "*" = polish xs ((I (convertI(head (tail ys)) * convertI(head ys))): tail (tail ys))
                   | x == "dup" =  polish xs ((head ys):ys)
                   | x == "/" = polish xs (I (div (convertI(head(tail ys))) (convertI (head ys))):tail(tail ys))
                   | x == "swap" = polish xs ((head (tail ys)):(head ys): (tail(tail ys)))
                   | x == "drop" = polish xs (tail ys)
polish [] ys = ys --}

first (L (I x:xs)) = I x
first (L (S x:xs)) = S x
first (L (L x:xs)) = L x
rest (L (x:xs)) = L xs
cons (L (xs))(L (ys)) = L (xs++ys)
null' (L xs)
           | null xs = S "true"
           | otherwise = S "false"

ifOp ((L xs):(L ys):(S zs):rls)
                              | zs == "true" = ys
                              | zs == "false" = xs
                              | otherwise = []

prtStr [] = return ()
prtStr (x:xs)
        | null xs = putChar x
        | otherwise = putChar x >> prtStr xs 

polish' (xs, ys)
               | null xs = prtStr("\x1b[32m"++ (strstack ys')++"\x1b[0m"++(strstack xs') ++"\n")
               | otherwise = prtStr("\x1b[32m"++ (strstack ys') ++"\x1b[0m"++(strstack xs') ++ "\n") >> polish' (xs', ys')
               where (xs', ys') = polish xs ys

polish (L x:xs) ys = (xs, (L x:ys))
polish (I x:xs) ys =  (xs, (I x:ys))
polish (S x:xs) ys | x == "-" = (xs, ((I (convertI(head (tail ys)) - convertI(head ys))): tail (tail ys)))
                   | x == "+" = (xs, ((I (convertI(head (tail ys)) + convertI(head ys))): tail (tail ys)))
                   | x == "*" = (xs, ((I (convertI(head (tail ys)) * convertI(head ys))): tail (tail ys)))
                   | x == "dup" = (xs, ((head ys):ys))
                   | x == "/" = (xs, (I (div (convertI(head(tail ys))) (convertI (head ys))):tail(tail ys)))
                   | x == "swap" = (xs, ((head (tail ys)):(head ys): (tail(tail ys))))
                   | x == "drop" = (xs, (tail ys))
                   | x == "first" = (xs, ((first(head ys)): tail ys))
                   | x == "cons" = (xs, ( cons (head(tail ys)) (head ys):(tail(tail ys)) ))
                   | x == "rest" = (xs, (rest (head ys):(tail ys)))
                   | x == "true" = (xs, (S x):ys)
                   | x == "false" = (xs, (S x):ys)
                   | x == "null" = (xs, null'(head ys):tail ys)
                   | x == "if" = (((ifOp ys)++xs), tail(tail (tail(ys))))
                   | (head x) == ':' = (xs, (S x:ys))
                   | otherwise = (xs, (ys))
polish [] ys = ([], ys)





