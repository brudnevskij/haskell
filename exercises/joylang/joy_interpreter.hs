import Data.Char
import System.IO


data StrInt = I Int | S [Char] | L [StrInt] deriving Show

toStr [] = []
toStr (I x:[]) = show x
toStr (S x:[]) = x
toStr (I x:xs) = show x ++ " " ++ toStr xs
toStr (S x:xs) = x++" "++ toStr xs

toInt' [] n = 0
toInt' (x:xs) n  = ((ord x) - 48) * (10 ^ n) + toInt' xs (n+1)

toInt xs = toInt' (reverse xs) 0 

isN [] = True
isN (x:xs) | elem x ['0'.. '9'] = isN xs
           | otherwise = False

tokenize xs = tokenize' xs ""

tokenize' (' ':xs) "" = tokenize' xs ""
tokenize' ('\n':xs) "" = tokenize' xs ""
tokenize' (']':xs) "" = "]" : (tokenize' xs "")
tokenize' ('[':xs) "" = "[" : (tokenize' xs "")
tokenize' (x:xs) ys
                   | x == '[' = (reverse ys):"[" : (tokenize' xs "")
                   | x == ']' = (reverse ys):"]" : (tokenize' xs "")
                   | x == ' ' = (reverse ys) : (tokenize' xs "")
                   | x == '\n' = (reverse ys) : (tokenize' xs "")
                   | otherwise = tokenize' xs (x:ys)
tokenize' "" "" = []
tokenize' "" ys = (reverse ys):[]

takeFromStack xs
               | show(last xs) == show(S "[") = []
               | otherwise = (last xs):takeFromStack (init xs) 

dropStack xs
           | show(last xs) == show(S "[") = init xs
           | otherwise = dropStack (init xs)

parseList (x:xs) ys
               | isN x = parseList xs (ys++[(I (toInt x))])
               | x == "]" = parseList xs ((dropStack ys)++[(L (reverse(takeFromStack ys)))])
               | otherwise = parseList xs (ys++[(S x)])
parseList [] ys = ys

{--
strlist (S x:xs) = x++strlist xs
strlist (I x:xs) = show x ++ strlist xs
strlist (L x:xs) = (strlist x)++strlist xs
strList [] = ""
--}


strstack (I x:xs) = (show x)++" "++strstack xs
strstack (S x:xs) = x++" "++strstack xs
strstack (L x:xs) = ("["++(strstack x)++"]")++strstack xs
strstack [] = ""

convertI (I n) = n

first (L (I x:xs)) = I x
first (L (S x:xs)) = S x
first (L (L x:xs)) = L x

rest (L (x:xs)) = L xs

cons (L (xs))(L (ys)) = L ((L xs):ys)
cons (I (xs))(L (ys)) = L ((I xs):ys)
cons (L (xs))(I (ys)) = L (xs ++[(I ys)])
cons (S (xs))(L (ys)) = L ((S xs):ys)



null' (L xs)
           | null xs = S "true"
           | otherwise = S "false"

ifOp ((L xs):(L ys):(S zs):rls)
                              | zs == "true" = ys
                              | zs == "false" = xs
                              | otherwise = []

def ((L body):(S name):rls) = (tail name , body)
dip ((L ex):(I x):rls) = ex ++ [I x]
dip ((L ex):(S x):rls) = ex ++ [S x]
dip ((L ex):(L x):rls) = ex ++  [L x]
i ((L xs):rs) = xs


prtStr [] = return ()
prtStr (x:xs)
        | null xs = putChar x
        | otherwise = putChar x >> prtStr xs 

fsearch s (x:xs)
             | fst x == s =(True, snd x)
             | otherwise = fsearch s xs
fsearch s [] = (False, [])

polish' (xs, ys, zs)
               | null xs = prtStr("\x1b[32m"++ (strstack (reverse ys'))++"\x1b[0m"++(strstack xs') ++"\n")
               | otherwise = prtStr("\x1b[32m"++ (strstack (reverse ys')) ++"\x1b[0m"++(strstack xs') ++ "\n") >> polish' (xs', ys', zs')
               where (xs', ys', zs') = polish xs ys zs

-- 3 [2 +] i -> 3 2 + -> 5

polish (L x:xs) ys zs = (xs, (L x:ys), zs)
polish (I x:xs) ys zs =  (xs, (I x:ys), zs)
polish (S x:xs) ys zs
                   | x == "-" = (xs, ((I (convertI(head (tail ys)) - convertI(head ys))): tail (tail ys)), zs)
                   | x == "+" = (xs, ((I (convertI(head (tail ys)) + convertI(head ys))): tail (tail ys)), zs)
                   | x == "*" = (xs, ((I (convertI(head (tail ys)) * convertI(head ys))): tail (tail ys)), zs)
                   | x == "dup" = (xs, ((head ys):ys), zs)
                   | x == "/" = (xs, (I (div (convertI(head(tail ys))) (convertI (head ys))):tail(tail ys)), zs)
                   | x == "swap" = (xs, ((head (tail ys)):(head ys): (tail(tail ys))), zs)
                   | x == "drop" = (xs, (tail ys), zs)
                   | x == "first" = (xs, ((first(head ys)): tail ys), zs)
                   | x == "cons" = (xs, ( cons (head(tail ys)) (head ys):(tail(tail ys)) ), zs)
                   | x == "rest" = (xs, (rest (head ys):(tail ys)), zs)
                   | x == "true" = (xs, (S x):ys, zs)
                   | x == "false" = (xs, (S x):ys, zs)
                   | x == "null" = (xs, null'(head ys):tail ys, zs)
                   | x == "if" = (((ifOp ys)++xs), tail(tail (tail(ys))), zs)
                   | x == "dip" = (dip(ys)++xs, tail(tail ys),zs)
                   | x == "i" = (i(ys)++xs,(tail ys),zs)
                   | (head x) == ':' = (xs, (S x:ys), zs)
                   | x == "def" = (xs, tail(tail ys), def(ys):zs)
                   | fst(fsearch x zs) = (snd(fsearch x zs)++xs, ys, zs)
                   | otherwise = (xs, (ys), zs)
polish [] ys zs = ([], ys, zs)


-- 5 9 [2 +] dip -> 5 2 + 9 -> 7 9
{--
expr: 5 9 [2 +] dip
stack: 

expr: dip
stack: 5 9 [2 +]

expr: 2 + 9
stack: 5

expr: + 9
satack: 5 2

exzpr: 9
s: 7
--}
test :: IO ()
test = do
    line <- readFile "test.txt"
    polish' ((parseList  (tokenize line) []), [], [])
    putStrLn "done!"

main :: IO ()
main = do
    line <- readFile "program.txt"
    polish' ((parseList  (tokenize line) []), [], [])
    putStrLn "done!"