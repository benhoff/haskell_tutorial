multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z



reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs



head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs) = x : take' (n-1) xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

{-
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
-}

describeList :: [a] -> String
describeList xs = "This list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallBabyChild x = if x > 100
        then x
        else x * 2

doubleSmallBabyChild' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--length' xs = sum [1 | _ <- xs]
--
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
captial "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <-st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

{-
head' :: [a] -> a
head' [] = error "I love it when tutorials call me dumb"
head' (x:_) = x
-}
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "this list is too long. First and second element are :" ++ show x ++ "and " ++ show y

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, that can be hard, sorry!"
  | bmi <= normal = "You're supposedly nomral. Bet you're not though!"
  | bmi <= fat   = "You're fat! Lose some weight!"
  | otherwise     = "you're undeniable!"
  where bmi = weight / height ^2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where 
    (f:_) = firstname
    (l:_) = lastname
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b   = GT
  | a == b  = EQ
  | otherwise = LT

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let 
    sideArea = 2 * pi * r * h
    topArea = pi * r 
  in 
    sideArea + 2 * topArea

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

--max' :: (Ord a) => a -> a -> a
--max' a b
--  | a > b   = a
--  | otherwise = b

--calcMod :: Int -> (Int, Int)
--calcMod x = (x `mod` 3, x `mod` 5)

fizzBuzz' :: (Integral a, Show a) => a -> String
fizzBuzz' a 
  | a `mod` 3 == 0 && a `mod` 5 == 0 = "FizzBuzz"
  | a `mod` 3 == 0 = "Fizz"
  | a `mod` 5 == 0 = "Buzz"
  | otherwise = show a 


-- Init merely strips of the last space.
fizzBuzz :: Integer -> String
fizzBuzz x = init (concat [fizzBuzz' n ++ " " | n <- [1..x]])
