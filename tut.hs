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

head' :: [a] -> a
head' [] = error "I love it when tutorials call me dumb"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "this list is too long. First and second element are :" ++ show x ++ "and " ++ show y

{-bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, that can be hard, sorry!"
  | bmi <= normal = "You're supposedly nomral. Bet you're not though!"
  | bmi <= fat    = "You're fat! Lose some weight!"
  | otherwise     = "you're undeniable!"
  where 
    bmi = weight / height ^ 2
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
calcBmis xs = [bmi w h | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let 
    sideArea = 2 * pi * r * h
    topArea = pi * r ^2
  in 
    sideArea + 2 * topArea

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
-}

--max' :: (Ord a) => a -> a -> a
--max' a b
--  | a > b   = a
--  | otherwise = b

--calcMod :: Int -> (Int, Int)
--calcMod x = (x `mod` 3, x `mod` 5)

--fizzBuzz :: (Int, Int) -> String
--fizzBuzz :: (0, 0) = "FizzBuzz"
--fizzBuzz :: (0, x) = "Fizz"
--fizzBuzz :: (x, 0) = "Buzz"
--fizzBuzz :: (x, y) = show (x * 3)

fizzBuzz' :: (Integral a, Show a) => a -> String
fizzBuzz' a 
  | a `mod` 3 == 0 && a `mod` 5 == 0 = "FizzBuzz"
  | a `mod` 3 == 0 = "Fizz"
  | a `mod` 5 == 0 = "Buzz"
  | otherwise = show a 


-- Init merely strips of the last space.
fizzBuzz :: Integer -> String
fizzBuzz x = init (concat [fizzBuzz' n ++ " " | n <- [1..x]])