{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x`mod`10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
	| x <= 0 = []
	| otherwise = x `mod` 10 : toRevDigits (x `div` 10)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs
doubleEveryOther a = a

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits l = findSum 0 l
	where
	  findSum n [] = n
	  findSum n (x:xs) =
		if x < 10
		 then findSum (n+x) xs
		else findSum (n + lastDigit x + lastDigit (dropLastDigit x)) xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn l = if ((sumDigits (doubleEveryOther (toRevDigits l)))`mod` 10 == 0)
		then True
	else False

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
