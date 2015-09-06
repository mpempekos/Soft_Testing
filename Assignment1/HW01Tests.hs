-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
		[(1234,[4,3,2,1]), (0,[]),(7845,[5,4,8,7]),(-17,[])]
	]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
		[([4,9,5,5],[4,18,5,10]), ([0,0],[0,0]),([2,6,7,8],[2,12,7,16])]
	]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d


ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
		[([10,5,18,4], 19), ([5,8,9,11],24), ([12,4,13],11)]
	]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
		[(5594589764218858, True), (1234567898765432,False), (5167134832004555,True)]
	]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  --, ex6Tests
                  ]
