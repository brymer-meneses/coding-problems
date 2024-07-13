-- https://wiki.haskell.org/99_questions/1_to_10

-- Problem 1
-- Find the last element of a list
findLast :: [a] -> a
findLast = head . reverse

-- Problem 2
-- Find the second last element of a list
secondLast :: [a] -> a
secondLast x = head $ tail $ reverse x

-- Problem 3
-- Find the k'th element of a list
elementAt :: Int -> [a] -> Maybe a
elementAt k x
  | k > length x = Nothing
  | k <= 0 = Nothing
  | otherwise = impl k x
  where
    impl _ [] = Nothing
    impl 1 (y : _) = Just y
    impl k' (_ : ys) = impl (k' - 1) ys

-- Problem 4
-- Find the number of elements in a list
length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length xs

-- Problem 5
-- Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x = head x == last x && isPalindrome (tail $ init x)

main = do
  let array = [1 .. 10]
  putStrLn "Problem 1: Find the last element of a list"
  print $ findLast array

  putStrLn "Problem 2: Find the second last element of a list"
  print $ secondLast array

  putStrLn "Problem 3: Find the kth element of a list"
  print $ elementAt 4 array

  putStrLn "Problem 4: Find the number of elements in a list"
  print $ length' array

  putStrLn "Problem 5: Reverse a list"
  print $ reverse' array

  putStrLn "Problem 6: Find out whether a list is a palindrome"
  print $ isPalindrome [1, 1]
  print $ isPalindrome [1, 2, 1]
  print $ isPalindrome [1, 2, 3, 1]
