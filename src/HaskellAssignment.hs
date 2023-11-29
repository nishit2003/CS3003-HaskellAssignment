-- Name: Nishit grover
-- UCID: M15329773

module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst _ [] = NoMatch
findFirst p xs@(x:xs') |
p x       = Match $ length xs - length xs'
| otherwise = findFirst p xs'



------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome [] = True
palindrome str@(x:xs)
| str == reverse str = True
| otherwise          = False
where
  revStr = reverse str
