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

findFirst needle haystack@(x : xs) -- takes 2 parameter, needle(to "find" the item of interest) and haystack(list)
  | needle x = Match 0         -- If the first element satisfies the predicate, return 'Match' at index 0
  | otherwise = case findFirst needle xs of  --If the first element does not satisfy the predicate, recursively search in the remaining elements
                  NoMatch -> NoMatch  -- If no match is found in the remaining elements, return 'NoMatch'
                  Match index -> Match (index + 1)  -- If a match is found, return 'Match' at the next index
findFirst _ [] = NoMatch --If the list is empty, return 'NoMatch'

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome [] = True --empty string is True
palindrome str = str == reverse str --reverse str reverses the list, and str == reverse str checks if the original list is equal to its reverse, indicating that it's a palindrome.
  


