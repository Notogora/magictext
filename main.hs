import System.IO
import Data.Char


{- search file value
   TODO: This is just an initial play version of a serach, going to be searching
         for full words and full words only, making sure we've got something
         useful at least.

         For now it works as intended, although the intended use of this function
         isn't really a whole lot...
-}
search file value = do
  contents <- readFile file
  if null contents
    then return ()
    else do
      putStrLn $ (show (searchCount (words contents) 0 value))

{- searchCount xs acc value
   TODO: xs takes in a words of it, so it's a list to use and just recurse through
-}
searchCount :: [String] -> Int -> String -> Int
searchCount [] acc value = acc
searchCount (x:xs) acc value
  | x == value = searchCount xs (acc+1) value
  | otherwise = searchCount xs acc value
