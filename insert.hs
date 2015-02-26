-- Project Group 15. 15:00 -16:00 24 feb 2015 skrevs denna funktion
-- Projecy Group 15. 13:00 -20:00 .
import Data.Char
import System.IO
import Data.List
-- * Insert, insert a string "x" into row y är den här delen core

--todo comment acording to coding convention
insertintofile :: String -> Integer -> String -> IO ()
insertintofile input row file = do
   contents <- readFile file
   if null contents
     then return ()
     else do
     (length contents `seq` ( writeFile file $ (unwords(insert_row input row (rows_func (words contents) contents [])))))
-- This a forced way to WriteFile I read about on the net







--todo comment acording to coding convention
insert_row :: String -> Integer -> [String] -> [String]

insert_row instr row str_list = insert_row_aux instr row 1 [] str_list

insert_row_aux :: String -> Integer -> Integer -> [String] -> [String] -> [String]

insert_row_aux instr row cur_row new_xs []
                                  | row == cur_row = reverse ("\n":instr:new_xs)
                                  | True = insert_row_aux instr row (cur_row+1) ("\n":new_xs) []

insert_row_aux instr row cur_row new_xs (str:xs)
                                  | row == cur_row = (reverse new_xs) ++ [instr] ++ ["\n"] ++ (str:xs)
                                  | str == "\n" = insert_row_aux instr row (cur_row+1) (str:new_xs) xs
                                  | True = insert_row_aux instr row cur_row (str:new_xs) xs



            {-  rows_func (words readfileString) readfileString accList
                Purpose: Returns a list of strings (including "\n" which denotes row change)
                from a long string retrived from a text file.
                PRE: The first argument must be same as the 2nd argument but with the function
                    words performed on it.The 3rd argument is an empty list
                POST: Returns a readfilestring as List of strings including "/n"
                EXAMPLES: rows_func (words rfilestring) rfilestring [] = [word1,word2,word3,...,"\n",wordN,...;"\n"]
            -}

rows_func :: [String] -> String ->[String] -> [String]

rows_func [] _ listwrows = reverse listwrows

rows_func wordxs (' ':txtstr) listwrows =
                                  rows_func wordxs txtstr listwrows

rows_func (word:wordxs) txtstr listwrows
                              | word == take (length word) txtstr =
                                  rows_func wordxs (drop (length word) txtstr) (word:listwrows)
                              | otherwise = rows_func (word:wordxs) (drop 1 txtstr) ("\n":listwrows)
