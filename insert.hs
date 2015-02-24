-- Project Group 15. 15:00 -16:00 24 feb 2015 skrevs denna funktion

insert_row :: String -> Integer -> [String] -> [String]

insert_row instr row str_list = insert_row_aux instr row 1 [] str_list

insert_row_aux :: String -> Integer -> Integer -> [String] -> [String] -> [String]

insert_row_aux instr row cur_row new_xs []
                                  | row == cur_row = reverse ("/n":instr:new_xs)
                                  | True = insert_row_aux instr row (cur_row+1) ("/n":new_xs) []

insert_row_aux instr row cur_row new_xs (str:xs)
                                  | row == cur_row = (reverse new_xs) ++ [instr] ++ ["/n"] ++ (str:xs)
                                  | str == "/n" = insert_row_aux instr row (cur_row+1) (str:new_xs) xs
                                  | True = insert_row_aux instr row cur_row (str:new_xs) xs

testlist= ["hello","/n","sven","/n","my","name","is","/n","olof","raxores","/n","I ","/n","hope","that","/n",
            "we","/n","will","meet","again","/n","someday"]
