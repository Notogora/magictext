import Data.Char
import System.IO





readfileWrows :: String -> IO ()
readfileintolist file = do
  contents <- readFile file
  do
    putStrLn $ show (rows_func (words contents) contents [])



-- Det här är den användbara saken
rows_func :: [String] -> String ->[String] -> [String]

rows_func [] _ listwrows = reverse listwrows

rows_func wordxs (' ':txtstr) listwrows =
                              rows_func wordxs txtstr listwrows

rows_func (word:wordxs) txtstr listwrows
                        | word == take (length word) txtstr =
                             rows_func wordxs (drop (length word) txtstr) (word:listwrows)
                        | otherwise = rows_func (word:wordxs) (drop 1 txtstr) ("\n":listwrows)
