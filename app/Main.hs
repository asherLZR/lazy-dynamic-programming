module Main where

import Lib

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    arr_temp <- getMultipleLines n
    let arr = arr_temp :: [[String]]
    mapM_ print (map (\x -> dynProgEd (head x) (last x)) arr)

getMultipleLines :: Int -> IO [[String]]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine
        y <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = ([x,y]:xs)    
        return ret   
  