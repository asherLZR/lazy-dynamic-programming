module Lib
    ( recursiveEd, dynProgEd, dynProgEdSlow, lazyDynProgEd ) where

import Debug.Trace(trace)

recursiveEd :: String -> String -> Int
recursiveEd [] b = length b
recursiveEd a [] = length a
recursiveEd a@(ax: axs) b@(bx: bxs) = if ax == bx then recursiveEd axs bxs
                        else 1 + minimum [recursiveEd axs bxs, recursiveEd axs b, recursiveEd a bxs]

dynProgEdSlow :: String -> String -> Int
dynProgEdSlow [] b = length b
dynProgEdSlow a [] = length a
dynProgEdSlow a b = 
    let 
        nextRow :: [Int] -> Char -> [Int]
        nextRow prevRow ac = 
            let diagonals = zipWith (\bc v -> if bc == ac then v-1 else v) b prevRow
                lefts = thisRow
                ups = tail prevRow
                mins = zipWith min diagonals $ zipWith min lefts ups
                rowCount = head prevRow + 1
                thisRow = rowCount:zipWith (\min bc -> min + 1) mins b
            in thisRow
        table = [0..length b]:zipWith nextRow table a
    in last $ last table

dynProgEd :: 
    String      -- | first string a
    -> String   -- | second string b
    -> Int      -- | output to our edit distance problem
dynProgEd [] b = length b
dynProgEd a [] = length a
dynProgEd a b = 
    let 
        -- | Calculate all subsequent rows from the previous of the memoisation table
        nextRows :: 
            [Int]       -- | the previous row in the memoisation table
            -> [Char]   -- | the remaining characters of string a to be compared
            -> [[Int]]  -- | subsequent rows of the memoisation table
        nextRows _ [] = []
        nextRows prevRow (ax:axs) = 
            let 
                -- | Calculates the rest of the row from the previous character in the row
                doRow :: 
                    [Char]      -- | the remaining characters of string b to be compared
                    -> [Int]    -- | the previous row of the memoisation table starting from the column being examined
                    -> Int      -- | the previous cost assignment west of the current square
                    -> [Int]    -- | the rest of the row, cost of the remaining characters of string b
                doRow [] _ _ = []
                doRow (bx:bxs) (nw:n) w =
                    let me = if ax == bx then nw 
                        else 1 + minimum [w, nw, (head n)]
                    in me:doRow bxs n me
                firstElement = 1 + (head prevRow)
                thisRow = firstElement:(doRow b prevRow firstElement)
            in thisRow:nextRows thisRow axs
        table = [0..length b]:nextRows (head table) a
    in last $ last table

lazyDynProgEd :: 
    String      -- | first string a
    -> String   -- | second string b
    -> Int      -- | output to our edit distance problem
lazyDynProgEd [] b = length b
lazyDynProgEd a [] = length a
lazyDynProgEd a b = 
    let mainDiag = oneDiag a b (head uppers) (-1:(head lowers))     -- | -1 initialises the top-left square in the mainDiag to 0
        uppers = eachDiag a b (mainDiag:uppers)                     -- | uppers passed to eachDiag expands to form the remaining diags
        lowers = eachDiag b a (mainDiag:lowers)                     -- | lowers passed to eachDiag expands to form the remaining diags
        -- | Lazily expands the diagonal between diagAbove and diagBelow as they become known
        oneDiag :: 
            String      -- | first string a
            -> String   -- | second string b
            -> [Int]    -- | diagAbove
            -> [Int]    -- | diagBelow
            -> [Int]    -- | edit distance values for the middle diagonal
        oneDiag a b diagAbove diagBelow =
            let 
                doDiag :: 
                    String      -- | the remaining characters of string a to be compared
                    -> String   -- | the remaining characters of string b to be compared
                    -> Int      -- | nw element to the current square in the diagonal (the previous element in this diagonal)
                    -> [Int]    -- | diagAbove
                    -> [Int]    -- | diagBelow
                    -> [Int]    -- | edit distance values for the middle diagonal from the 2 input strings
                doDiag [] _ _ _ _ = []
                doDiag _ [] _ _ _ = []
                doDiag (ax:axs) (bx:bxs) nw n w =
                    let me = if ax == bx then nw
                        else 1 + specialMin3 (head w) nw (head n)
                    in me:(doDiag axs bxs me (tail n) (tail w))
                firstElement = 1 + head diagBelow
                thisDiag = firstElement:(doDiag a b firstElement diagAbove (tail diagBelow))
            in thisDiag
        -- | Initialisation for either upper or lower diagonals. The functions a', bx', and bx' are apostrophed to differentiate them from strings a and b in the outer scope.
        eachDiag :: 
            String      -- | first string a'
            -> String   -- | the remaining characters of string b' to be compared
            -> [[Int]]  -- | the final form of uppers/lowers
            -> [[Int]]  -- | a list containing all diagonals from comparing the remaining characters in b'
        eachDiag a' [] diags = []
        eachDiag a' (bx':bxs') (lastDiag:diags) =
            let nextDiag = head $ tail diags                    -- | grab the successor of thisDiag in uppers/lowers to form thisDiag
                thisDiag = oneDiag a' bxs' nextDiag lastDiag
            in thisDiag:(eachDiag a' bxs' diags)
        lengthDiff = (length a) - (length b)
    -- | only evaluate what is necessary
    in last (if lengthDiff == 0 then mainDiag 
    else if lengthDiff > 0 then (lowers !! (lengthDiff-1)) 
    else (uppers !! (abs (lengthDiff)-1)))

specialMin3 :: Ord a => a -> a -> a -> a
specialMin3 a b c = if a < b then a else min b c