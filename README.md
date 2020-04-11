# Lazy Dynamic-Programming for Edit Distance
_I recently stumbled upon the edit distance problem again while working on an [ElasticSearch](https://www.elastic.co/) problem. It reminded me of the wonder and joy of studying algorithms. However well-trodden a path, another clever "trick" lies waiting to be found. Using the power of lazy evaluation to solve the edit distance problem is a beautiful rendition of an old favourite, yet current problem._

_Github repository with tests [here](https://github.com/asherLZR/lazy-dynamic-programming). The following code samples are provided in Python where applicable and Haskell to demonstrate the constrast between the imperative and functional solutions. We will discuss 3 solutions including brute-force O(3<sup>N</sup>), standard dynamic programming O(|A| * |B|), and lazy dynamic programming O(|A| * (1 + D A B))._

_Based on the paper [Lazy Dynamic-Programming Can be Eager](http://users.monash.edu/~lloyd/tildeStrings/Alignment/92.IPL.html) by Dr. L. Allison (1992)._

## The Problem
Given 2 strings A and B, find the Levenshtein distance, the minimum number of point-mutations (delete, insert, substitutions) required to transform them into identical strings.

For example, the word pair ("cgggtatccaa", "ccctaggtccca") has an edit distance of 6

| Operation | Index |     Letter |         Result |
| --------- | :---: | ---------: | -------------: |
| -         |   -   |          - |  "cgggtatccaa" |
| DEL       | a[1]  |        "g" |   "cggtatccaa" |
| SUB       | a[1]  | "g" -> "c" |   "ccgtatccaa" |
| SUB       | a[2]  | "g" -> "c" |   "ccctatccaa" |
| INS       | a[5]  |        "g" |  "ccctagtccaa" |
| INS       | a[6]  |        "g" | "ccctaggtccaa" |
| SUB       | a[10] | "a" -> "c" | "ccctaggtccca" |
  
Note that there may be multiple sets of point-mutations that arrive at the same solution.

The edit distance problem is useful in a variety of applications including DNA sequencing, spell-check, and spam filtering.

## Subproblems

The solution to this problem comes from the insight that the cost of mutating a string can be minimised by taking the fewest mutations required for `a[:1], a[:2]..,a`. Note that a similar principle applies if we check from the last character of the string `a[-1], a[-2:].., a`.

To take the example above, ("cgggtatccaa", "ccctaggtccca"), if we start from the last character of either string, conversion of (`a[-1]`, `b[-1]`), ("a", "a") requires a cost of 0. Then looking at  (`a[-2:]`, `b[-2:]`), ("aa", "ca"), the cost relies on the cost of the previous comparison, ("a", "a") and potential additions or deletions ("", "a"), ("a", ""). The last 2 are our base cases at cost 1.

From this insight, we derive the following:

The cost of converting substrings of a to b to each other is not incremented if the characters being examined match. In all other cases, a point-mutation is required so 1 is added to the minimum of 3 costs.

## Naive Solution: O(3<sup>N</sup>)
_where N = min(|A|, |B|)._

By checking all possible mutations recursively, we can incrementally build to the solution for the entire string. So far nothing special between either implementations.

### Python Implementation
```py
def edit_distance_rec(a, b):
    if len(a) == 0:
        return len(b)
    if len(b) == 0:
        return len(a)
    if a[-1] == b[-1]:
        return edit_distance_rec(a[:-1], b[:-1])
    else:
        return 1 + min(edit_distance_rec(a[:-1], b),
                       edit_distance_rec(a, b[:-1]),
                       edit_distance_rec(a[:-1], b[:-1]))
```

### Haskell Implementation
```hs
recursiveEd :: String -> String -> Int
recursiveEd [] b = length b
recursiveEd a [] = length a
recursiveEd a@(ax: axs) b@(bx: bxs) = if ax == bx then recursiveEd axs bxs
                        else 1 + minimum [recursiveEd axs bxs, recursiveEd axs b, recursiveEd a bxs]
```

## Tabular Dynamic Programming (O(|A| * |B|))
The previous solution works but is slow as there are recurring sub-problems that are not exploited. Dynamic programming is typically used to cache these previous edit values to avoid re-computation.

### Python Implementation
```py
def edit_distance(a, b):
    memo = [[0] * (len(a)+1) for _ in range(len(b)+1)]
    memo[0] = [x for x in range(len(a)+1)]
    for i, row in enumerate(memo):
        row[0] = i
    for i in range(1, len(b)+1):
        for j in range(1, len(a)+1):
            if b[i-1] == a[j-1]:
                memo[i][j] = memo[i-1][j-1]
            else:
                memo[i][j] = min(memo[i-1][j]+1, memo[i][j-1]+1, memo[i-1][j-1]+1)
    return memo[-1][-1], memo
```

### Haskell Implementation
DP in a functional language with immutable variables utilises the caching property of unevaluated values, [thunks](https://wiki.haskell.org/Thunk) in Haskell. Each row is built recursively by taking as input the previous row, the row count, and character being compared.

An in-depth explanation of how DP problems are solved in Haskell can be found [here](http://travis.athougies.net/posts/2018-05-05-dynamic-programming-is-recursion.html).

This is a copy of the implementation provided in the [paper](http://users.monash.edu/~lloyd/tildeStrings/Alignment/92.IPL.html), translated to Haskell and commented.
```hs
-- | Solve the edit distance problem
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
```

## Lazy Dynamic Programming O(|A| * (1 + D A B)) 
### Haskell Implementation
In the previous solutions, we built a memoisation table to calculate our edit distance values. However this is inefficient because the only value that matters is the one in the last position of some diagonal in `table`! This means that the number of intermediate calculations to get to that value can be minimised. How do we use laziness to do this? We will go through 3 general ideas then let the code speak about its specifics; they are:

1. Prefer diagonals over rows to enforce order of evaluation
2. Force evaluation in a consistent direction
3. Be lazy - declare calculation of new values based on un-evaluated dependencies

Let's take the simple case where `a = "aaa"` and `b = "aaaaa"`. Imagine for a moment that we were using the earlier method of expanding the entire memoisation table - it would look like this. 

|     |   '' |    a |    a |    a |    a |    a |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ''  |    0 |    1 |    2 |    3 |    4 |    5 |
| a   |    1 |    0 |    1 |    2 |    3 |    4 |
| a   |    2 |    1 |    0 |    1 |    2 |    3 |
| a   |    3 |    2 |    1 |    0 |    1 |    2 |

Because we are lazily evaluating the entire table not all of these values will be filled in, but for now this gives us a visual representation of what we are trying to do. For this simplified example, in theory we should ever only need to calculate the values marked by `X` and `Y` here. The `Y` values need to be known as we look at the NW, W and N values of a particular entry to determine `X`. 

|     |   '' |    a |    a |    a |    a |    a |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ''  |    X |    Y |    2 |    3 |    4 |    5 |
| a   |    Y |    X |    Y |    2 |    3 |    4 |
| a   |    2 |    Y |    X |    Y |    2 |    3 |
| a   |    3 |    2 |    Y |    X |    1 |    2 |

It suggests that we need to rethink the way we represent the table. Instead of rows and columns, we structure our data as 3 diagonal groups instead. This allows us to better enforce our intended order of evaluation.

```hs
mainDiag :: [Int]
mainDiag = [0, 0, 0, 0]

lowers :: [[Int]]
lower = [
    [1, 1, 1],
    [2, 2],
    [3]
]

uppers :: [[Int]]
uppers = [
    [1, 1, 1, 1],
    [2, 2, 2, 2],
    [3, 3, 3],
    [4, 4],
    [5]
]
```

One thing we notice is that if `len(a) == len(b)`, the last value in `mainDiag` is our answer. Otherwise if `len(a) != len(b)`, we look in either `lowers` or `uppers` for the corresponding corner value.

Depending on the input strings of course, this path starting in the top-left can snake to the left or the right of the table and cause further evaluation of `uppers` or `lowers`. This change in direction is governed by our simple `minimum [W, NW, N]` when a point mutation is required. Where possible we prefer expanding in one direction rather than in both. The wonderful solution to this problem was to simply introduce `specialMin3 a b c = if a < b then a else min b c`.

Now we worry about how to fill in our values. `lowers` is represented by L, `mainDiag` by M, and `uppers` by U. We initialise our 3 diagonal stores to depend on their NW, W and N values. To construct the next value of `mainDiag` here for example, it needs to take as input `head uppers` and `head lowers`. _This is allowed even if `uppers` and `lowers` have not been evaluated._ Coming from an imperative programming background, this was something quite difficult to wrap my head around, but reading more about thunks and how functional programming works was incredibly useful in bridging this gap.

|     |   '' |    a |    a |    a |    a |    a |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ''  |    M |    U |    - |    - |    - |    - |
| a   |    L |    ? |    - |    - |    - |    - |
| a   |    - |    - |    - |    - |    - |    - |
| a   |    - |    - |    - |    - |    - |    - |

And that's it! This sums up the main points I had trouble with understanding the algorithm. This is a copy of the implementation provided in the [paper](http://users.monash.edu/~lloyd/tildeStrings/Alignment/92.IPL.html), translated to Haskell and commented.
```hs
-- | Solve the edit distance problem
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
        -- | Constructor for a diagonal between diagAbove and diagBelow which lazily expands as values become known
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
        -- | Constructor for either upper or lower diagonals. The functions a', bx', and bx' are apostrophed to differentiate them from strings a and b in the outer scope.
        eachDiag :: 
            String      -- | first string a'
            -> String   -- | the remaining characters of string b' to be compared
            -> [[Int]]  -- | (lastDiag which started from bx'):(list of diagonals from comparing the remaining characters in bxs')
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
```

```hs
-- | always expand lowers if possible
specialMin3 :: Ord a => a -> a -> a -> a
specialMin3 a b c = if a < b then a else min b c
```

## Final thoughts
The paper explains the algorithm more concisely and it is worth reading. My thoughts here are about the points that I didn't quite appreciate until I had a closer look at this, at face value, complex algorithm, so I hope it was at least somewhat useful for the reader. Do visit the repo [here](https://github.com/asherLZR/lazy-dynamic-programming) and if you like, make changes and run the tests provided.

## References
Allison, L. (1992). Lazy dynamic-programming can be eager. Information Processing Letters, 43(4), 207-212. doi:10.1016/0020-0190(92)90202-7