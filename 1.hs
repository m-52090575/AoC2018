import Data.List (scanl)
import qualified Data.Set as Set

main = do
    let file = "input"
    contents <- readFile file
    let linesOfFile = lines contents
    -- print "Final frequency is:"
    -- print (sum (listToInt lines))
    print "First duplicate frequency is:"
    print (firstDupInt (listToInt linesOfFile))

listToInt lines = map num lines
    where num = \x -> getNumToken 0 x

runningTotal list = scanl (+) 0 (cycle list)
firstDupInt list = dupString (runningTotal list)

dupString :: (Ord a, Show a) => [a] -> [Char]
dupString x =
    case dup x of
        Just x -> "First dup int is " ++ (show x)
        Nothing -> "No dup"

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
    where dup' [] _ = Nothing
          dup' (x:xs) s = if Set.member x s
                        then Just x
                        else dup' xs (Set.insert x s)

getNumToken accum [] = accum
getNumToken accum (c:more)
   | isDigit c  = getNumToken (accum * 10 + (digitToInt c)) more
   | c == '+'   = getNumToken accum more
   | c == '-'   = -1 * (getNumToken accum more)
   | otherwise  = accum
