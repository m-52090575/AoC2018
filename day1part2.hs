import Data.List (scanl)
import qualified Data.Set as Set
import Data.Char (isDigit, digitToInt)

main = do
    let file = "input-2"
    --let file = "input"
    contents <- readFile file
    let linesOfFile = lines contents
    print "Final frequency is:"
    print (sum (listToInt linesOfFile))
    print "Running total:"
    print (runningTotal (listToInt linesOfFile))
    --print "First duplicate frequency is:"
    --print (firstDupInt (listToInt linesOfFile))

listToInt lines = map num lines
    where num = \x -> getNumToken 0 x

runningTotal list = scanl (+) 0 list

getNumToken accum [] = accum
getNumToken accum (c:more)
   | isDigit c  = getNumToken (accum * 10 + (digitToInt c)) more
   | c == '+'   = getNumToken accum more
   | c == '-'   = -1 * (getNumToken accum more)
   | otherwise  = accum
