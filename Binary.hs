module Binary where

import Data.Char
import Data.Array

runBinaryString :: String -> Int -> Int -> Int
runBinaryString str total i = do
    if (length str > i)
        then do 
            let value = (digitToInt (str!!i)) * (2^((length str) - (i + 1)))
            runBinaryString str (total + value) (i + 1)
        else total

decode :: String -> Int
decode str = runBinaryString str 0 0
