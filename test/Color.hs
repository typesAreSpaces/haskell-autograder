module Color where

import Text.Printf

-- colored printing enabled
coloredPrint = False
-- coloredPrint = True

-- colored printing functionality
data Color = Normal | Red | Green

codeFor Normal = 0
codeFor Red = 31
codeFor Green = 32

stringFor color = "\x1b[" ++ show (codeFor color) ++ "m"

paint color string
    | coloredPrint = stringFor color ++ string ++ stringFor Normal
    | otherwise = string

labelColor solved given
    | solved == given = Green
    | otherwise = Red

resultColor scored maximum
    | maximum / 2 <= scored = Green
    | otherwise = Red
