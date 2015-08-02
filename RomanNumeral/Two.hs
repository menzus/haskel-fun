module RomanNumeral.Two where 

import Data.String
import Data.Either
import Data.Maybe

asArabic :: String -> Either String Integer
asArabic roman = case asArabicWithAcc roman 0 of 
    Just arabic -> Right arabic
    Nothing     -> Left  ("Illegal roman numeral: " ++ roman)
  where 
    asArabicWithAcc ('I':'V':ns) acc = asArabicWithAcc ns (acc + 4)
    asArabicWithAcc ('I':'X':ns) acc = asArabicWithAcc ns (acc + 9)
    asArabicWithAcc ('X':'L':ns) acc = asArabicWithAcc ns (acc + 40)
    asArabicWithAcc ('X':'C':ns) acc = asArabicWithAcc ns (acc + 90)
    asArabicWithAcc ('C':'D':ns) acc = asArabicWithAcc ns (acc + 400)
    asArabicWithAcc ('C':'M':ns) acc = asArabicWithAcc ns (acc + 900)
    asArabicWithAcc ('I':ns) acc     = asArabicWithAcc ns (acc + 1)
    asArabicWithAcc ('V':ns) acc     = asArabicWithAcc ns (acc + 5)
    asArabicWithAcc ('X':ns) acc     = asArabicWithAcc ns (acc + 10)
    asArabicWithAcc ('L':ns) acc     = asArabicWithAcc ns (acc + 50)
    asArabicWithAcc ('C':ns) acc     = asArabicWithAcc ns (acc + 100)
    asArabicWithAcc ('D':ns) acc     = asArabicWithAcc ns (acc + 500)
    asArabicWithAcc ('M':ns) acc     = asArabicWithAcc ns (acc + 1000)
    asArabicWithAcc [] acc           = Just acc
    asArabicWithAcc _ acc            = Nothing
    