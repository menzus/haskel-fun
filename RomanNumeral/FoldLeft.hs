module RomanNumeral.FoldLeft where 

import Data.String
import Data.Maybe

asArabic :: String -> Maybe Int
asArabic roman = 
    let
      arabicByRomanDigit = [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]      
      romanDigitsToArabic roman = sequence (map (\char -> lookup char arabicByRomanDigit) roman)
      addArabicDigits (prev, total) curr = 
        if prev < curr then 
          (curr, total - (2 * prev) + curr)  
        else
        (curr, total + curr)
      sumArabicDigits arabicDigits = Just (foldl addArabicDigits (maxBound, 0) arabicDigits)
    in do 
      arabicDigits <- romanDigitsToArabic roman
      result <- sumArabicDigits arabicDigits
      Just (snd result)