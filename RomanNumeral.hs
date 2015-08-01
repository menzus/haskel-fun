import Data.String
import Data.Maybe

asArabic :: String -> Maybe Integer
asArabic roman = 
  let 
    romanToArabic = [("IV", 4), ("IX", 9), ("XL", 40), ("XC", 90), ("CD", 400), ("CM", 900), 
      ("I", 1), ("V", 5), ("X", 10), ("L", 50), ("C", 100), ("D", 500), ("M", 1000)]

    romanSymbolToArabic char remaining acc = do 
      arabic <- lookup [char] romanToArabic
      asArabicWithAcc remaining (arabic + acc)
    romanDualSymbolToArabic a b remaining acc = 
        let arabic = lookup [a,b] romanToArabic
        in 
          if isJust arabic then
            do 
              num <- arabic
              asArabicWithAcc remaining (num + acc)
          else
            romanSymbolToArabic a (b:remaining) acc

    asArabicWithAcc roman acc = case (roman) of
      (a:b:ns) -> romanDualSymbolToArabic a b ns acc
      (a:ns)   -> romanSymbolToArabic a ns acc
      ([])     -> Just acc
  in 
    asArabicWithAcc roman 0
