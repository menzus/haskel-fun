module RomanNumeral.ParserMonad where 

import Control.Applicative
import Control.Monad

newtype Parser a = Parser (String -> [(a, String)])
parse (Parser p) = p

instance Functor Parser where
  fmap  = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap 

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
  return v = Parser (\inp -> [(v, inp)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
  mzero       = Parser (\inp -> [])
  p `mplus` q = Parser (\inp -> (parse p inp ++ parse q inp))

item :: Parser Char
item = Parser (\cs -> case cs of
  ""     -> []
  (c:cs) -> [(c, cs)])

sat   :: (Char -> Bool) -> Parser Char
sat p = do { c <- item; if p c then return c else mzero}

char   :: Char -> Parser Char
char c = sat (c ==)

(|.)   :: Parser a -> Parser a -> Parser a
p |. q = Parser (\cs -> case parse (p <|> q) cs of
                            []     -> []
                            (x:xs) -> [x])

upTo     :: Int -> Parser a -> Parser [a]
upTo 0 _ = return []
upTo n p = exactly n p |. upTo (n-1) p |. return []
  where exactly 0 _ = return []
        exactly n p = do { a <- p; as <- exactly (n-1) p; return (a:as)}

