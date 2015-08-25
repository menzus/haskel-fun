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
  mzero     = Parser (\inp -> [])
  mplus p q = Parser (\inp -> (parse p inp ++ parse q inp))

item :: Parser Char
item = Parser (\cs -> case cs of
  ""     -> []
  (c:cs) -> [(c, cs)])

sat     :: (a -> Bool) -> Parser a -> Parser a
sat l p = do { x <- p; if l x then return x else mzero }

char   :: Char -> Parser Char
char c = sat (c ==) item

(|.)   :: Parser a -> Parser a -> Parser a
p |. q = Parser (\cs -> case parse (p <|> q) cs of
                            []     -> []
                            (x:xs) -> [x])

upTo     :: Int -> Parser a -> Parser [a]
upTo n p = do { xs <- many p; if length xs <= n then return xs else mzero }
