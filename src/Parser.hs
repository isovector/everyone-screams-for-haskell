{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Parser where

import BasePrelude hiding (try)

data Parser a = Parser { runParser :: String -> [(a, String)] }


instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (first f) $ runParser p s


instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> [(a, s)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pfs <*> pas = Parser $ \s -> do
    (f, s') <- runParser pfs s
    let as = runParser pas s'
    fmap (first f) as


instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = Parser $ \s ->
    let as = runParser pa s
     in concatMap (\(a, s') -> runParser (f a) s') as


instance Alternative Parser where
  empty = Parser $ const []

  pa <|> pb = Parser $ \s ->
    let as = runParser pa s
     in if length as == 0
           then runParser pb s
           else as


instance MonadPlus Parser where
  mzero = empty


satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \(c : s)  ->
  if pred c
     then [(c, s)]
     else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string str = do
  forM_ str char
  return str

parens :: Parser a -> Parser a
parens p = do
  char '('
  result <- p
  char ')'
  return result

oneOf :: [Parser a] -> Parser a
oneOf ps = foldl1 (<|>) ps

try :: Parser a -> Parser (Maybe a)
try p = fmap Just p <|> pure Nothing

