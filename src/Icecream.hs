{-# LANGUAGE NoImplicitPrelude #-}

module Icecream where

import Parser
import BasePrelude hiding (try)


data TypeDecl = TypeDecl { tdId :: String, tdType :: Type }

data Type = TypeCtr String
          | TypeArr Type Type
          deriving Show


typeParser :: Parser Type
typeParser = do
  t <- oneOf [ TypeCtr <$> ident
             , parens typeParser
             ]
  arr <- try (string "->")
  case arr of
    Just _  -> TypeArr t <$> typeParser
    Nothing -> return t



ident :: Parser String
ident = do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlpha
  return $ c : cs


icecream :: Parser Type
icecream = typeParser


main :: IO ()
main = do
  f <- readFile "src/test.icecream"
  print $ runParser icecream f

