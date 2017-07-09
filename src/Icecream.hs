{-# LANGUAGE NoImplicitPrelude #-}

module Icecream where

import Parser
import BasePrelude hiding (try)


data TypeDecl = TypeDecl { tdId :: String, tdType :: Type }
          deriving Show

data Type = TypeCtr String
          | TypeArr Type Type
          deriving Show

data Func = Func { fID :: String, fBinds :: [String], fBody :: Expr }
          deriving Show

type Expr = String
           

typeParser :: Parser Type
typeParser = do
  t <- oneOf [ TypeCtr <$> ident
             , parens typeParser
             ]
  arr <- try $ trimming $ string "->"
  case arr of
    Just _  -> TypeArr t <$> typeParser
    Nothing -> return t



ident :: Parser String
ident = trimming $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlpha
  return $ c : cs
  

icecream = parseFunc

parseTypeD :: Parser TypeDecl
parseTypeD = do
    a <- ident
    trimming $ char ':'
    b <- typeParser
    return $ TypeDecl a b

parseFunc :: Parser Func
parseFunc = do 
    a <- ident
    b <- many $ ident
    trimming $ char '='
    c <- ident
    return $ Func a b c

main :: IO ()
main = do
  f <- readFile "src/test.icecream"
  print $ runParser icecream f


