{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Icecream where

import Parser
import BasePrelude hiding (try)
import qualified Data.Map as M
import Control.Lens


data FuncDecl = FuncDecl
  { fdId       :: String
  , fdTypeDecl :: Maybe Type
  , fdFuncs    :: [Func]
  } deriving Show


data TLD = TypeDeclTLD TypeDecl
         | FuncTLD Func
         deriving Show

getRelevantName :: TLD -> String
getRelevantName (TypeDeclTLD td) = tdId td
getRelevantName (FuncTLD f)      = fId f



data TypeDecl = TypeDecl { tdId :: String, tdType :: Type }
          deriving Show

data Type = TypeCtr String
          | TypeArr Type Type
          deriving Show

data Func = Func { fId :: String, fBinds :: [String], fBody :: Expr }
          deriving Show

type Expr = String

makePrisms ''TLD


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


icecream = makeFuncDecls <$> many parseTLD

parseTLD :: Parser TLD
parseTLD = do
  trimmerDinger
  r <- oneOf [ TypeDeclTLD <$> parseTypeD
             , FuncTLD     <$> parseFunc
             ]
  trimmerDinger
  return r


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

trimmerDinger :: Parser ()
trimmerDinger = void . many $ char '\n'

main :: IO ()
main = do
  f <- readFile "src/test.icecream"
  print $ runParser icecream f



makeFuncDecls :: [TLD] -> [FuncDecl]
makeFuncDecls = fmap (uncurry makeFuncDecls2)
              . M.toList
              . M.fromListWith (++)
              . fmap (\tld -> (getRelevantName tld, [tld]))



-- TODO(sandy): they are all the same relevant name
makeFuncDecls2 :: String -> [TLD] -> FuncDecl
makeFuncDecls2 name tlds =
  let (yes, nos) = partition (has _TypeDeclTLD) tlds
   in FuncDecl name
               (fmap (\(TypeDeclTLD t) -> tdType t) $ listToMaybe yes)
               (fmap (\(FuncTLD f) -> f) nos)



