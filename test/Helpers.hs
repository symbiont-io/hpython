module Helpers where

import Control.Monad ((<=<))
import Text.Megaparsec (SourcePos, initialPos)

import qualified Data.Text.Lazy as Lazy

import Hedgehog

import Language.Python.Internal.Lexer
  (LogicalLine, IndentedLine, Nested, tokenize, logicalLines, indentation, nested)
import Language.Python.Internal.Parse (Parser, runParser)
import Language.Python.Internal.Token (PyToken)

doTokenize :: Monad m => Lazy.Text -> PropertyT m [PyToken SourcePos]
doTokenize str = do
  let res = tokenize str
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doIndentation :: (Show a, Monad m) => [LogicalLine a] -> PropertyT m [IndentedLine a]
doIndentation lls = do
  let res = indentation lls
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doNested :: Monad m => [IndentedLine a] -> PropertyT m (Nested a)
doNested ils = do
  let res = nested ils
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doParse :: (Show ann, Monad m) => ann -> Parser ann a -> Nested ann -> PropertyT m a
doParse initial pa input = do
  let res = runParser initial pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doParse' :: Monad m => Parser SourcePos a -> Nested SourcePos -> PropertyT m a
doParse' = doParse $ initialPos "test"

doToPython :: Monad m => Parser SourcePos a -> Lazy.Text -> PropertyT m a
doToPython pa =
  doParse (initialPos "test") pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize
