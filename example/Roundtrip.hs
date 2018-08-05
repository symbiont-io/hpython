{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Text.Lazy (unpack)
import Language.Python.Internal.Lexer (indentation, logicalLines, nested,
                                       tokenize)
import Language.Python.Internal.Parse (module_, runParser)
import Language.Python.Internal.Render (renderModule, showRenderOutput)
import Language.Python.Internal.Syntax.Module (Module)
import Language.Python.Internal.Token ()
import System.Environment (getArgs)
import qualified Text.Trifecta as Trifecta

tokens str = do
  let res = tokenize str
  case res of
    Trifecta.Failure err -> Left $ "error tokenizing " <> show err
    Trifecta.Success a   -> pure a

indents lls = do
  let res = indentation lls
  case res of
    Left err -> Left $ "error checking indentation " <> show err
    Right a  -> pure a

nesteds ils = do
  let res = nested ils
  case res of
    Left err -> Left $ "error nested " <> show err
    Right a  -> pure a

parse pa input = do
  let res = runParser (Trifecta.Caret mempty mempty) pa input
  case res of
    Left err -> Left $ "error parsing " <> show err
    Right a  -> pure a

toPython :: String -> Either String (Module '[] Trifecta.Caret)
toPython =
  parse module_ <=< nesteds <=< indents <=< pure . logicalLines <=< tokens

main :: IO ()
main = do
  args <- getArgs
  traverse_ readPy args

readPy :: FilePath -> IO ()
readPy f = do
  code <- readFile f
  let res = toPython code
  putStrLn $ either id (unpack . showRenderOutput . renderModule) res
