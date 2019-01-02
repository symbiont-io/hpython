{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import Control.Monad (when, (<=<))
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Lazy (unpack)
import Data.Validation (Validation, toEither)
import Language.Python.Internal.Lexer (SrcInfo)
import Language.Python.Internal.Render (renderModule, showRenderOutput)
import Language.Python.Internal.Token ()
import Language.Python.Parse (ParseError, parseModule)
import Language.Python.Syntax (Module)
import System.Environment (getArgs)
import System.Exit

toPython :: String -> String -> Either String (Module '[] SrcInfo)
toPython fp =  bimap show id . toEither . parseModule' fp . pack
  where
    parseModule' :: String -> Text -> Validation (NonEmpty (ParseError SrcInfo)) (Module '[] SrcInfo)
    parseModule' = parseModule

main :: IO ()
main = do
  args <- getArgs
  exits <- traverse readPy args
  when (any (/= ExitSuccess) exits) $ exitWith (ExitFailure 1)

readPy :: FilePath -> IO ExitCode
readPy f = do
  code <- readFile f
  let res = toPython f code
  either (putStrLn . ((f <> " : ") <>))  (const $ pure ()) res
  return $ either (const $ ExitFailure 1) (const ExitSuccess) res
