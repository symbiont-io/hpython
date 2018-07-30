{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Roundtrip
  ( roundtripTests
  ) where

import           Control.Monad                              ((<=<))
import           Control.Monad.IO.Class                     (liftIO)
import           Data.Monoid                                ((<>))
import           Data.String                                (fromString)
import           Data.Validate                              (Validate (..))
import           Hedgehog                                   (Group (..),
                                                             Property,
                                                             annotateShow,
                                                             failure, property,
                                                             withShrinks,
                                                             withTests, (===))
import           System.FilePath                            ((</>))
import           Text.Trifecta                              (Caret)
import qualified Text.Trifecta                              as Trifecta

import           Helpers                                    (doToPython)
import           Language.Python.Internal.Lexer             (indentation,
                                                             logicalLines,
                                                             nested, tokenize)
import           Language.Python.Internal.Parse             (module_, runParser)
import           Language.Python.Internal.Render            (showModule)
import           Language.Python.Internal.Token             ()
import           Language.Python.Validate.Indentation       (Indentation, runValidateIndentation,
                                                             validateModuleIndentation)
import           Language.Python.Validate.Indentation.Error (IndentationError)
import           Language.Python.Validate.Syntax            (initialSyntaxContext,
                                                             runValidateSyntax,
                                                             validateModuleSyntax)
import           Language.Python.Validate.Syntax.Error      (SyntaxError)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 . withShrinks 1 $ doRoundtrip name)) <$>
  ["weird.py", "django.py", "test.py", "ansible.py", "comments.py"]

doRoundtrip :: String -> Property
doRoundtrip name =
  property $ do
    file <- liftIO . readFile $ "test/files" </> name
    py <- doToPython module_ file
    case runValidateIndentation $ validateModuleIndentation py of
      Failure errs ->
        annotateShow (errs :: [IndentationError '[] Caret]) *> failure
      Success res ->
        case runValidateSyntax
               initialSyntaxContext
               []
               (validateModuleSyntax res) of
          Failure errs' ->
            annotateShow (errs' :: [SyntaxError '[ Indentation] Caret]) *>
            failure
          Success _ -> showModule py === file

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

toPython =
  parse module_ <=< nesteds <=< indents <=< pure . logicalLines <=< tokens

readPy :: FilePath -> IO ()
readPy f = do
  code <- readFile f
  let res = toPython code
  print res
