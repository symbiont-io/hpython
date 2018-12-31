{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-| Compute a dependency graph over a list of python files -}
import Control.Lens
import Control.Lens.Plated
import Control.Monad (when, (<=<))
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Text.Lazy (unpack)
import Language.Python.Internal.Lexer (indentation, logicalLines, nested,
                                       tokenize)
import Language.Python.Internal.Optics
import Language.Python.Internal.Parse (module_, runParser)
import Language.Python.Internal.Render (renderModule, showRenderOutput)
import Language.Python.Internal.Syntax
import Language.Python.Internal.Syntax.Module (Module)
import Language.Python.Internal.Token ()
import System.Environment (getArgs)
import System.Exit
import qualified Text.Trifecta as Trifecta


toPython :: String -> Either String (Module '[] Trifecta.Caret)
toPython =
  parse module_ <=< nesteds <=< indents <=< pure . logicalLines <=< tokens
  where
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

-- |Parse a python file into a `Module` definition
--
-- `fail`s if the file cannot be parsed
readPy :: FilePath -> IO (Module '[] Trifecta.Caret)
readPy f = do
  code <- readFile f
  let res = toPython code
  either (fail . (("error in file " <> f <> " : ") <>)) pure res

main :: IO ()
main = do
  args <- getArgs
  modules <- traverse readPy args
  let deps :: [String] = fmap _identValue $ buildDependencyGraph modules
  mapM_ putStrLn deps


-- type Node = String

-- type Graph = [ Node, [ Node ] ]

buildDependencyGraph :: [ Module '[] Trifecta.Caret ] -> [ Ident '[] Trifecta.Caret ]
buildDependencyGraph = allDefinitions . allExpressions

allDefinitions :: [ Statement '[] Trifecta.Caret ] -> [ Ident '[] Trifecta.Caret ]
allDefinitions = fmap _getIdent . concatMap (toListOf _Fundef)
  where
    _getIdent (_,_,_,_,i,_,_,_,_,_) = i

allExpressions :: [ Module '[] Trifecta.Caret ] -> [ Statement '[] Trifecta.Caret ]
allExpressions = concatMap (toListOf _Statements)
