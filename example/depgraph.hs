{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-| Compute a dependency graph over a list of python files -}
import Control.Lens
import Control.Lens.Plated
import Control.Monad (when, (<=<))
import Data.Function (on)
import Data.Graph (Graph, Vertex, edges, graphFromEdges)
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text.Lazy (unpack)
import Language.Python.Internal.Lexer (indentation, logicalLines, nested,
                                       tokenize)
import Language.Python.Internal.Optics
import Language.Python.Internal.Parse (module_, runParser)
import Language.Python.Internal.Render (renderModule, showExpr,
                                        showRenderOutput)
import Language.Python.Internal.Syntax
import Language.Python.Internal.Syntax.Module (Module)
import Language.Python.Internal.Token ()
import System.Environment (getArgs)
import System.Exit
import qualified System.FilePath as File
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
  let calls = concatMap (uncurry buildDependencyGraph) (zip args modules)
  putStrLn $ displayGraph $ filter unwantedCalls $ calls

unwantedCalls :: ( Node, Node) -> Bool
unwantedCalls (s,t) = isContractCall s && isContractCall t
  where
    isContractCall n = _namespace n `elem` contracts
    contracts = [ "amortization"
                , "borrowers"
                , "date"
                , "documents"
                , "escrow"
                , "investors"
                , "loans"
                , "loans_events"
                , "loans_history"
                , "loans_schemas"
                , "payments"
                , "rational"
                , "servicing"
                , "todos"
                , "transfer"
                , "workflow"
                ]


data Node = Node { _namespace :: String
                 , _function  :: String
                 }
  deriving (Eq, Ord, Show)

display :: Node -> String
display (Node ns fn) = ns <> "." <> fn

displayGraph :: [(Node, Node)]
             -> String
displayGraph graph =
  unlines $ hdr <> clusters <> links <> ftr
  where
    clusters = concatMap mkCluster $
               zip [1 :: Int ..] $
               List.groupBy ((==) `on` _namespace) $
               List.sortBy (compare `on` _namespace) $
               uncurry (<>) $
               unzip graph
    mkCluster (num, nodes) = [ "  subgraph cluster_" <> show num <> " {"
                             , "    label=\"" <> _namespace (head nodes) <> "\";" ] <>
                             fmap ((\ n -> "    " <> bracketed n <> ";") . display) functions <>
                             [ "  };" ]
      where
        functions = List.nub $ List.sort nodes
    bracketed s = "\"" <> s <> "\""
    links = fmap mkEdges $ List.nub $ List.sort $ graph
    mkEdges (s, t) = "  \"" <> display s <> "\" -> \"" <> display t <> "\";"
    hdr = [ "digraph G {"
          , "concentrate = true;"
          , "rankdir = LR;"
          ]
    ftr = [ "}" ]

buildDependencyGraph :: FilePath -> Module '[] Trifecta.Caret -> [ (Node, Node) ]
buildDependencyGraph fp = allCalls ns . allDefinitions . allExpressions
  where
    ns = File.takeBaseName fp

allCalls :: String
         -> [(Ident '[] Trifecta.Caret,
               Suite '[] Trifecta.Caret)]
         -> [(Node, Node)]
allCalls ns = concatMap (uncurry $ allCallExprs ns)

allCallExprs :: String
             -> Ident '[] Trifecta.Caret
             -> Suite '[] Trifecta.Caret
             -> [(Node, Node)]
allCallExprs ns (_identValue -> caller) body =
 let calls = toListOf (_Exprs._Call._2) body
 in fmap ( \c -> (Node ns caller, extractNode c)) calls
 where
   builtins = [ "Identifier", "cast", "PostTxArgs", "ChannelName" ]
   extractNode (Ident _ (_identValue -> called))
     | called `notElem` builtins = Node ns called
     | otherwise = Node "cvm" called
   extractNode (Deref _ l _ identifier) = Node (unpack $ showExpr l) $ _identValue identifier
   extractNode e = Node "<?>" (unpack $ showExpr e)

allDefinitions :: [ Statement '[] Trifecta.Caret ] -> [ (Ident '[] Trifecta.Caret, Suite '[] Trifecta.Caret) ]
allDefinitions = concatMap (toListOf (_Fundef . to makeFun))
  where
     makeFun (_,_,_,_,i,_,_,_,_,s) = (i,s)

allExpressions :: Module '[] Trifecta.Caret -> [ Statement '[] Trifecta.Caret ]
allExpressions = toListOf _Statements
