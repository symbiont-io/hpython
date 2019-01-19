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
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import Data.Validation (Validation, toEither)
import Language.Python.Internal.Lexer (SrcInfo)
import Language.Python.Internal.Render (renderModule, showRenderOutput)
import Language.Python.Internal.Token ()
import Language.Python.Optics
import Language.Python.Parse (ParseError, parseModule)
import Language.Python.Render
import Language.Python.Syntax
import System.Environment (getArgs)
import System.Exit
import qualified System.FilePath as File



toPython :: String -> String -> Either String (Module '[] SrcInfo)
toPython fp =  bimap show id . toEither . parseModule' fp . pack
  where
    parseModule' :: String -> Text -> Validation (NonEmpty (ParseError SrcInfo)) (Module '[] SrcInfo)
    parseModule' = parseModule

-- |Parse a python file into a `Module` definition
--
-- `fail`s if the file cannot be parsed
readPy :: FilePath -> IO (Module '[] SrcInfo)
readPy f = do
  code <- readFile f
  let res = toPython f code
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

buildDependencyGraph :: FilePath -> Module '[] SrcInfo -> [ (Node, Node) ]
buildDependencyGraph fp = allCalls ns . allDefinitions . allExpressions
  where
    ns = File.takeBaseName fp

allCalls :: String
         -> [(Ident '[] SrcInfo,
               Suite '[] SrcInfo)]
         -> [(Node, Node)]
allCalls ns = concatMap (uncurry $ allCallExprs ns)

allCallExprs :: String
             -> Ident '[] SrcInfo
             -> Suite '[] SrcInfo
             -> [(Node, Node)]
allCallExprs ns (_identValue -> caller) body =
 let calls = toListOf (_Exprs._Call.callArguments.folded.folded.argExpr) body
 in fmap ( \c -> (Node ns caller, extractNode c)) calls
 where
   builtins = [ "Identifier", "cast", "PostTxArgs", "ChannelName" ]
   extractNode (Ident _ (_identValue -> called))
     | called `notElem` builtins = Node ns called
     | otherwise = Node "cvm" called
   extractNode (Deref _ l _ identifier) = Node (unpack $ showExpr l) $ _identValue identifier
   extractNode e = Node "<?>" (unpack $ showExpr e)

allDefinitions :: [ Statement '[] SrcInfo ] -> [ (Ident '[] SrcInfo, Suite '[] SrcInfo) ]
allDefinitions = concatMap (toListOf (_Fundef . to makeFun))
  where
     makeFun (MkFundef _ _ _ _ _ i _ _ _ _ s) = (i,s)
--(Fundef _ decos idnt asyncWs ws1 name ws2 params ws3 mty s)
allExpressions :: Module '[] SrcInfo -> [ Statement '[] SrcInfo ]
allExpressions = toListOf _Statements
