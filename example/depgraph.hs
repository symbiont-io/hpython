{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import Data.Validation (Validation, toEither)
import Debug.Trace (trace)
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


--  stak exec depgraph -- ../rex/contracts/loans/loans.py  	../rex/contracts/escrow/escrow.py  	../rex/contracts/amortization/amortization.py  	../rex/contracts/loans_events/loans_events.py  	../rex/contracts/loans_schemas/loans_schemas.py  	../rex/contracts/loans_history/loans_history.py  	../rex/contracts/date/date.py  	../rex/contracts/payments/payments.py  	../rex/contracts/transfer/transfer.py  	../rex/contracts/todos/todos.py  	../rex/contracts/servicing/servicing.py  	../rex/contracts/workflow/workflow.py  	../rex/contracts/documents/documents.py | dot -Tpdf -odeps.pdf

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

allReferences ::  [ Statement '[] SrcInfo ] -> [ Expr '[] SrcInfo ]
allReferences = filter isDeref .
                concatMap (universeOn (_Exprs))
  where
    isDeref (Deref _ _ _ _) = True
    isDeref _               = False

allDefinitions :: [ Statement '[] SrcInfo ] -> [ (Ident '[] SrcInfo, Suite '[] SrcInfo) ]
allDefinitions = concatMap (toListOf (_Fundef . to makeFun))
  where
     makeFun (MkFundef _ _ _ _ _ i _ _ _ _ s) = (i,s)
--(Fundef _ decos idnt asyncWs ws1 name ws2 params ws3 mty s)
allExpressions :: Module '[] SrcInfo -> [ Statement '[] SrcInfo ]
allExpressions = toListOf _Statements

toplevelAssignments :: [ Statement '[] SrcInfo ] -> [ Statement '[] SrcInfo ]
toplevelAssignments = filter isAssignment
  where
    isAssignment  (SmallStatement _ (MkSmallStatement (Assign _ _ _) _ _ _ _)) = True
    isAssignment  _ = False

-- | Get contracts' imports
contractDefinitions :: [ Statement '[] SrcInfo ] -> [ Statement '[] SrcInfo ]
contractDefinitions = filter contractImports
  where
    contractImports (SmallStatement _ (MkSmallStatement asss _ _ _ _)) =
      case asss ^? _Exprs._Call.callFunction._Ident.identValue of
        Just "Contract" -> True
        _               -> False
    contractImports _ = False

data ContractRef = ContractRef { _name :: Text, _version :: Text }
  deriving (Eq, Show)

-- | Extract all contracts references from the given list of statements.
--
-- Given the following source code,
-- @
-- rat = Contract('rational', '1.0.0', direct=True)
-- investors = Contract('investors', '1.0.0')
-- @
--
-- This function will return
-- @
-- [ ("rat", ContractRef {_name = "'rational'", _version = "'1.0.0'"})
-- , ("investors", ContractRef {_name = "'investors'", _version = "'1.0.0'"})
-- ]
-- @
--
-- This is needed to chase external calls to other contracts while
-- chasing references within a contract.
contractsRefs :: [ Statement '[] SrcInfo ] -> [ (String, ContractRef) ]
contractsRefs = fmap mkContractRef . contractDefinitions
  where
    mkContractRef (SmallStatement _ (MkSmallStatement (Assign _ lhs rhs) _ _ _ _)) =
      ( mkLhs lhs, mkRhs rhs)
    mkContractRef _ = error "unexpected statement, should be something like 'xyz = Contract('xyz', '1.0.0')'"

    mkLhs expr = expr ^. _Ident.identValue

    mkRhs :: NonEmpty (a0, Expr '[] SrcInfo) -> ContractRef
    mkRhs expr = case args of
                   (c:v:_) -> ContractRef c v
                   others  -> error $ "don't know how to extract contract ref from "  <> show others
      where
        args = fmap asString extArgs
        rawArgs = fmap snd $ toList expr
        extArgs = concatMap (toListOf (_Call.callArguments.folded.folded.argExpr)) $ rawArgs
        asString e = showExpr e


-- | Get contracts' imports
schemaDefinitions :: [ Statement '[] SrcInfo ] -> [ Statement '[] SrcInfo ]
schemaDefinitions = filter schemaImports
  where
    schemaImports (SmallStatement _ (MkSmallStatement asss _ _ _ _)) =
      case asss ^? _Exprs._Call.callFunction._Ident.identValue of
        Just "Schema" -> True
        _             -> False
    schemaImports _ = False
