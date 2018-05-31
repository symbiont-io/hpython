{-# language DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Language.Python.Internal.Optics
import Language.Python.Internal.Parse
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error

import Scope
import Roundtrip

import Generators.Common (GenWhitespaces(..), genWhitespaces, genWhitespaces1)
import qualified Generators.General as General
import qualified Generators.Correct as Correct

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader (runReaderT)
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validate
import System.Directory
import System.Exit
import System.Process
import qualified Text.Trifecta as Trifecta

import Hedgehog
import qualified Hedgehog.Gen as Gen

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Expr '[Syntax, Indentation] a)
validateExprSyntax' = runValidateSyntax initialSyntaxContext [] . validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validate [IndentationError '[] a] (Expr '[Indentation] a)
validateExprIndentation' = validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Statement '[Syntax, Indentation] a)
validateStatementSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateStatementSyntax

validateStatementIndentation'
  :: Statement '[] a
  -> Validate [IndentationError '[] a] (Statement '[Indentation] a)
validateStatementIndentation' = validateStatementIndentation

validateModuleSyntax'
  :: Module '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Module '[Syntax, Indentation] a)
validateModuleSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateModuleSyntax

validateModuleIndentation'
  :: Module '[] a
  -> Validate [IndentationError '[] a] (Module '[Indentation] a)
validateModuleIndentation' = validateModuleIndentation

runPython3 :: (MonadTest m, MonadIO m) => FilePath -> Bool -> String -> m ()
runPython3 path shouldSucceed str = do
  () <- liftIO $ writeFile path str
  (ec, sto, ste) <- liftIO $ readProcessWithExitCode "python3" ["-m", "py_compile", path] ""
  annotateShow shouldSucceed
  annotateShow ec
  annotate sto
  annotate ste
  case (shouldSucceed, ec) of
    (True, ExitSuccess) -> success
    (True, ExitFailure{})
      | "SyntaxError" `isInfixOf` last (lines ste) -> failure
      | otherwise -> success
    (False, ExitSuccess) -> failure
    (False, ExitFailure{}) -> success

simpleWs :: GenWhitespaces
simpleWs = GenWhitespaces (pure [Space]) (pure $ Space :| [])

complexWs :: GenWhitespaces
complexWs = GenWhitespaces genWhitespaces genWhitespaces1

syntax_expr :: FilePath -> GenWhitespaces -> Property
syntax_expr path genWs =
  property $ do
    ex <- forAll . Gen.resize 300 $ runReaderT General.genExpr genWs
    let rex = renderExpr ex
    shouldSucceed <-
      case validateExprIndentation' ex of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateExprSyntax' res of
            Failure errs' ->
              let
                errs'' = filter (hasn't _MissingSpacesIn) errs'
              in
                if null errs''
                then pure True
                else annotateShow errs'' $> False
            Success res' -> pure True
    annotate rex
    runPython3
      path
      shouldSucceed
      rex

syntax_statement :: FilePath -> GenWhitespaces -> Property
syntax_statement path genWs =
  property $ do
    st <- forAll $ Gen.resize 300 $ runReaderT General.genStatement genWs
    let rst = renderLines $ renderStatement st
    shouldSucceed <-
      case validateStatementIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateStatementSyntax' res of
            Failure errs' ->
              let
                errs'' = filter (hasn't _MissingSpacesIn) errs'
              in
                if null errs''
                then pure True
                else annotateShow errs'' $> False
            Success res' -> pure True
    annotate rst
    runPython3 path shouldSucceed rst

syntax_module :: FilePath -> GenWhitespaces -> Property
syntax_module path genWs =
  property $ do
    st <- forAll . Gen.resize 300 $ runReaderT General.genModule genWs
    let rst = renderModule st
    shouldSucceed <-
      case validateModuleIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateModuleSyntax' res of
            Failure errs' ->
              let
                errs'' = filter (hasn't _MissingSpacesIn) errs'
              in
                if null errs''
                then pure True
                else annotateShow errs'' $> False
            Success res' -> pure True
    annotate rst
    runPython3 path shouldSucceed rst

correct_syntax_expr :: FilePath -> Property
correct_syntax_expr path =
  property $ do
    ex <- forAll Correct.genExpr
    annotateShow $ renderExpr ex
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 path True (renderExpr ex)

correct_syntax_statement :: FilePath -> Property
correct_syntax_statement path =
  property $ do
    st <- forAll $ evalStateT Correct.genStatement Correct.initialGenState
    annotate $ renderLines (renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 path True (renderLines $ renderStatement st)

expr_printparseprint_print :: Property
expr_printparseprint_print =
  property $ do
    ex <- forAll Correct.genExpr
    annotate (renderExpr ex)
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case Trifecta.parseString (expr whitespace) mempty (renderExpr res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' -> do
                annotateShow res''
                renderExpr (res' ^. unvalidated) === renderExpr (res'' $> ())

parseStatement = Trifecta.parseString (evalStateT statement []) mempty

statement_printparseprint_print :: Property
statement_printparseprint_print =
  property $ do
    st <- forAll $ evalStateT Correct.genStatement Correct.initialGenState
    annotate (renderLines $ renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case parseStatement (renderLines $ renderStatement res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' -> do
                annotateShow res''
                renderLines (renderStatement (res' ^. unvalidated)) ===
                  renderLines (renderStatement (res'' $> ()))

statement_printparse_id :: Property
statement_printparse_id =
  property $ do
    st <- forAll $ evalStateT Correct.genStatement Correct.initialGenState
    annotate (renderLines $ renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case parseStatement (renderLines $ renderStatement res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' -> res ^. unvalidated === st

main = do
  let file = "hedgehog-test.py"
  check $ syntax_expr file simpleWs
  check $ syntax_expr file complexWs
  check $ syntax_statement file simpleWs
  check $ syntax_statement file complexWs
  check $ syntax_module file simpleWs
  check $ syntax_module file complexWs
  check $ correct_syntax_expr file
  check $ correct_syntax_statement file
  check expr_printparseprint_print
  check statement_printparseprint_print
  check statement_printparse_id
  checkParallel scopeTests
  checkParallel roundtripTests
  removeFile "hedgehog-test.py"
