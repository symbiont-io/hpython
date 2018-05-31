{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Generators.Correct where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Plated
import Control.Lens.Prism (_Just)
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.TH
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup ((<>))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Shrink as Shrink
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Stack

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax

import Generators.Common
import Generators.Sized

initialGenState =
  GenState
  { _inFunction = Nothing
  , _currentNonlocals = []
  , _willBeNonlocals = []
  , _inLoop = False
  }

data GenState
  = GenState
  { _inFunction :: Maybe [String]
  , _currentNonlocals :: [String]
  , _willBeNonlocals :: [String]
  , _inLoop :: Bool
  }
makeLenses ''GenState

localState m = do
  a <- get
  b <- m
  put a
  pure b

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  Gen.filter (\i -> _identValue i `notElem` reservedWords) $
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  genWhitespaces

genModuleName :: MonadGen m => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: MonadGen m => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot <*>
    genWhitespaces
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: MonadGen m => m (ImportTargets '[] ())
genImportTargets =
  thresholds
  [ (Nothing, ImportAll () <$> genWhitespaces)
  , ( Just 2
    , ImportSome () <$>
      genCommaSep1 (genImportAs genIdent genIdent)
    )
  , ( Just 2
    , ImportSomeParens () <$>
      genWhitespaces <*>
      genCommaSep1' (genImportAs genIdent genIdent) <*>
      genWhitespaces
    )
  ]

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant 0 (2^32)) <*> genWhitespaces

genBlock :: (MonadGen m, MonadState GenState m) => m (Block '[] ())
genBlock =
  Gen.shrink (\(Block (x :| xs)) -> shrinkIndents . Block . (x :|) <$> tails xs) $ do
    indent <- NonEmpty.toList <$> genWhitespaces1
    Block <$> go indent
  where
    shrinkIndents (Block ls) = Block $ (\(a, b, c) -> (a, [head b], c)) <$> ls

    go indent =
      sizedNonEmpty
        ((,,) () indent <$>
         Gen.choice
           [ fmap Left $ (,) <$> Gen.maybe genComment <*> genNewline
           , Right <$> genStatement
           ])

genPositionalArg :: MonadGen m => m (Arg '[] ())
genPositionalArg =
  thresholds
    [ (Just 2, PositionalArg () <$> genExpr)
    , (Just 2, StarArg () <$> genWhitespaces <*> genExpr)
    ]

genKeywordArg :: MonadGen m => m (Arg '[] ())
genKeywordArg =
  thresholds
    [ (Just 2, KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr)
    , (Just 2, DoubleStarArg () <$> genWhitespaces <*> genExpr)
    ]

genArgs :: MonadGen m => m (CommaSep (Arg '[] ()))
genArgs =
  sized2
    appendCommaSep
    (genCommaSep genPositionalArg)
    (genCommaSep genKeywordArg)

genArgs1 :: MonadGen m => m (CommaSep1 (Arg '[] ()))
genArgs1 = sized2 (<>) (genCommaSep1 genPositionalArg) (genCommaSep1 genKeywordArg)

genPositionalParams :: MonadGen m => m (CommaSep (Param '[] ()))
genPositionalParams =
  Gen.sized $
  fmap (listToCommaSep . fmap (PositionalParam ())) . Gen.shrink Shrink.list . go []
  where
    go seen 0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      (i :) <$> go (_identValue i : seen) (n-1)

genKeywordParam :: MonadGen m => [String] -> m (Param '[] ())
genKeywordParam positionals =
  KeywordParam () <$>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  genWhitespaces <*>
  genExpr

genStarParam :: MonadGen m => [String] -> m (Param '[] ())
genStarParam positionals =
  StarParam () <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent

genDoubleStarParam :: MonadGen m => [String] -> m (Param '[] ())
genDoubleStarParam positionals =
  DoubleStarParam () <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent

genParams :: MonadGen m => m (CommaSep (Param '[] ()))
genParams =
  flip evalStateT [] $
  sized4
    (\a b c d ->
       appendCommaSep
         (a `appendCommaSep` maybe CommaSepNone CommaSepOne b)
         (c `appendCommaSep` maybe CommaSepNone CommaSepOne d))
    (do
       pparams <- genPositionalParams
       put (pparams ^.. folded.paramName.identValue)
       pure pparams)

    (do
       pparamNames <- get
       sp <- Gen.maybe $ genStarParam pparamNames
       modify (<> (sp ^.. _Just.paramName.identValue))
       pure sp)

    (do
        pparamNames <- get
        kwparams <- genCommaSep (genKeywordParam pparamNames)
        modify (<> (kwparams ^.. folded.paramName.identValue))
        pure kwparams)

    (get >>= Gen.maybe . genDoubleStarParam)

genList :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\case
        List _ _ (CommaSepOne e) _ -> [e]
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  genCommaSep genExpr' <*>
  genWhitespaces

genParens :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genParens genExpr' = Parens () <$> genWhitespaces <*> genExpr' <*> genWhitespaces

genDeref :: MonadGen m => m (Expr '[] ())
genDeref =
  Gen.subtermM
    genExpr
    (\a ->
        Deref () a <$>
        genWhitespaces <*>
        genIdent)

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False Nothing

genExprIdents :: MonadGen m => [Ident '[] ()] -> m (Expr '[] ())
genExprIdents is = genExpr' False (Just is)

genExpr' :: MonadGen m => Bool -> Maybe [Ident '[] ()] -> m (Expr '[] ())
genExpr' isExp idents =
  thresholds
    [ (Nothing, genBool)
    , (Nothing, if isExp then genSmallInt else genInt)
    , ( Nothing
      , Ident () <$>
        maybe
          genIdent
          (\is -> foldr (\_ _ -> Gen.element is) genIdent is)
          idents
      )
    , ( Nothing
      , String () <$>
        Gen.maybe genStringPrefix <*>
        genStringType <*>
        genString <*>
        genWhitespaces
      )
    , (Just 2, genList $ genExpr' False idents)
    , (Just 2, genDeref)
    , (Just 2, genParens $ genExpr' isExp idents)
    , ( Just 2
      , sized2M
          (\a b -> Call () a <$> genWhitespaces <*> pure b <*> genWhitespaces)
          (genExpr' False idents)
          genArgs
      )
    , ( Just 2
      , do
          op <- genOp
          sized2
            (\a ->
               BinOp ()
                 (a & whitespaceAfter .~ [Space])
                 (op & whitespaceAfter .~ [Space]))
            genExpr
            genExpr
      )
    , (Just 2, genTuple genExpr)
    , (Just 2, Not () <$> (NonEmpty.toList <$> genWhitespaces1) <*> genExpr' False idents)
    , (Just 2, ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces)
    ]

genCompFor :: MonadGen m => m ([Ident '[] ()], CompFor '[] ())
genCompFor =
  sized2M
    (\a b ->
      fmap ((,) (a ^.. targets)) $
        CompFor () <$>
        fmap NonEmpty.toList genWhitespaces1 <*>
        pure (a & whitespaceAfter .~ [Space]) <*>
        fmap NonEmpty.toList genWhitespaces1 <*>
        pure (b & whitespaceAfter .~ [Space]))
     genAssignable
     genExpr

genCompIf :: MonadGen m => m (CompIf '[] ())
genCompIf =
  CompIf () <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr

genComprehension :: MonadGen m => m (Comprehension '[] ())
genComprehension = do
  (bound, cf, conds) <-
    sized2
      (\(bound, cf) conds -> (bound, cf, conds))
      genCompFor
      (sizedList $
       Gen.choice
         [ do
             (bound', cf') <- genCompFor
             pure (bound', Left $ cf' & whitespaceAfter .~ [Space])
         , do
             ci' <- genCompIf
             pure ([], Right $ ci' & whitespaceAfter .~ [Space])
         ])
  let
    bounds = bound <> (conds >>= fst)
    conds' = snd <$> conds
  Comprehension () <$>
    genExprIdents bounds <*>
    pure cf <*>
    pure conds'

genAssignable :: MonadGen m => m (Expr '[] ())
genAssignable =
  thresholds
    [ (Nothing, Ident () <$> genIdent)
    , (Just 2, genList genAssignable)
    , (Just 2, genParens genAssignable)
    , (Just 2, genTuple genAssignable)
    , (Just 2, genDeref)
    ]

genSmallStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (SmallStatement '[] ())
genSmallStatement = do
  ctxt <- get
  nonlocals <- use currentNonlocals
  thresholds $
    [(Nothing, pure $ Pass ())] ++
    [(Nothing, pure $ Break ()) | _inLoop ctxt] ++
    [(Nothing, pure $ Continue ()) | _inLoop ctxt] ++
    [ (Just 2, Expr () <$> genExpr)
    , ( Just 2
      , sizedBind genAssignable $ \a -> do
          isInFunction <- use inFunction
          when
            (isJust isInFunction)
            (willBeNonlocals %= ((a ^.. cosmos._Ident._2.identValue) ++))
          Assign () a <$> genWhitespaces <*> genWhitespaces <*> genExpr
      )
    , ( Just 2
      , Global () <$>
        genWhitespaces1 <*>
        genCommaSep1 genIdent
      )
    , ( Just 2
      , Del () <$>
        genWhitespaces1 <*>
        genCommaSep1 genIdent
      )
    , ( Just 2
      , Import () <$>
        genWhitespaces1 <*>
        genCommaSep1 (genImportAs genModuleName genIdent)
      )
    , ( Just 2
      , From () <$>
        genWhitespaces <*>
        (genRelativeModuleName & mapped.whitespaceAfter .~ [Space]) <*>
        (NonEmpty.toList <$> genWhitespaces1) <*>
        genImportTargets
      )
    , ( Just 2
      , Raise () <$>
        fmap NonEmpty.toList genWhitespaces1 <*>
        Gen.maybe
          ((,) <$>
            set (mapped.whitespaceAfter) [Space] genExpr <*>
            Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr))
      )
    ] ++
    [ ( Just 2
      , do
          nonlocals <- use currentNonlocals
          Nonlocal () <$>
            genWhitespaces1 <*>
            (genCommaSep1 . Gen.element $ MkIdent () <$> nonlocals <*> pure [])
      )
    | isJust (_inFunction ctxt) && not (null nonlocals)
    ] ++
    [ ( Just 2
      , Return () <$>
        fmap NonEmpty.toList genWhitespaces1 <*>
        genExpr
      )
    | isJust (_inFunction ctxt)
    ]

genCompoundStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (CompoundStatement '[] ())
genCompoundStatement =
  Gen.choice
    [ sized2M
        (\a b ->
           Fundef () <$> genWhitespaces1 <*> genIdent <*>
           genWhitespaces <*> pure a <*> genWhitespaces <*> genWhitespaces <*>
           genNewline <*> pure b)
        (do
            ps <- genParams
            let paramIdents = ps ^.. folded.paramName.identValue
            modify $ \ctxt ->
              ctxt
              { _inLoop = False
              , _inFunction =
                  fmap
                    (`union` paramIdents)
                    (_inFunction ctxt) <|>
                  Just paramIdents
              , _currentNonlocals = _willBeNonlocals ctxt <> _currentNonlocals ctxt
              }
            pure ps)
        genBlock
    , sized3M
        (\a b c ->
           If () <$> fmap NonEmpty.toList genWhitespaces1 <*>
           pure a <*> genWhitespaces <*> genNewline <*> pure b <*>
           pure c)
        genExpr
        genBlock
        (sizedMaybe $
         (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> genBlock)
    , sized2M
        (\a b ->
           While () <$>
           fmap NonEmpty.toList genWhitespaces1 <*> pure a <*> genWhitespaces <*>
           genNewline <*> pure b)
        genExpr
        genBlock
    , sized4M
        (\a b c d ->
           TryExcept () <$> genWhitespaces <*> genWhitespaces <*> genNewline <*>
           pure a <*> pure b <*> pure c <*> pure d)
        genBlock
        (sizedNonEmpty $
         (,,,,) <$>
         fmap NonEmpty.toList genWhitespaces1 <*>
         (ExceptAs () <$>
          (genExpr & mapped.whitespaceAfter .~ [Space]) <*>
          Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genIdent)) <*>
         genWhitespaces <*> genNewline <*> genBlock)
        (sizedMaybe $ (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> genBlock)
        (sizedMaybe $ (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> genBlock)
    , sized2M
        (\a b ->
           TryFinally () <$>
           genWhitespaces <*> genWhitespaces <*> genNewline <*> pure a <*>
           genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b)
        genBlock
        genBlock
    , sized2M
        (\a b ->
           ClassDef () <$> genWhitespaces1 <*> genIdent <*> pure a <*>
           genWhitespaces <*> genNewline <*> pure b)
        (sizedMaybe $
         (,,) <$>
         genWhitespaces <*>
         sizedMaybe genArgs1 <*>
         genWhitespaces)
        genBlock
    , sized4M
        (\a b c d ->
           For () <$>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           pure (a & whitespaceAfter .~ [Space]) <*>
           fmap NonEmpty.toList genWhitespaces1 <*> pure b <*>
           genWhitespaces <*> genNewline <*>
           pure c <*> pure d)
        genAssignable
        genExpr
        genBlock
        (sizedMaybe $
         (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> genBlock)
    ]

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  Gen.choice
    [ CompoundStatement <$> localState genCompoundStatement
    , sized2M
        (\a b -> SmallStatements a b <$> Gen.maybe genWhitespaces <*> genNewline)
        (localState genSmallStatement)
        (sizedList $ (,) <$> genWhitespaces <*> localState genSmallStatement)
    ]
