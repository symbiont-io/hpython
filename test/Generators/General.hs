{-# language DataKinds, TypeFamilies #-}
{-# language LambdaCase #-}
module Generators.General where

import Control.Applicative
import Control.Lens.Setter (set)
import Data.List.NonEmpty (NonEmpty(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Shrink as Shrink

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

import Generators.Common
import Generators.Sized

genParam :: (MonadGen m, Alternative m) => m (Expr '[] ()) -> m (Param '[] ())
genParam genExpr =
  thresholds
    [ (Nothing, PositionalParam () <$> genIdent)
    , (Just 2, KeywordParam () <$> genIdent <*> genWhitespaces <*> genExpr)
    , (Just 2, StarParam () <$> genWhitespaces <*> genIdent)
    , (Just 2, DoubleStarParam () <$> genWhitespaces <*> genIdent)
    ]

genArg :: (MonadGen m, Alternative m) => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr =
  thresholds
    [ (Just 2, PositionalArg () <$> genExpr)
    , (Just 2, KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr)
    , (Just 2, StarArg () <$> genWhitespaces <*> genExpr)
    , (Just 2, DoubleStarArg () <$> genWhitespaces <*> genExpr)
    ]

genInt :: (MonadGen m, Alternative m) => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^32) (2^32)) <*> genWhitespaces

genIdent :: (MonadGen m, Alternative m) => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  genWhitespaces

genModuleName :: (MonadGen m, Alternative m) => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: (MonadGen m, Alternative m) => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot <*>
    genWhitespaces
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: (MonadGen m, Alternative m) => m (ImportTargets '[] ())
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

genBlock :: (MonadGen m, Alternative m) => m (Block '[] ())
genBlock =
  Gen.shrink (\(Block (x :| xs)) -> Block . (x :|) <$> Shrink.list xs) $ do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Block <$>
      sizedNonEmpty
        ((,,) () indent <$>
         Gen.choice
         [ Right <$> genStatement
         , fmap Left $ (,) <$> Gen.maybe genComment <*> genNewline
         ])

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: (MonadGen m, Alternative m) => m (Expr '[] ())
genExpr = genExpr' False

genExpr' :: (MonadGen m, Alternative m) => Bool -> m (Expr '[] ())
genExpr' isExp =
  thresholds
  [ (Nothing, genBool)
  , (Nothing, if isExp then genSmallInt else genInt)
  , (Nothing, Ident () <$> genIdent)
  , ( Nothing
    , String () <$>
      Gen.maybe genStringPrefix <*>
      genStringType <*>
      genString <*>
      genWhitespaces
    )
  , ( Just 2
    , List () <$>
      genWhitespaces <*>
      genCommaSep genExpr <*>
      genWhitespaces
    )
  , ( Just 2
    , Gen.subtermM
        genExpr
        (\a ->
            Deref () <$>
            pure a <*>
            genWhitespaces <*>
            genIdent)
    )
  , ( Just 2
    , Gen.shrink
        (\case
            Call () a _ (CommaSepOne b) _ -> [a, _argExpr b]
            Call () a _ _ _ -> [a]
            _ -> []) $
        sized2M
          (\a b -> Call () a <$> genWhitespaces <*> pure b <*> genWhitespaces)
          genExpr
          (genCommaSep $ genArg genExpr)
    )
  , ( Just 3
    , do
        op <- genOp
        sized2
          (\a b -> BinOp () a op b)
          genExpr
          (genExpr' $ case op of; Exp{} -> True; _ -> False)
    )
  , ( Just 2
    , Gen.subtermM
        (genExpr' isExp)
        (\a -> Parens () <$> genWhitespaces <*> pure a <*> genWhitespaces)
    )
  , (Just 2, genTuple genExpr)
  , (Just 2, Not () <$> genWhitespaces <*> genExpr)
  , (Just 2, ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces)
  ]

genCompFor :: (MonadGen m, Alternative m) => m (CompFor '[] ())
genCompFor =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 1 $ n-1)
    n2 <- Gen.integral (Range.constant 1 $ n-n1)
    sized2M
      (\a b ->
         CompFor () <$>
         fmap NonEmpty.toList genWhitespaces1 <*>
         pure a <*>
         fmap NonEmpty.toList genWhitespaces1 <*>
         pure b)
      genExpr
      genExpr

genCompIf :: (MonadGen m, Alternative m) => m (CompIf '[] ())
genCompIf =
  CompIf () <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr

genComprehension :: (MonadGen m, Alternative m) => m (Comprehension '[] ())
genComprehension =
  sized3
    (Comprehension ())
    genExpr
    genCompFor
    (sizedList
       (Gen.choice
          [ Left . set whitespaceAfter [Space] <$> genCompFor
          , Right . set whitespaceAfter [Space] <$> genCompIf
          ]))

genSmallStatement :: (MonadGen m, Alternative m) => m (SmallStatement '[] ())
genSmallStatement =
  thresholds
  [ (Nothing, pure $ Pass ())
  , (Nothing, pure $ Break ())
  , (Nothing, pure $ Continue ())
  , (Just 2, Expr () <$> genExpr)
  , ( Just 3
    , sized2M
        (\a b -> Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b)
        genExpr
        genExpr
    )
  , (Just 2, Global () <$> genWhitespaces1 <*> genCommaSep1 genIdent)
  , (Just 2, Del () <$> genWhitespaces1 <*> genCommaSep1 genIdent)
  , (Just 2, Nonlocal () <$> genWhitespaces1 <*> genCommaSep1 genIdent)
  , (Just 2, Return () <$> genWhitespaces <*> genExpr)
  , ( Just 2
    , Import () <$>
      genWhitespaces1 <*>
      genCommaSep1 (genImportAs genModuleName genIdent)
    )
  , ( Nothing
    , From () <$>
      genWhitespaces <*>
      genRelativeModuleName <*>
      genWhitespaces <*>
      genImportTargets
    )
  , ( Nothing
    , Raise () <$>
      genWhitespaces <*>
      sizedMaybe
        ((,) <$> genExpr <*> sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
    )
  ]

genCompoundStatement :: (MonadGen m, Alternative m) => m (CompoundStatement '[] ())
genCompoundStatement =
  Gen.choice
    [ sized2M
        (\a b ->
            Fundef () <$>
            genWhitespaces1 <*> genIdent <*>
            genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*>
            genNewline <*> pure b)
        (genCommaSep $ genParam genExpr)
        genBlock
    , sized3M
        (\a b c ->
            If () <$> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genNewline <*> pure b <*> pure c)
        genExpr
        genBlock
        (sizedMaybe $
          (,,,) <$>
          genWhitespaces <*>
          genWhitespaces <*>
          genNewline <*>
          genBlock)
    , sized2M
        (\a b ->
           While () <$> genWhitespaces <*> pure a <*>
           genWhitespaces <*> genNewline <*> pure b)
        genExpr
        genBlock
    , sized4M
        (\a b c d ->
            TryExcept () <$>
            genWhitespaces <*> genWhitespaces <*> genNewline <*>
            pure a <*> pure b <*> pure c <*> pure c)
        genBlock
        (sizedNonEmpty $
          sized2M
            (\a b ->
              (,,,,) <$>
              genWhitespaces <*>
              pure a <*>
              genWhitespaces <*>
              genNewline <*>
              pure b)
            (ExceptAs () <$> genExpr <*> sizedMaybe ((,) <$> genWhitespaces <*> genIdent))
            genBlock)
        (sizedMaybe $
          (,,,) <$>
          genWhitespaces <*>
          genWhitespaces <*>
          genNewline <*>
          genBlock)
        (sizedMaybe $
          (,,,) <$>
          genWhitespaces <*>
          genWhitespaces <*>
          genNewline <*>
          genBlock)
    , sized2M
        (\a b ->
           TryFinally () <$>
           genWhitespaces <*> genWhitespaces <*> genNewline <*> pure a <*>
           genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b)
        genBlock
        genBlock
    , sized2M
        (\a b ->
           ClassDef () <$>
           genWhitespaces1 <*>
           genIdent <*>
           pure a <*>
           genWhitespaces <*>
           genNewline <*>
           pure b)
        (sizedMaybe $
         (,,) <$>
         genWhitespaces <*>
         sizedMaybe (genCommaSep1 $ genArg genExpr) <*>
         genWhitespaces)
        genBlock
    , sized4M
        (\a b c d ->
           For () <$> genWhitespaces <*> pure a <*> genWhitespaces <*> pure b <*>
           genWhitespaces <*> genNewline <*> pure c <*> pure d)
        genExpr
        genExpr
        genBlock
        (sizedMaybe $
         (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> genBlock)
    ]

genStatement :: (MonadGen m, Alternative m) => m (Statement '[] ())
genStatement =
  Gen.choice
  [ sized2M
      (\a b -> SmallStatements a b <$> Gen.maybe genWhitespaces <*> genNewline)
      genSmallStatement
      (sizedList $ (,) <$> genWhitespaces <*> genSmallStatement)
  , CompoundStatement <$> genCompoundStatement
  ]

genModule :: (MonadGen m, Alternative m) => m (Module '[] ())
genModule =
  Module <$>
  sizedList
    (Gen.choice
     [ fmap Left $ (,,) <$> genWhitespaces <*> Gen.maybe genComment <*> genNewline
     , Right <$> genStatement
     ])
