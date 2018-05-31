{-# language DataKinds, TypeFamilies #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
module Generators.General where

import Control.Applicative
import Control.Lens.Setter (set)
import Control.Monad.Reader (MonadReader)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

import Generators.Common
import Generators.Sized

genParam :: (MonadReader GenWhitespaces m, MonadGen m) => m (Expr '[] ()) -> m (Param '[] ())
genParam genExpr =
  sizedRecursive
    [ PositionalParam () <$> genIdent
    , DoubleStarParam () <$> whitespaces <*> genIdent
    , StarParam () <$> whitespaces <*> genIdent
    ]
    [KeywordParam () <$> genIdent <*> whitespaces <*> genExpr]

genArg :: (MonadReader GenWhitespaces m, MonadGen m) => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr =
  sizedRecursive
    []
    [ PositionalArg () <$> genExpr
    , KeywordArg () <$> genIdent <*> whitespaces <*> genExpr
    , StarArg () <$> whitespaces <*> genExpr
    , DoubleStarArg () <$> whitespaces <*> genExpr
    ]

genInt :: (MonadReader GenWhitespaces m, MonadGen m) => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^32) (2^32)) <*> whitespaces

genIdent :: (MonadReader GenWhitespaces m, MonadGen m) => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  whitespaces

genModuleName :: (MonadReader GenWhitespaces m, MonadGen m) => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    whitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: (MonadReader GenWhitespaces m, MonadGen m) => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot <*>
    whitespaces
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: (MonadReader GenWhitespaces m, MonadGen m) => m (ImportTargets '[] ())
genImportTargets =
  sizedRecursive
    [ ImportAll () <$> whitespaces ]
    [ ImportSome () <$>
      genCommaSep1 (genImportAs genIdent genIdent)
    , ImportSomeParens () <$>
      whitespaces <*>
      genCommaSep1' (genImportAs genIdent genIdent) <*>
      whitespaces
    ]

genBlock :: (MonadReader GenWhitespaces m, MonadGen m) => m (Block '[] ())
genBlock = do
  indent <- NonEmpty.toList <$> whitespaces1
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
genExpr :: (MonadReader GenWhitespaces m, MonadGen m) => m (Expr '[] ())
genExpr = genExpr' False

genExpr' :: (MonadReader GenWhitespaces m, MonadGen m) => Bool -> m (Expr '[] ())
genExpr' isExp =
  sizedRecursive
    [ genBool
    , if isExp then genSmallInt else genInt
    , Ident () <$> genIdent
    , String () <$>
      Gen.maybe genStringPrefix <*>
      genStringType <*>
      genString <*>
      whitespaces
    ]
    [ List () <$>
      whitespaces <*>
      genCommaSep genExpr <*>
      whitespaces
    , Gen.subtermM
        genExpr
        (\a ->
            Deref () <$>
            pure a <*>
            whitespaces <*>
            genIdent)
    , Gen.shrink
        (\case
            Call () a _ (CommaSepOne b) _ -> [a, _argExpr b]
            Call () a _ _ _ -> [a]
            _ -> []) $
        sized2M
          (\a b -> Call () a <$> whitespaces <*> pure b <*> whitespaces)
          genExpr
          (genCommaSep $ genArg genExpr)
    , do
        op <- genOp
        sized2
          (\a b -> BinOp () a op b)
          genExpr
          (genExpr' $ case op of; Exp{} -> True; _ -> False)
    , Gen.subtermM
        (genExpr' isExp)
        (\a -> Parens () <$> whitespaces <*> pure a <*> whitespaces)
    , genTuple genExpr
    , Not () <$> whitespaces <*> genExpr
    , ListComp () <$> whitespaces <*> genComprehension <*> whitespaces
    ]

genCompFor :: (MonadReader GenWhitespaces m, MonadGen m) => m (CompFor '[] ())
genCompFor =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 1 $ n-1)
    n2 <- Gen.integral (Range.constant 1 $ n-n1)
    sized2M
      (\a b ->
         CompFor () <$>
         fmap NonEmpty.toList whitespaces1 <*>
         pure a <*>
         fmap NonEmpty.toList whitespaces1 <*>
         pure b)
      genExpr
      genExpr

genCompIf :: (MonadReader GenWhitespaces m, MonadGen m) => m (CompIf '[] ())
genCompIf =
  CompIf () <$> fmap NonEmpty.toList whitespaces1 <*> genExpr

genComprehension :: (MonadReader GenWhitespaces m, MonadGen m) => m (Comprehension '[] ())
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

genSmallStatement :: (MonadReader GenWhitespaces m, MonadGen m) => m (SmallStatement '[] ())
genSmallStatement =
  sizedRecursive
    [ pure $ Pass ()
    , pure $ Break ()
    , pure $ Continue ()
    ]
    [ Expr () <$> genExpr
    , sized2M
        (\a b -> Assign () a <$> whitespaces <*> whitespaces <*> pure b)
        genExpr
        genExpr
    , Global () <$> whitespaces1 <*> genCommaSep1 genIdent
    , Del () <$> whitespaces1 <*> genCommaSep1 genIdent
    , Nonlocal () <$> whitespaces1 <*> genCommaSep1 genIdent
    , Return () <$> whitespaces <*> genExpr
    , Import () <$>
      whitespaces1 <*>
      genCommaSep1 (genImportAs genModuleName genIdent)
    , From () <$>
      whitespaces <*>
      genRelativeModuleName <*>
      whitespaces <*>
      genImportTargets
    , Raise () <$>
      whitespaces <*>
      sizedMaybe
        (sized2 (,)
           genExpr
           (sizedMaybe ((,) <$> whitespaces <*> genExpr)))
    ]

genCompoundStatement :: (MonadReader GenWhitespaces m, MonadGen m) => m (CompoundStatement '[] ())
genCompoundStatement =
  Gen.choice
    [ sized2M
        (\a b ->
            Fundef () <$>
            whitespaces1 <*> genIdent <*>
            whitespaces <*> pure a <*>
            whitespaces <*> whitespaces <*>
            genNewline <*> pure b)
        (genCommaSep $ genParam genExpr)
        genBlock
    , sized3M
        (\a b c ->
            If () <$> whitespaces <*> pure a <*>
            whitespaces <*> genNewline <*> pure b <*> pure c)
        genExpr
        genBlock
        (sizedMaybe $
          (,,,) <$>
          whitespaces <*>
          whitespaces <*>
          genNewline <*>
          genBlock)
    , sized2M
        (\a b ->
           While () <$> whitespaces <*> pure a <*>
           whitespaces <*> genNewline <*> pure b)
        genExpr
        genBlock
    , sized4M
        (\a b c d ->
            TryExcept () <$>
            whitespaces <*> whitespaces <*> genNewline <*>
            pure a <*> pure b <*> pure c <*> pure c)
        genBlock
        (sizedNonEmpty $
          sized2M
            (\a b ->
              (,,,,) <$>
              whitespaces <*>
              pure a <*>
              whitespaces <*>
              genNewline <*>
              pure b)
            (ExceptAs () <$> genExpr <*> sizedMaybe ((,) <$> whitespaces <*> genIdent))
            genBlock)
        (sizedMaybe $
          (,,,) <$>
          whitespaces <*>
          whitespaces <*>
          genNewline <*>
          genBlock)
        (sizedMaybe $
          (,,,) <$>
          whitespaces <*>
          whitespaces <*>
          genNewline <*>
          genBlock)
    , sized2M
        (\a b ->
           TryFinally () <$>
           whitespaces <*> whitespaces <*> genNewline <*> pure a <*>
           whitespaces <*> whitespaces <*> genNewline <*> pure b)
        genBlock
        genBlock
    , sized2M
        (\a b ->
           ClassDef () <$>
           whitespaces1 <*>
           genIdent <*>
           pure a <*>
           whitespaces <*>
           genNewline <*>
           pure b)
        (sizedMaybe $
         (,,) <$>
         whitespaces <*>
         sizedMaybe (genCommaSep1 $ genArg genExpr) <*>
         whitespaces)
        genBlock
    , sized4M
        (\a b c d ->
           For () <$> whitespaces <*> pure a <*> whitespaces <*> pure b <*>
           whitespaces <*> genNewline <*> pure c <*> pure d)
        genExpr
        genExpr
        genBlock
        (sizedMaybe $
         (,,,) <$> whitespaces <*> whitespaces <*> genNewline <*> genBlock)
    ]

genStatement :: (MonadReader GenWhitespaces m, MonadGen m) => m (Statement '[] ())
genStatement =
  Gen.choice
  [ sized2M
      (\a b -> SmallStatements a b <$> Gen.maybe whitespaces <*> genNewline)
      genSmallStatement
      (sizedList $ (,) <$> whitespaces <*> genSmallStatement)
  , CompoundStatement <$> genCompoundStatement
  ]

genModule :: (MonadReader GenWhitespaces m, MonadGen m) => m (Module '[] ())
genModule =
  Module <$>
  sizedList
    (Gen.choice
     [ fmap Left $ (,,) <$> whitespaces <*> Gen.maybe genComment <*> genNewline
     , Right <$> genStatement
     ])
