{-# language DataKinds #-}
module Generators.Common where

import Control.Applicative (Alternative)
import Data.List.NonEmpty (NonEmpty(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.Internal.Syntax

import Generators.Sized

genSmallInt :: (MonadGen m, Alternative m) => m (Expr '[] ())
genSmallInt = Int () <$> Gen.integral (Range.constant 0 100) <*> genWhitespaces

genString :: (MonadGen m, Alternative m) => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

genNewline :: (MonadGen m, Alternative m) => m Newline
genNewline = Gen.element [LF, CR, CRLF]

genStringType :: (MonadGen m, Alternative m) => m StringType
genStringType = Gen.element [ShortSingle, ShortDouble, LongSingle, LongDouble]

genAnyWhitespace :: (MonadGen m, Alternative m) => m Whitespace
genAnyWhitespace =
  Gen.choice
    [ pure Space
    , pure Tab
    , Newline <$> genNewline
    , Continued <$>
      genNewline <*>
      Gen.list
        (Range.constant 0 10)
        (Gen.choice [pure Space, pure Tab, Newline <$> genNewline])
    ]

genNormalWhitespace :: (MonadGen m, Alternative m) => m Whitespace
genNormalWhitespace =
  Gen.choice
    [ pure Space
    , pure Tab
    , Continued <$>
      genNewline <*>
      Gen.list (Range.constant 0 10) (Gen.element [Space, Tab])
    ]

genStringPrefix :: (MonadGen m, Alternative m) => m StringPrefix
genStringPrefix =
  Gen.element
    [ Prefix_r
    , Prefix_R
    , Prefix_u
    , Prefix_U
    , Prefix_b
    , Prefix_B
    , Prefix_br
    , Prefix_Br
    , Prefix_bR
    , Prefix_BR
    , Prefix_rb
    , Prefix_rB
    , Prefix_Rb
    , Prefix_RB
    ]

genComment :: (MonadGen m, Alternative m) => m Comment
genComment =
  Comment <$> Gen.list (Range.linear 0 100) (Gen.filter (`notElem` "\0\r\n") Gen.ascii)

genWhitespaces :: (MonadGen m, Alternative m) => m [Whitespace]
genWhitespaces = Gen.list (Range.constant 0 10) genNormalWhitespace

genAnyWhitespaces :: (MonadGen m, Alternative m) => m [Whitespace]
genAnyWhitespaces = Gen.list (Range.constant 0 10) genAnyWhitespace

genWhitespaces1 :: (MonadGen m, Alternative m) => m (NonEmpty Whitespace)
genWhitespaces1 = Gen.nonEmpty (Range.constant 1 10) genNormalWhitespace

genNone :: (MonadGen m, Alternative m) => m (Expr '[] ())
genNone = None () <$> genWhitespaces

genBool :: (MonadGen m, Alternative m) => m (Expr '[] ())
genBool = Bool () <$> Gen.bool <*> genWhitespaces

genOp :: (MonadGen m, Alternative m) => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

genDot :: (MonadGen m, Alternative m) => m Dot
genDot = Dot <$> genWhitespaces

genCommaSep :: (MonadGen m, Alternative m) => m a -> m (CommaSep a)
genCommaSep ma =
  thresholds
    [ (Nothing, pure CommaSepNone)
    , (Just 1, CommaSepOne <$> ma)
    , ( Just 1
      , sized2M
          (\a b -> CommaSepMany a <$> genWhitespaces <*> pure b)
          ma
          (genCommaSep ma)
      )
    ]

genTuple :: (MonadGen m, Alternative m) => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  sized2M
    (\a b -> Tuple () a <$> genWhitespaces <*> pure b)
    expr
    (sizedMaybe $ genCommaSep1' expr)

genCommaSep1 :: (MonadGen m, Alternative m) => m a -> m (CommaSep1 a)
genCommaSep1 ma =
  thresholds
    [ (Just 1, CommaSepOne1 <$> ma)
    , ( Just 1
      , sized2M
          (\a b -> CommaSepMany1 a <$> genWhitespaces <*> pure b)
          ma
          (genCommaSep1 ma)
      )
    ]

genCommaSep1' :: (MonadGen m, Alternative m) => m a -> m (CommaSep1' a)
genCommaSep1' ma =
  thresholds
    [ (Just 1, CommaSepOne1' <$> ma <*> Gen.maybe genWhitespaces)
    , ( Just 1
      , sized2M
          (\a b -> CommaSepMany1' a <$> genWhitespaces <*> pure b)
          ma
          (genCommaSep1' ma)
      )
    ]

genImportAs :: (MonadGen m, Alternative m) => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2
    (ImportAs ())
    me
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
