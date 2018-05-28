{-# language LambdaCase #-}
{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language ExistentialQuantification #-}
module Language.Python.Internal.Syntax.Expr where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.), getting, to)
import Control.Lens.Lens (Lens, lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (_Just, _Left, _Right)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal, failing)
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import GHC.Generics (Generic)

import Language.Python.Internal.Syntax.BinOp
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

-- | 'Traversal' over all the expressions in a term
class HasExprs s where
  _Exprs :: Traversal (s v a) (s '[] a) (Expr v a) (Expr '[] a)

data Arg (v :: [*]) a
  = PositionalArg
  { _argAnn :: a
  , _argExpr :: Expr v a
  }
  | KeywordArg
  { _argAnn :: a
  , _unsafeKeywordArgName :: Ident v a
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | StarArg
  { _argAnn :: a
  , _unsafeStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | DoubleStarArg
  { _argAnn :: a
  , _unsafeDoubleStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance IsString (Arg '[] ()) where; fromString = PositionalArg () . fromString

argExpr :: Lens (Arg v a) (Arg '[] a) (Expr v a) (Expr '[] a)
argExpr = lens _argExpr (\s a -> (coerce s) { _argExpr = a })

instance HasExprs Arg where
  _Exprs f (KeywordArg a name ws2 expr) = KeywordArg a (coerce name) ws2 <$> f expr
  _Exprs f (PositionalArg a expr) = PositionalArg a <$> f expr
  _Exprs f (StarArg a ws expr) = StarArg a ws <$> f expr
  _Exprs f (DoubleStarArg a ws expr) = StarArg a ws <$> f expr

data StringPrefix
  = Prefix_r
  | Prefix_R
  | Prefix_u
  | Prefix_U
  | Prefix_b
  | Prefix_B
  | Prefix_br
  | Prefix_Br
  | Prefix_bR
  | Prefix_BR
  | Prefix_rb
  | Prefix_rB
  | Prefix_Rb
  | Prefix_RB
  deriving (Eq, Show)

data StringType
  = ShortSingle
  | ShortDouble
  | LongSingle
  | LongDouble
  deriving (Eq, Show)

data CompFor (v :: [*]) a
  -- ^ 'for' <any_spaces> <targets> 'in' <any_spaces> <expr>
  = CompFor a [Whitespace] (Expr v a) [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (CompFor v a) (CompFor '[] a) where
  unvalidate = coerce
  whitespaceAfter =
    lens
      (\(CompFor _ _ _ _ e) -> e ^. getting whitespaceAfter)
      (\(CompFor a b c d e) ws ->
         CompFor a b (unvalidate c) d (unvalidate e & whitespaceAfter .~ ws))
  startChar _ = 'f'
  endChar (CompFor _ _ _ _ e) = endChar e

data CompIf (v :: [*]) a
  -- ^ 'if' <any_spaces> <expr>
  = CompIf a [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (CompIf v a) (CompIf '[] a) where
  unvalidate = coerce
  whitespaceAfter =
    lens
      (\(CompIf _ _ e) -> e ^. getting whitespaceAfter)
      (\(CompIf a b c) ws -> CompIf a b (c & whitespaceAfter .~ ws))
  startChar _ = 'i'
  endChar (CompIf _ _ e) = endChar e

data Comprehension (v :: [*]) a
  -- ^ <expr> <comp_for> (comp_for | comp_if)*
  = Comprehension a (Expr v a) (CompFor v a) [Either (CompFor v a) (CompIf v a)]
  deriving (Eq, Show)

instance Token (Comprehension v a) (Comprehension '[] a) where
  unvalidate = coerce
  whitespaceAfter =
    lens
      (\(Comprehension _ _ a b) ->
         fromMaybe (a ^. getting whitespaceAfter) $
         b ^? _last.failing (_Left.getting whitespaceAfter) (_Right.getting whitespaceAfter))
      (\(Comprehension a b c d) ws ->
         Comprehension a (unvalidate b)
           (foldr (\_ _ -> unvalidate c) (unvalidate c & whitespaceAfter .~ ws) d)
           (coerce d &
            _last.failing (_Left.whitespaceAfter) (_Right.whitespaceAfter) .~ ws))
  startChar (Comprehension _ a _ _) = startChar a
  endChar (Comprehension _ _ a b) =
    fromMaybe (endChar a) (b ^? _last.to (either endChar endChar))

instance Functor (Comprehension v) where
  fmap f (Comprehension a b c d) =
    Comprehension (f a) (fmap f b) (fmap f c) (fmap (bimap (fmap f) (fmap f)) d)

instance Foldable (Comprehension v) where
  foldMap f (Comprehension a b c d) =
    f a <> foldMap f b <> foldMap f c <> foldMap (bifoldMap (foldMap f) (foldMap f)) d

instance Traversable (Comprehension v) where
  traverse f (Comprehension a b c d) =
    Comprehension <$>
    f a <*>
    traverse f b <*>
    traverse f c <*>
    traverse (bitraverse (traverse f) (traverse f)) d

instance HasExprs CompFor where
  _Exprs f (CompFor a b c d e) = CompFor a b <$> f c <*> pure d <*> f e

instance HasExprs CompIf where
  _Exprs f (CompIf a b c) = CompIf a b <$> f c

instance HasExprs Comprehension where
  _Exprs f (Comprehension a b c d) =
    Comprehension a <$>
    f b <*>
    _Exprs f c <*>
    (traverse.(\x -> bitraverse (_Exprs x) (_Exprs x))) f d

data Expr (v :: [*]) a
  = ListComp
  { _exprAnnotation :: a
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  , _unsafeListCompValue :: Comprehension v a
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  | List
  { _exprAnnotation :: a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: CommaSep (Expr v a)
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _exprAnnotation :: a
  -- expr
  , _unsafeDerefValueLeft :: Expr v a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident v a
  }
  | Call
  { _exprAnnotation :: a
  -- expr
  , _unsafeCallFunction :: Expr v a
  -- ( spaces
  , _unsafeCallWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeCallArguments :: CommaSep (Arg v a)
  -- ) spaces
  , _unsafeCallWhitespaceRight :: [Whitespace]
  }
  | None
  { _exprAnnotation :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  | BinOp
  { _exprAnnotation :: a
  , _unsafeBinOpExprLeft :: Expr v a
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpExprRight :: Expr v a
  }
  | Negate
  { _exprAnnotation :: a
  -- - spaces
  , _unsafeNegateWhitespace :: [Whitespace]
  -- expr
  , _unsafeNegateValue :: Expr v a
  }
  | Parens
  { _exprAnnotation :: a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr v a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  | Ident
  { _exprAnnotation :: a
  , _unsafeIdentValue :: Ident v a
  }
  | Int
  { _exprAnnotation :: a
  , _unsafeIntValue :: Integer
  , _unsafeIntWhitespace :: [Whitespace]
  }
  | Bool
  { _exprAnnotation :: a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  | String
  { _exprAnnotation :: a
  , _unsafeStringPrefix :: Maybe StringPrefix
  , _unsafeStringType :: StringType
  , _unsafeStringValue :: String
  , _unsafeStringWhitespace :: [Whitespace]
  }
  | Tuple
  { _exprAnnotation :: a
  -- expr
  , _unsafeTupleHead :: Expr v a
  -- , spaces
  , _unsafeTupleWhitespace :: [Whitespace]
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (Expr v a))
  }
  | Not
  { _exprAnnotation :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance Token (Expr v a) (Expr '[] a) where
  unvalidate = coerce

  startChar ListComp{} = '['
  startChar List{} = '['
  startChar (Deref _ e _ _) = startChar e
  startChar (Call _ e _ _ _) = startChar e
  startChar None{} = 'N'
  startChar (BinOp _ e _ _) = startChar e
  startChar Negate{} = '-'
  startChar Parens{} = '('
  startChar (Ident _ i) = Prelude.head $ _identValue i
  startChar (Int _ i _) = Prelude.head $ show i
  startChar (Bool _ b _) = Prelude.head $ show b
  startChar String{} = '"'
  startChar (Tuple _ a _ _) = startChar a
  startChar Not{} = 'n'

  endChar ListComp{} = ']'
  endChar List{} = ']'
  endChar (Deref _ _ _ i) = Prelude.last $ _identValue i
  endChar Call{} = ')'
  endChar None{} = 'e'
  endChar (BinOp _ _ _ e) =
    case e of
      Tuple{} -> ')'
      _ -> endChar e
  endChar (Negate _ _ e) = endChar e
  endChar Parens{} = ')'
  endChar (Ident _ i) = Prelude.last $ _identValue i
  endChar (Int _ i _) = Prelude.last $ show i
  endChar (Bool _ b _) = Prelude.last $ show b
  endChar String{} = '"'
  endChar (Tuple _ a _ c) = maybe (endChar a) endChar c
  endChar (Not _ _ e) = endChar e

  whitespaceAfter =
    lens
      (\case
          None _ ws -> ws
          List _ _ _ ws -> ws
          ListComp _ _ _ ws -> ws
          Deref _ _ _ a -> a ^. getting whitespaceAfter
          Call _ _ _ _ ws -> ws
          BinOp _ _ _ e -> e ^. getting whitespaceAfter
          Negate _ _ e -> e ^. getting whitespaceAfter
          Parens _ _ _ ws -> ws
          Ident _ a -> a ^. getting whitespaceAfter
          Int _ _ ws -> ws
          Bool _ _ ws -> ws
          String _ _ _ _ ws -> ws
          Not _ _ e -> e ^. getting whitespaceAfter
          Tuple _ _ ws Nothing -> ws
          Tuple _ _ _ (Just cs) -> go cs
            where
              go cs =
                case cs of
                  CommaSepOne1' e Nothing -> e ^. getting whitespaceAfter
                  CommaSepOne1' _ (Just ws) -> ws
                  CommaSepMany1' _ _ rest -> go rest)
      (\e ws ->
        case e of
          None a _ -> None a ws
          List a b c _ -> List a b (coerce c) ws
          ListComp a b c _ -> ListComp a b (coerce c) ws
          Deref a b c d -> Deref a (coerce b) c (d & whitespaceAfter .~ ws)
          Call a b c d _ -> Call a (coerce b) c (coerce d) ws
          BinOp a b c e -> BinOp a (coerce b) c (e & whitespaceAfter .~ ws)
          Negate a b c -> Negate a b (c & whitespaceAfter .~ ws)
          Parens a b c _ -> Parens a b (coerce c) ws
          Ident a b -> Ident a (b & whitespaceAfter .~ ws)
          Int a b _ -> Int a b ws
          Bool a b _ -> Bool a b ws
          String d a b c _ -> String d a b c ws
          Not a b c -> Not a b (c & whitespaceAfter .~ ws)
          Tuple a e _ Nothing -> Tuple a (coerce e) ws Nothing
          Tuple a b ws (Just cs) -> Tuple a (coerce b) ws (Just $ cs & whitespaceAfter .~ ws))

instance IsString (Expr '[] ()) where
  fromString s = Ident () (MkIdent () s [])

instance Num (Expr '[] ()) where
  fromInteger n = Int () n []
  negate = Negate () []
  (+) a = BinOp () (a & whitespaceAfter .~ [Space]) (Plus () [Space])
  (*) a = BinOp () (a & whitespaceAfter .~ [Space]) (Multiply () [Space])
  (-) a = BinOp () (a & whitespaceAfter .~ [Space]) (Minus () [Space])
  signum = undefined
  abs = undefined

instance Plated (Expr '[] a) where; plate = gplate

instance HasExprs Expr where
  _Exprs = id

shouldBracketLeft :: BinOp a -> Expr v a -> Bool
shouldBracketLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case left of
        BinOp _ _ lOp _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just R <- lEntry ^? _Just.opAssoc -> True
        _ -> False

    leftf' =
      case (left, op) of
        (Negate{}, Exp{}) -> True
        (Tuple{}, _) -> True
        (Not{}, _) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (lEntry ^? _Just.opPrec)
  in
    leftf || leftf'

shouldBracketRight :: BinOp a -> Expr v a -> Bool
shouldBracketRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case right of
        BinOp _ _ rOp _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just L <- rEntry ^? _Just.opAssoc -> True
        _ -> False

    rightf' =
      case (op, right) of
        (_, Tuple{}) -> True
        (_, Not{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'

makeLenses ''Expr
