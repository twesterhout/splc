{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Types
-- Description : Types for the lexer
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : MIT
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Types
  ( Getter(..)
  , BinOp(..)
  , UnOp(..)
  , TokenData(..)
  , Pos
  , pos1
  , mkPos
  , defaultTabWidth
  , SourcePos(..)
  , Span(..)
  , Token(..)
  , Input(..)
  , LexError(..)
  , path
  , column
  , row
  , Ident(..)
  , ExprF(..)
  , ProgramF(..)
  , parseErrorPretty'
  , IxFunctor(..)
  , IxFoldable(..)
  , IxTraversable(..)
  , IxFix(..)
  , cata
  , cataM
  , cata'
  , cataM'
  , HasLo(..)
  , HasHi(..)
  , HasText(..)
  , HasPosition(..)
  , HasPayload(..)
  ) where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Monad((<=<))
import           Data.Foldable (foldl')
import           Data.Functor.Const
-- import           Data.Functor.Classes
-- import           Data.Functor.Foldable
import           Data.Functor.Identity
-- import           Data.Functor.Product
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
-- import           Data.Semigroup
import qualified Data.Text as T
import           Data.Traversable (Traversable(..))
import           GHC.Generics (Generic)
import           Lens.Micro
import           Lens.Micro.Extras
import           Lens.Micro.TH
import qualified Text.Megaparsec as P hiding (empty)
import           Text.Megaparsec.Pos
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty(..), hsep, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

_defaultShow :: Pretty a => a -> String
_defaultShow x = P.displayS (P.renderPretty 0.4 50 . P.pretty $ x) ""

data TokenData
  = TkIdent {-# UNPACK #-}!T.Text
  -- Keyword
  | TkIf
  | TkElse
  | TkWhile
  | TkReturn
  | TkVar
  -- Getters
  | TkHead
  | TkTail
  | TkFirst
  | TkSecond
  -- Types
  | TkTpBool
  | TkTpInt
  | TkTpChar
  | TkTpVoid
  -- Literals
  | TkBool !Bool
  | TkInt {-# UNPACK #-}!Int
  | TkChar {-# UNPACK #-}!Char
  -- Punctuation
  | TkSemiColon
  | TkComma
  | TkDoubleColon
  | TkRArrow
  | TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkLBrace
  | TkRBrace
  -- Operators
  | TkPlus
  | TkMinus
  | TkTimes
  | TkDivide
  | TkModulo
  | TkEqual
  | TkLess
  | TkGreater
  | TkLessEqual
  | TkGreaterEqual
  | TkNotEqual
  | TkAnd
  | TkOr
  | TkPrepend
  | TkAssign
  | TkNot
  deriving (Read, Show, Eq, Ord, Generic, NFData)

instance Pretty TokenData where
  pretty = go
    where
      go (TkIdent x) = P.text . T.unpack $ x
      go TkIf = P.text $ "if"
      go TkElse = P.text $ "else"
      go TkWhile = P.text $ "while"
      go TkReturn = P.text $ "return"
      go TkVar = P.text $ "var"
      go TkHead = P.text ".hd"
      go TkTail = P.text ".tl"
      go TkFirst = P.text ".fst"
      go TkSecond = P.text ".snd"
      go TkTpInt = P.text $ "Int"
      go TkTpBool = P.text $ "Bool"
      go TkTpChar = P.text $ "Char"
      go TkTpVoid = P.text $ "Void"
      go (TkBool x) = P.bool x
      go (TkInt x) = P.int x
      go (TkChar x) = P.squotes . P.char $ x
      go TkSemiColon = P.semi
      go TkComma = P.comma
      go TkDoubleColon = P.text "::"
      go TkRArrow = P.text "->"
      go TkLParen = P.lparen
      go TkRParen = P.rparen
      go TkLBracket = P.lbracket
      go TkRBracket = P.rbracket
      go TkLBrace = P.lbrace
      go TkRBrace = P.rbrace
      go TkPlus = P.char '+'
      go TkMinus = P.char '-'
      go TkTimes = P.char '*'
      go TkDivide = P.char '/'
      go TkModulo = P.char '%'
      go TkEqual = P.text "=="
      go TkLess = P.char '<'
      go TkGreater = P.char '>'
      go TkLessEqual = P.text "<="
      go TkGreaterEqual = P.text ">="
      go TkNotEqual = P.text "!="
      go TkAnd = P.text "&&"
      go TkOr = P.text "||"
      go TkPrepend = P.char ':'
      go TkAssign = P.char '='
      go TkNot = P.char '!'

makeLensesFor
  [("sourceName", "path"), ("sourceLine", "row"), ("sourceColumn", "column")]
  ''SourcePos

data Span = Span
  { _spanLo :: !SourcePos
  , _spanHi :: !SourcePos
  } deriving (Read, Show, Eq, Ord, Generic, NFData)

makeFields ''Span

{-
pos1 :: Pos
pos1 = unsafePos 1

mkPos :: Int -> Pos
mkPos !n = assert (n > 0) $ unsafePos . fromIntegral $ n
-}
data Token = Token
  { _tokenPosition :: !Span
  , _tokenPayload :: !TokenData
  } deriving (Read, Show, Eq, Ord, Generic, NFData)

makeFields ''Token

data Input = Input
  { _inputPosition :: !SourcePos
  , _inputText :: String
  } deriving (Read, Show, Eq, Generic, NFData)

makeFields ''Input

data LexError =
  LexError !SourcePos
           !T.Text
  deriving (Eq, Generic, NFData)

instance P.Pretty LexError where
  pretty (LexError pos msg) =
    P.red (P.string "Error:") P.<+> P.string (pos ^. path) P.<> P.colon P.<+>
    P.int (fromIntegral . unPos $ (pos ^. row)) P.<+>
    P.colon P.<+>
    P.int (fromIntegral . unPos $ (pos ^. column)) P.<+>
    P.string (T.unpack msg)

instance Show LexError where
  show x = P.displayS (P.renderPretty 0.4 50 . P.pretty $ x) ""

{-
data BasicType
  = TpInt
  | TpBool
  | TpChar
  deriving (Read, Show, Eq, Bounded, Enum, Generic, NFData)

data TypeF a
  = TpSimple !BasicType
  | TpPair !a !a
  | TpList !a
  deriving (Read, Show, Eq, Read1, Show1, Eq1, Generic, Functor, Foldable, Traversable, NFData)

data ReturnTypeF a
  = RTp !(TypeF a)
  | TpVoid
  deriving (Read, Show, Eq, Generic)

instance Pretty BasicType where
  pretty = P.text . go
    where
      go TpInt = "Int"
      go TpBool = "Bool"
      go TpChar = "Char"

instance Pretty Type where
  pretty = cata go
    where
      go :: TypeF Doc -> Doc
      go (TpSimple t) = pretty t
      go (TpPair t1 t2) = P.tupled $ [t1, t2]
      go (TpList t) = P.brackets t

instance Pretty ReturnType where
  pretty = go
    where
      go (RTp t) = pretty t
      go TpVoid = P.text "Void"
-}

data Getter
  = Head
  | Tail
  | First
  | Second
  deriving (Read, Show, Eq, Bounded, Enum, Generic, NFData)

instance Pretty Getter where
  pretty = P.text . \case
    Head -> ".hd"
    Tail -> ".tl"
    First -> ".fst"
    Second -> ".snd"

data BinOp
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpModulo
  | OpEqual
  | OpLess
  | OpGreater
  | OpLessEqual
  | OpGreaterEqual
  | OpNotEqual
  | OpAnd
  | OpOr
  | OpPrepend
  | OpAssign
  deriving (Read, Show, Eq, Bounded, Enum, Generic, NFData)

data UnOp
  = OpNegate
  | OpNot
  deriving (Read, Show, Eq, Bounded, Enum, Generic, NFData)

instance Pretty BinOp where
  pretty = go
    where
      go OpPlus = P.char '+'
      go OpMinus = P.char '-'
      go OpTimes = P.char '*'
      go OpDivide = P.char '/'
      go OpModulo = P.char '%'
      go OpEqual = P.text "=="
      go OpLess = P.char '<'
      go OpGreater = P.char '>'
      go OpLessEqual = P.text "<="
      go OpGreaterEqual = P.text ">="
      go OpNotEqual = P.text "!="
      go OpAnd = P.text "&&"
      go OpOr = P.text "||"
      go OpPrepend = P.char ':'
      go OpAssign = P.char '='

instance Pretty UnOp where
  pretty = go
    where
      go OpNegate = P.char '-'
      go OpNot = P.char '!'

data Ident = Ident {-# UNPACK #-}!T.Text
  deriving (Read, Show, Eq, Generic, NFData)

instance Pretty Ident where
  pretty (Ident name) = P.text . T.unpack $ name


data ExprType
  = TypeTy -- ^ Simple type
  | ReturnTypeTy -- ^ Function return type
  | VarTypeAnnotTy
  | FunTypeAnnotTy
  | LitTy
  | FieldTy
  | ExprTy
  | StmtTy
  | VarDeclTy
  | DeclTy
  deriving (Read, Show, Eq, Ord, Generic, NFData)

imapDefault :: IxTraversable t => (a ~> b) -> (t a ~> t b)
imapDefault f = runIdentity . itraverse (Identity . f)

ifoldMapDefault :: (IxTraversable t, Monoid m) => (a ~>. m) -> (t a ~>. m)
ifoldMapDefault f = getConst . itraverse (Const . f)



data ExprF (k :: ExprType -> *) (i :: ExprType) where
  TpInt :: ExprF k 'TypeTy
  TpChar :: ExprF k 'TypeTy
  TpBool :: ExprF k 'TypeTy
  TpTuple :: k 'TypeTy -> k 'TypeTy -> ExprF k 'TypeTy
  TpList :: k 'TypeTy -> ExprF k 'TypeTy
  TpReturn :: k 'TypeTy -> ExprF k 'ReturnTypeTy
  TpVoid :: ExprF k 'ReturnTypeTy
  -- * Type annotations
  VarTypeAnnotF :: k 'TypeTy -> ExprF k 'VarTypeAnnotTy
  FunTypeAnnotF :: [k 'TypeTy] -> k 'ReturnTypeTy -> ExprF k 'FunTypeAnnotTy
  -- * Literals
  Int :: Int -> ExprF k 'LitTy
  Char :: Char -> ExprF k 'LitTy
  Bool :: Bool -> ExprF k 'LitTy
  Tuple :: k 'ExprTy -> k 'ExprTy -> ExprF k 'LitTy
  List :: [k 'ExprTy] -> ExprF k 'LitTy
  -- * Fields
  Name :: Ident -> ExprF k 'FieldTy
  Get :: Getter -> k 'FieldTy -> ExprF k 'FieldTy
  -- * Expressions
  BinOp :: BinOp -> k 'ExprTy -> k 'ExprTy -> ExprF k 'ExprTy
  UnOp :: UnOp -> k 'ExprTy -> ExprF k 'ExprTy
  Call :: Ident -> [k 'ExprTy] -> ExprF k 'ExprTy
  Return :: k 'ExprTy -> ExprF k 'ExprTy
  Assignment :: k 'FieldTy -> k 'ExprTy -> ExprF k 'ExprTy
  -- * Statements
  If :: k 'ExprTy -> [k 'StmtTy] -> [k 'StmtTy] -> ExprF k 'StmtTy
  While :: k 'ExprTy -> [k 'StmtTy] -> ExprF k 'StmtTy
  Semi :: k 'ExprTy -> ExprF k 'StmtTy
  -- * Declarations
  VarDeclF
    :: Ident -> Maybe (k 'VarTypeAnnotTy) -> k 'ExprTy -> ExprF k 'VarDeclTy
  GlobalVarF :: k 'VarDeclTy -> ExprF k 'DeclTy
  FunDeclF
    :: Ident
    -> [Ident]
    -> Maybe (k 'FunTypeAnnotTy)
    -> [k 'VarDeclTy]
    -> [k 'StmtTy]
    -> ExprF k 'DeclTy

newtype ProgramF (k :: ExprType -> *) =
  ProgramF [ExprF k 'DeclTy]
  -- deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

infixr 5 ~>
infixr 5 .~>
infixr 5 ~>.

type f ~>  g = forall i. f i -> g i -- ^ Natural transformation
type f .~> g = forall i. f   -> g i -- ^ Constant on the left
type f ~>. g = forall i. f i -> g   -- ^ Constant on the right

class IxFunctor (f :: (k -> *) -> (k -> *)) where
  imap :: (a ~> b) -> (f a ~> f b)

-- the type would be "(a ~> (f . b)) -> (t a ~> (f . t b))" if we had the composition
-- function on type level.
class IxFunctor t => IxTraversable t where
  itraverse :: Applicative f
            => (forall i. a i -> f (b i)) -> (forall i. t a i -> f (t b i))

class IxFoldable t where
  ifoldMap :: Monoid m => (a ~>. m) -> (t a ~>. m)

newtype IxFix f i = In { out :: f (IxFix f) i }

deriving instance Show (f (IxFix f) t) => Show (IxFix f t)
deriving instance Eq (f (IxFix f) t) => Eq (IxFix f t)
deriving instance Ord (f (IxFix f) t) => Ord (IxFix f t)

-- instance Pretty (f (IxFix f) t) => Pretty (IxFix f t) where
--   pretty = pretty . out

data (f :*: g) a i = (:*:) (f a i) (g a i)

instance (IxFunctor a, IxFunctor b) => IxFunctor (a :*: b) where
  imap f (a :*: b) = (imap f a) :*: (imap f b)

instance (IxFoldable a, IxFoldable b) => IxFoldable (a :*: b) where
  ifoldMap f (a :*: b) = ifoldMap f a `mappend` ifoldMap f b

instance (IxTraversable f, IxTraversable g) => IxTraversable (f :*: g) where
  itraverse f (a :*: b) = (:*:) <$> itraverse f a <*> itraverse f b

newtype K a k i = K a

cata :: IxFunctor f => (f a ~> a) -> (IxFix f ~> a)
cata = go
  where go :: IxFunctor f => (f a ~> a) -> (IxFix f ~> a)
        go φ = φ . imap (go φ) . out

cataM :: (Monad m, IxTraversable t)
      => (forall i. t a i -> m (a i)) -> (forall i. IxFix t i -> m (a i))
cataM = go
  where go :: (Monad m, IxTraversable t)
           => (forall i. t a i -> m (a i)) -> (forall i. IxFix t i -> m (a i))
        go φ = φ <=< itraverse (go φ) . out

cata' :: IxFunctor f => (f (Const a) ~>. a) -> (IxFix f ~>. a)
cata' φ = getConst . cata (Const . φ)

cataM' :: (Monad m, IxTraversable t) => (t (Const a) ~>. m a) -> (IxFix t ~>. m a)
cataM' = go
 where go :: (Monad m, IxTraversable t) => (t (Const a) ~>. m a) -> (IxFix t ~>. m a)
       go φ = φ <=< itraverse (fmap Const . cataM' φ) . out

instance IxFoldable ExprF where
  ifoldMap = ifoldMapDefault

instance IxFunctor ExprF where
  imap = imapDefault

instance IxTraversable ExprF where
  itraverse f = \case
    TpInt -> pure TpInt
    TpChar -> pure TpChar
    TpBool -> pure TpBool
    TpTuple x y -> TpTuple <$> f x <*> f y
    TpList x -> TpList <$> f x
    TpReturn x -> TpReturn <$> f x
    TpVoid -> pure TpVoid
    VarTypeAnnotF tp -> VarTypeAnnotF <$> f tp
    FunTypeAnnotF xs r -> FunTypeAnnotF <$> traverse f xs <*> f r
    Int x -> pure $ Int x
    Char c -> pure $ Char c
    Bool b -> pure $ Bool b
    Tuple x y -> Tuple <$> f x <*> f y
    List xs -> List <$> traverse f xs
    Name i -> pure $ Name i
    Get g x -> Get g <$> f x
    BinOp op x y -> BinOp op <$> f x <*> f y
    UnOp op x -> UnOp op <$> f x
    Call name xs -> Call name <$> traverse f xs
    Return x -> Return <$> f x
    Assignment x y -> Assignment <$> f x <*> f y
    If p x y -> If <$> f p <*> traverse f x <*> traverse f y
    While p x -> While <$> f p <*> traverse f x
    Semi x -> Semi <$> f x
    VarDeclF i tp x -> VarDeclF i <$> traverse f tp <*> f x
    GlobalVarF x -> GlobalVarF <$> f x
    FunDeclF i is tp xs ys -> FunDeclF i is <$> traverse f tp <*> traverse f xs <*> traverse f ys


type Expr = IxFix ExprF


type AnnotExpr = IxFix (K SourcePos :*: ExprF)

instance Pretty (Expr (k :: ExprType)) where
  pretty = cata' go
    where go :: ExprF (Const Doc) i -> Doc
          go = \case
            TpInt -> P.text "Int"
            TpChar -> P.text "Char"
            TpBool -> P.text "Bool"
            TpTuple (Const x) (Const y) -> P.tupled [x, y]
            TpList (Const x) -> P.brackets x
            TpReturn (Const x) -> x
            TpVoid -> P.text "Void"
            VarTypeAnnotF (Const x) -> x
            FunTypeAnnotF xs (Const r) -> P.text "::" <+> hsep (getConst <$> xs)
                                                      <+> P.text "->"
                                                      <+> r
            Int x -> P.int x
            Char c -> P.text (show c)
            Bool b -> P.text (show b)
            Tuple (Const x) (Const y) -> P.tupled [x, y]
            List xs -> P.list $ getConst <$> xs
            Name s -> pretty s
            Get g (Const x) -> x P.<> pretty g
            BinOp op (Const x) (Const y) -> P.parens $ x <+> pretty op <+> y
            UnOp op (Const x) -> P.parens $ pretty op <+> x
            Call name xs -> pretty name P.<> (P.parens . P.cat . P.punctuate P.comma $ getConst <$> xs)
            Return (Const x) -> P.text "return" <+> x
            Assignment (Const x) (Const y) -> x <+> P.equals <+> y
            If (Const p) xs ys -> prettyIf p (getConst <$> xs) (getConst <$> ys)
            While (Const p) xs -> prettyWhile p (getConst <$> xs)
            Semi (Const x) -> x <+> P.semi
            VarDeclF name tp (Const x) -> fromMaybe (P.text "var") (getConst <$> tp)
                                      <+> pretty name <+> P.equals <+> x <+> P.semi
            GlobalVarF (Const x) -> x

-- prettyFunc :: Doc -> [Doc] -> Maybe Doc -> [Doc] -> [Doc]
-- prettyFunc name args tp decls stmts =
--   block 
--   where block start middle end = P.align (P.nest 2 (start P.<$> middle) P.<$> end)

prettyIf :: Doc -> [Doc] -> [Doc] -> Doc
prettyIf pred ifs elses = ifClause P.<> elseClause
    where block start middle end = P.align (P.nest 2 (start P.<$> middle) P.<$> end)
          ifClause = block (P.text "if" <+> P.parens pred <+> P.lbrace)
                           (P.vcat ifs)
                           P.rbrace
          elseClause
            | null elses = P.empty
            | otherwise = P.line P.<> block (P.text "else" <+> P.lbrace)
                                            (P.vcat elses)
                                            P.rbrace

prettyWhile :: Doc -> [Doc] -> Doc
prettyWhile pred stmts = block (P.text "while" <+> P.parens pred <+> P.lbrace)
                               (P.vcat stmts)
                               P.rbrace
    where block start middle end = P.align (P.nest 2 (start P.<$> middle) P.<$> end)


{- hello
data ItemF a
  = FunDecl !(FunDeclF a)
  | VarDecl !(VarDeclF a)
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

data VarDeclF a =
  Var !Ident
      !(Maybe (VarTypeAnnotF a))
      !(ExprF a)
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

data FunDeclF a =
  Fn !Ident
     ![Ident]
     !(Maybe (FunTypeAnnotF a))
     ![VarDeclF a]
     ![StmtF a]
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

type VarTypeAnnotF = TypeF

data FunTypeAnnotF a =
  FunTypeAnnot ![TypeF a]
               !ReturnType
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)


data Lit
  = Int {-# UNPACK #-}!Int
  | Char {-# UNPACK #-}!Char
  | Bool !Bool
  deriving (Read, Show, Eq, Generic, NFData)

data ExprF a
  = BinOp !BinOp a a
  | UnOp !UnOp a
  | Pair a a
  | List [a]
  | Call !Ident ![a]
  | Return a
  | Assignment a a
  | Get !Getter a
  | Lit !Lit
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

data StmtF a
  = If (ExprF a)
       [StmtF a]
       [StmtF a]
  | While (ExprF a)
          [StmtF a]
  | Semi (ExprF a)
  deriving (Read, Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

-}
instance P.Stream [Token] where
  type Token [Token] = Token
  type Tokens [Token] = [Token]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  positionAt1 _ _ t = t ^. position . lo
  positionAtN _ p [] = p
  positionAtN _ _ (t:_) = t ^. position . lo
  advance1 _ _ _ t = t ^. position . hi
  advanceN _ _ p [] = p
  advanceN _ _ _ (last -> t) = t ^. position . hi
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | [] <- s = Nothing
    | otherwise = Just $ List.splitAt n s
  takeWhile_ f = List.break (not . f)

_showTokens :: NonEmpty Token -> Doc
_showTokens (x :| xs) = view _1 $ foldl' join (doc₀, n₀) xs
  where
    doc₀ = P.pretty (x ^. payload)
    n₀ = unPos $ x ^. position . hi . column
    skip n = P.hcat $ replicate n P.space
    join !(!doc, !n) !y =
      let δn = unPos (y ^. position . lo . column) - n
      in assert (δn >= 0) $
         ( doc P.<> skip δn P.<> P.pretty (y ^. payload)
         , unPos $ y ^. position . hi . column)

instance P.ShowToken Token where
  showTokens xs = P.displayS (P.renderPretty 0.4 50 (_showTokens xs)) ""

selectLine ::
     forall s. (P.Stream s, HasPosition (P.Token s) Span)
  => Pos -- ^ Number of line to select
  -> s -- ^ Input stream
  -> P.Tokens s -- ^ Selected line
selectLine n =
  fst . P.takeWhile_ (\t -> t ^. position . lo . row == n) . snd .
  P.takeWhile_ (\t -> t ^. position . lo . row < n)

parseErrorPretty' ::
     forall e. P.ShowErrorComponent e
  => [Token] -- ^ Original input stream
  -> P.ParseError Token e -- ^ Parse error to render
  -> Doc -- ^ Result of rendering
parseErrorPretty' s e =
  P.vcat
    [ (P.string . P.sourcePosStackPretty . P.errorPos) e P.<> P.colon
    , empty
    , P.blue (P.text lineNumber P.<+> bar) P.<+> rline
    , empty P.<+> indicator
    , P.string (P.parseErrorTextPretty e)
    ]
  where
    epos = NE.last (P.errorPos e)
    lineNumber = show . unPos . view row $ epos
    bar = P.char '|'
    skip n = P.hcat $ replicate n P.space
    empty = P.blue $ skip (length lineNumber) P.<+> bar
    indicator
      | P.TrivialError _ (Just (P.Tokens ts)) _ <- e =
        let n = length $ P.displayS (P.renderCompact (_showTokens ts)) ""
        in skip (unPos (epos ^. column) - 1) P.<>
           (P.bold . P.red . P.text) (replicate n '^')
      | otherwise =
        skip (unPos (epos ^. column) - 1) P.<> (P.bold . P.red . P.char) '^'
    rline =
      case selectLine (epos ^. row) s of
        (x:xs) ->
          skip (unPos (x ^. position . lo . column) - 1) P.<>
          _showTokens (x :| xs)
        [] -> P.text "<empty line>"
