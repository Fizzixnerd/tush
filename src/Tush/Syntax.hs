{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Tush.Syntax where

import ClassyPrelude

-- | Tokens

data VarClass = VClassNormal
              | VClassOperator
              | VClassType
  deriving (Eq, Ord, Show)

data Var a = Var { varName :: Text
                 , varType :: a
                 , varClass :: VarClass
                 } deriving (Eq, Ord, Show, Functor)

isOp :: Var a -> Bool
isOp (Var _ _ VClassOperator) = True
isOp _ = False

-- | Tokens

data ReservedWord = For 
                  | If 
                  | Then 
                  | Else 
                  | In 
                  | Let 
                  | Def 
                  | Extern
  deriving (Eq, Ord, Show)

data ReservedOp = Arrow
                | Equals
  deriving (Eq, Ord, Show)

data ReservedPunctuation = Comma
                         | Semicolon
                         | Colon
                         | OpenParen
                         | CloseParen
                         | OpenBrace
                         | CloseBrace
                         | OpenBracket
                         | CloseBracket
  deriving (Eq, Ord, Show)

data Token = CommentT Text
           | ReservedWordT ReservedWord
           | ReservedOpT ReservedOp
           | ReservedPunctuationT ReservedPunctuation
           | VarT (Var (Maybe Type))
           | TypeVarT (Var Kind)
           | LiteralT Literal
           | TypeLiteralT TypeLiteral
           | EofT
  deriving (Eq, Ord, Show)

isVarT :: Token -> Bool
isVarT (VarT _) = True
isVarT _ = False

isOpT :: Token -> Bool
isOpT (VarT v) = isOp v
isOpT _ = False

isTypeLiteralT :: Token -> Bool
isTypeLiteralT (TypeLiteralT _) = True
isTypeLiteralT _ = False

isOpenParenT :: Token -> Bool
isOpenParenT (ReservedPunctuationT OpenParen) = True
isOpenParenT _ = False

isCommaT :: Token -> Bool
isCommaT (ReservedPunctuationT Comma) = True
isCommaT _ = False

isColonT :: Token -> Bool
isColonT (ReservedPunctuationT Colon) = True
isColonT _ = False

isLiteralT :: Token -> Bool
isLiteralT (LiteralT _) = True
isLiteralT _ = False

-- | Syntax Tree

data FProto a = FProto { fProtoName :: Var a
                       , fProtoArgs :: Vector (Var a)
                       }
              deriving (Eq, Ord, Show, Functor)

data Literal = ILit Integer
             | FLit Double
             | BLit Bool deriving (Eq, Ord, Show)

data Expression a = LitE { litELiteral :: Literal
                         , litEInfo :: a
                         }
                  | VarE { varEVar :: (Var a)
                         , varEInfo ::  a
                         }
                  | CallE { callEName :: Expression a
                          , callEArgs :: Vector (Expression a)
                          , callEInfo :: a
                          }
                  | IfE { ifEConditional :: Expression a
                        , ifEConsequent  :: Expression a
                        , ifEAntecedent  :: Expression a
                        , ifEInfo        :: a
                        } deriving (Eq, Ord, Show, Functor)
                  -- | ForE { forEVar :: Var a
                  --        , forEInitializer :: Expression a
                  --        , forETerminator :: Expression a
                  --        , forEIncrementer :: Expression a
                  --        , forEExpression :: Expression a
                  --        , forEInfo :: a
                  --        }

exprInfo :: Expression a -> a
exprInfo (LitE _ x) = x
exprInfo (VarE _ x) = x
exprInfo (CallE _ _ x) = x
exprInfo (IfE _ _ _ x) = x

data Statement a b = ExprS (Expression a)
                   | FuncS (FProto b) (Vector (Statement a b)) (Expression a)
                   | ExternS (FProto b)
                   deriving (Eq, Ord, Show)

isExprS :: Statement a b -> Bool
isExprS (ExprS _) = True
isExprS _ = False

data Type = TTypeLiteral TypeLiteral
          | TVar (Var Kind)
  deriving (Eq, Ord, Show)

data TypeLiteral = TLBuiltinType BuiltinType
                 | TLNamed Text
                 | TLUntyped
  deriving (Eq, Ord, Show)

type Kind = ()

builtinType = TTypeLiteral . TLBuiltinType

data BuiltinType = BTInt
                 | BTFloat
                 | BTBool
                 | BTLambda { btLambdaReturnType :: Type
                            , btLambdaArgTypes :: (Vector Type)
                            } deriving (Eq, Ord, Show)
