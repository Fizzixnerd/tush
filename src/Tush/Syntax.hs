{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
                 } deriving (Eq, Ord, Show)
type SimplyTypedVar = Var BuiltinType

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
           | VarT (Var Type)
           | TypeVarT (Var Kind)
           | LiteralT Literal
           | TypeLiteralT TypeLiteral
  deriving (Eq, Ord, Show)

isVarT :: Token -> Bool
isVarT (VarT _) = True
isVarT _ = False

isOpenParenT :: Token -> Bool
isOpenParenT (ReservedPunctuationT OpenParen) = True
isOpenParenT _ = False

isCommaT :: Token -> Bool
isCommaT (ReservedPunctuationT Comma) = True
isCommaT _ = False

isTypeAsT :: Token -> Bool
isTypeAsT (ReservedPunctuationT Colon) = True
isTypeAsT _ = False

-- | Syntax Tree

data FProto a = FProto { fProtoName :: Var a
                       , fProtoArgs :: Vector (Var a)
                       }
              deriving (Eq, Ord, Show)

data Literal = ILit Integer
             | FLit Double
             | BLit Bool deriving (Eq, Ord, Show)

data Expression a = LitE { litELiteral :: Literal
                         , litEInfo :: a }
                  | VarE { varEVar :: (Var a)
                         , varEInfo ::  a }
                  | CallE { callEName :: Expression a
                          , callEArgs :: Vector (Expression a)
                          , callEInfo :: a
                          }
                  | IfE { ifEConditional :: Expression a
                        , ifEConsequent  :: Expression a
                        , ifEAntecedent  :: Expression a
                        , ifEInfo        :: a
                        }
                  | ForE { forEVar :: Var a
                         , forEInitializer :: Expression a
                         , forETerminator :: Expression a
                         , forEIncrementer :: Expression a
                         , forEExpression :: Expression a
                         , forEInfo :: a
                         }
                  deriving (Eq, Ord, Show)

exprInfo :: Expression a -> a
exprInfo (LitE _ x) = x
exprInfo (VarE _ x) = x
exprInfo (CallE _ _ x) = x
exprInfo (IfE _ _ _ x) = x
exprInfo (ForE _ _ _ _ _ x) = x

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

data BuiltinType = BTInt
                 | BTFloat
                 | BTBool
                 | BTLambda { btLambdaReturnType :: BuiltinType
                            , btLambdaArgTypes :: (Vector BuiltinType)
                            }
                 deriving (Eq, Ord, Show)
