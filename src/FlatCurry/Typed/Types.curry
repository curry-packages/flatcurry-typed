------------------------------------------------------------------------------
--- This library contains a version of FlatCurry's abstract syntax tree
--- modified with type information
---
--- For more information about the abstract syntax tree of `FlatCurry`,
--- see the documentation of the respective module.
---
--- @author  Kai-Oliver Prott
--- @version June 2018
--- @category meta
------------------------------------------------------------------------------
module FlatCurry.Typed.Types
  ( module FlatCurry.Typed.Types
  , module FlatCurry.Types
  ) where

import FlatCurry.Types ( QName, VarIndex, Visibility (..), TVarIndex
                       , TypeDecl (..), OpDecl (..), Fixity (..)
                       , TypeExpr (..), ConsDecl (..), NewConsDecl (..)
                       , Literal (..), CombType (..), CaseType (..)
                       )

data TProg = TProg String [String] [TypeDecl] [TFuncDecl] [OpDecl]
  deriving (Eq, Read, Show)

data TFuncDecl = TFunc QName Int Visibility TypeExpr TRule
  deriving (Eq, Read, Show)

data TRule
  = TRule     [(VarIndex, TypeExpr)] TExpr
  | TExternal TypeExpr String
  deriving (Eq, Read, Show)

data TExpr
  = TVarE  TypeExpr VarIndex -- otherwise name clash with TypeExpr's TVar
  | TLit   TypeExpr Literal
  | TComb  TypeExpr CombType QName [TExpr]
  | TLet   [((VarIndex, TypeExpr), TExpr)] TExpr
  | TFree  [(VarIndex, TypeExpr)] TExpr
  | TOr    TExpr TExpr
  | TCase  CaseType TExpr [TBranchExpr]
  | TTyped TExpr TypeExpr
  deriving (Eq, Read, Show)

data TBranchExpr = TBranch TPattern TExpr
  deriving (Eq, Read, Show)

data TPattern
  = TPattern  TypeExpr QName [(VarIndex, TypeExpr)]
  | TLPattern TypeExpr Literal
  deriving (Eq, Read, Show)

class Typeable a where
  typeOf ::  a -> TypeExpr

instance Typeable TRule where
  typeOf (TRule args e) = foldr (FuncType . snd) (typeOf e) args
  typeOf (TExternal ty _) = ty

instance Typeable TExpr where
  typeOf (TVarE ty _) = ty
  typeOf (TLit ty _) = ty
  typeOf (TComb  ty _ _ _) = ty
  typeOf (TLet _ e) = typeOf e
  typeOf (TFree _ e) = typeOf e
  typeOf (TOr e _) = typeOf e
  typeOf (TCase _ _ (e:_)) = typeOf e
  typeOf (TTyped _ ty) = ty
  typeOf (TCase _ _ []) = error $ "FlatCurry.Typed.Type.typeOf: " ++
                                  "empty list in case expression"

instance Typeable TPattern where
  typeOf (TPattern ty _ _) = ty
  typeOf (TLPattern ty _) = ty

instance Typeable TBranchExpr where
  typeOf (TBranch _ e) = typeOf e
