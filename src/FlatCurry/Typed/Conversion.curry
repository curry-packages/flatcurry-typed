------------------------------------------------------------------------------
--- This module can convert TypedFlatCurry to AnnotatedFlatCurry
--- with type annotations.
---
--- @author  Kai-Oliver Prott
--- @version June 2018
--- @category meta
------------------------------------------------------------------------------

module FlatCurry.Typed.Conversion (toAnnotatedFlatCurry) where

import FlatCurry.Typed.Types
import FlatCurry.Typed.Goodies
import FlatCurry.Annotated.Types

--- Converts a TypedFlatCurry programm into AnnotatedFlatCurry with
--- type-annotations
toAnnotatedFlatCurry :: TProg -> AProg TypeExpr
toAnnotatedFlatCurry = trTProg (\name imps types funcs ops ->
  AProg name imps types (map funcToAnnotated funcs) ops)

funcToAnnotated :: TFuncDecl -> AFuncDecl TypeExpr
funcToAnnotated = trTFunc (\name arity vis t rule ->
  AFunc name arity vis t (ruleToAnnotated rule))

ruleToAnnotated :: TRule -> ARule TypeExpr
ruleToAnnotated rule = trTRule (\args e ->
  ARule (typeOf rule) args (exprToAnnotated e)) AExternal rule

exprToAnnotated :: TExpr -> AExpr TypeExpr
exprToAnnotated = trTExpr
  AVar
  ALit
  (\ty ct name args -> AComb ty ct (name, consType ty (map typeOf args)) args)
  (\bs e -> ALet (typeOf e) bs e)
  (\vs e -> AFree (typeOf e) vs e)
  (\e1 e2 -> AOr (typeOf e1) e1 e2)
  (\ct e bs -> ACase (typeOf (head bs)) ct e bs)
  (\p e -> ABranch (patternToAnnotated p) e)
  (\e ty -> ATyped ty e ty)

patternToAnnotated :: TPattern -> APattern TypeExpr
patternToAnnotated = trTPattern (\ty name args ->
  APattern ty (name, consType ty (map snd args)) args) ALPattern

consType :: TypeExpr -> [TypeExpr] -> TypeExpr
consType ty tys = foldr FuncType ty tys

instance Typeable TypeExpr where
  typeOf = id

instance Typeable a => Typeable (AExpr a) where
  typeOf (AVar a _) = typeOf a
  typeOf (ALit a _) = typeOf a
  typeOf (AComb a _ _ _) = typeOf a
  typeOf (ALet a _ _) = typeOf a
  typeOf (AFree a _ _) = typeOf a
  typeOf (AOr a _ _) = typeOf a
  typeOf (ACase a _ _ _) = typeOf a
  typeOf (ATyped a _ _) = typeOf a

instance Typeable a => Typeable (ABranchExpr a) where
  typeOf (ABranch _ e) = typeOf e

instance Typeable a => Typeable (APattern a) where
  typeOf (APattern a _ _) = typeOf a
  typeOf (ALPattern a _) = typeOf a
