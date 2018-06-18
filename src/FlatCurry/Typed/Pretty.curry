--- --------------------------------------------------------------------------
--- This library provides pretty-printers for TypedFlatCurry modules
--- and all substructures (e.g., expressions). Note that types are
--- ignored for pretty-printing.
--- The mocule is based on the pretty-printer for AnnotatedFlatCurry by
--- Bjoern Peemoeller from October 2015
---
--- @author  Kai-Oliver Prott
--- @version June 2018
--- @category meta
--- --------------------------------------------------------------------------
module FlatCurry.Typed.Pretty where

import Text.Pretty

import FlatCurry.Typed.Types

--- pretty-print a FlatCurry module
ppProg :: TProg -> Doc
ppProg (TProg m is ts fs os) = compose (<$+$>)
  [ ppHeader    m ts fs
  , ppImports   is
  , ppOpDecls   os
  , ppTypeDecls ts
  , ppFuncDecls fs
  ]

--- pretty-print the module header
ppHeader :: String -> [TypeDecl] -> [TFuncDecl] -> Doc
ppHeader m ts fs = indent $
  sep [text "module" <+> text m, ppExports ts fs, text "where"]

--- pretty-print the export list
ppExports :: [TypeDecl] -> [TFuncDecl] -> Doc
ppExports ts fs = tupledSpaced (map ppTypeExport ts ++ ppFuncExports fs)

--- pretty-print a type export
ppTypeExport :: TypeDecl -> Doc
ppTypeExport (Type    qn vis _ cs)
  | vis == Private      = empty
  | all isPublicCons cs = ppPrefixOp qn <+> text "(..)"
  | otherwise           = ppPrefixOp qn <+> tupled (ppConsExports cs)
    where isPublicCons (Cons _ _ v _) = v == Public
ppTypeExport (TypeSyn qn vis _ _ )
  | vis == Private = empty
  | otherwise      = ppPrefixOp qn

--- pretty-print the export list of constructors
ppConsExports :: [ConsDecl] -> [Doc]
ppConsExports cs = [ ppPrefixOp qn | Cons qn _ Public _ <- cs]

--- pretty-print the export list of functions
ppFuncExports :: [TFuncDecl] -> [Doc]
ppFuncExports fs = [ ppPrefixOp qn | TFunc qn _ Public _ _ <- fs]

--- pretty-print a list of import statements
ppImports :: [String] -> Doc
ppImports = vsep . map ppImport

--- pretty-print a single import statement
ppImport :: String -> Doc
ppImport m = indent $ text "import" <+> text m

--- pretty-print a list of operator fixity declarations
ppOpDecls :: [OpDecl] -> Doc
ppOpDecls = vsep . map ppOpDecl

--- pretty-print a single operator fixity declaration
ppOpDecl :: OpDecl -> Doc
ppOpDecl (Op qn fix n) = indent $ ppFixity fix <+> int n <+> ppInfixOp qn

--- pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

--- pretty-print a list of type declarations
ppTypeDecls :: [TypeDecl] -> Doc
ppTypeDecls = compose (<$+$>) . map ppTypeDecl

--- pretty-print a type declaration
ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (Type    qn _ vs cs) = indent $ text "data" <+> ppQName qn
  <+> hsep (map ppTVarIndex vs) <$$> ppConsDecls cs
ppTypeDecl (TypeSyn qn _ vs ty) = indent $ text "type" <+> ppQName qn
  <+> hsep (map ppTVarIndex vs) </> equals <+> ppTypeExp ty

--- pretty-print the constructor declarations
ppConsDecls :: [ConsDecl] -> Doc
ppConsDecls cs = vsep $
  zipWith (<+>) (equals : repeat bar) (map ppConsDecl cs)

--- pretty print a single constructor
ppConsDecl :: ConsDecl -> Doc
ppConsDecl (Cons qn _ _ tys) = hsep $ ppPrefixOp qn : map (ppTypeExpr 2) tys

--- pretty a top-level type expression
ppTypeExp :: TypeExpr -> Doc
ppTypeExp = ppTypeExpr 0

--- pretty-print a type expression
ppTypeExpr :: Int -> TypeExpr -> Doc
ppTypeExpr _ (TVar           v) = ppTVarIndex v
ppTypeExpr p (FuncType ty1 ty2) = parensIf (p > 0) $
  ppTypeExpr 1 ty1 </> rarrow <+> ppTypeExp ty2
ppTypeExpr p (TCons     qn tys)
  | isListId qn && length tys == 1 = brackets (ppTypeExp (head tys))
  | isTupleId qn                   = tupled   (map ppTypeExp tys)
  | otherwise                      = parensIf (p > 1 && not (null tys)) $ sep
                                      (ppPrefixOp qn : map (ppTypeExpr 2) tys)
ppTypeExpr _ (ForallType _ _) = error $ "FlatCurry.Typed.Pretty.ppTypeExp: " ++
                                       "ForallType in TypedFlatCurry"

--- pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ chr c : if n == 0 then [] else show n
               | n <- [0 ..], c <- [ord 'a' .. ord 'z']
               ]

--- pretty-print a list of function declarations
ppFuncDecls :: [TFuncDecl] -> Doc
ppFuncDecls = compose (<$+$>) . map ppFuncDecl

--- pretty-print a function declaration
ppFuncDecl :: TFuncDecl -> Doc
ppFuncDecl (TFunc qn _ _ ty r)
  =    indent (sep [ppPrefixOp qn, text "::", ppTypeExp ty])
  <$$> indent (ppPrefixOp qn <+> ppRule r)

--- pretty-print a function rule
ppRule :: TRule -> Doc
ppRule (TRule    vs e)
  | null vs   = equals <+> ppExp e
  | otherwise = hsep (map ppTVarEIndex vs) </> equals <+> ppExp e
ppRule (TExternal _ e) = text "external" <+> dquotes (text e)

--- pretty-print a top-level expression
ppExp :: TExpr -> Doc
ppExp = ppExpr 0

--- pretty-print an expression
ppExpr :: Int -> TExpr -> Doc
ppExpr _ (TVarE  _       v) = ppVarEIndex v
ppExpr _ (TLit   _       l) = ppLiteral l
ppExpr p (TComb  _ _ qn es) = ppComb p qn es
ppExpr p (TFree       vs e)
  | null vs                 = ppExpr p e
  | otherwise               = parensIf (p > 0) $ sep
                              [ text "let"
                                <+> encloseSep empty empty comma
                                    (map ppTVarEIndex vs)
                                <+> text "free"
                              , text "in" </> ppExp e
                              ]
ppExpr p (TLet        ds e) = parensIf (p > 0) $ sep
                              [text "let" <+> ppDecls ds, text "in" <+> ppExp e]
ppExpr p (TOr        e1 e2) = parensIf (p > 0)
                            $ ppExpr 1 e1 <+> text "?" <+> ppExpr 1 e2
ppExpr p (TCase    ct e bs) = parensIf (p > 0) $ indent
                            $ ppCaseType ct <+> ppExpr 1 e <+> text "of"
                              <$$> vsep (map ppBranch bs)
ppExpr p (TTyped      e ty) = parensIf (p > 0)
                            $ ppExp e <+> text "::" <+> ppTypeExp ty

--- pretty-print an annotated variable
ppTVarEIndex :: (VarIndex, _) -> Doc
ppTVarEIndex (i, _) | i < 0     = text $ 'x' : show (negate i)
                   | otherwise = text $ 'v' : show i

--- pretty-print a variable
ppVarEIndex :: VarIndex -> Doc
ppVarEIndex i | i < 0     = text $ 'x' : show (negate i)
             | otherwise = text $ 'v' : show i

--- pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = int i
ppLiteral (Floatc f) = float f
ppLiteral (Charc  c) = text (showEscape c)

--- Escape character literal
showEscape :: Char -> String
showEscape c
  | o <   10  = "'\\00" ++ show o ++ "'"
  | o <   32  = "'\\0"  ++ show o ++ "'"
  | o == 127  = "'\\127'"
  | otherwise = show c
  where o = ord c

--- Pretty print a constructor or function call
ppComb :: Int -> QName -> [TExpr] -> Doc
ppComb p qn es | isListId  qn && null es = text "[]"
               | isTupleId qn            = tupled (map ppExp es)
               | otherwise               = case es of
  []             -> ppPrefixOp qn
  [e1,e2]
    | isInfixOp qn -> parensIf (p > 0)
                    $ sep [ppExpr 1 e1, ppInfixOp qn, ppExpr 1 e2]
  _                -> parensIf (p > 0)
                    $ sep (ppPrefixOp qn : map (ppExpr 1) es)

--- pretty-print a list of declarations
ppDecls :: [((VarIndex, _), TExpr)] -> Doc
ppDecls = semiBracesSpaced . map ppDecl

--- pretty-print a single declaration
ppDecl :: ((VarIndex, _), TExpr) -> Doc
ppDecl (v, e) = ppTVarEIndex v <+> equals <+> ppExp e

--- Pretty print the type of a case expression
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

--- Pretty print a case branch
ppBranch :: TBranchExpr -> Doc
ppBranch (TBranch p e) = ppPattern p <+> rarrow <+> indent (ppExp e)

--- Pretty print a pattern
ppPattern :: TPattern -> Doc
ppPattern (TPattern _ c vs)
  | isListId c && null vs = text "[]"
  | isTupleId c           = tupled (map ppTVarEIndex vs)
  | otherwise             = case vs of
  [v1,v2] | isInfixOp c -> ppTVarEIndex v1 <+> ppInfixOp c <+> ppTVarEIndex v2
  _                     -> hsep (ppPrefixOp c : map ppTVarEIndex vs)
ppPattern (TLPattern _ l) = ppLiteral l

--- pretty-print a prefix operator
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parensIf (isInfixOp qn) (ppQName qn)

--- pretty-print an infix operator
ppInfixOp :: QName -> Doc
ppInfixOp qn = if isInfixOp qn then ppQName qn else bquotes (ppQName qn)

--- Pretty-print a qualified name
ppQName :: QName -> Doc
ppQName (m, i)
  | m == "Prelude" && i `elem` builtin = text i
  | otherwise                          = text $ m ++ '.' : i
  where builtin = [ "[]", "?", ":", "+", "-", "*", "<"
                  , ">", "<=", ">=", "==", "/=", "&>", "&" ]

--- Check whether an operator is an infix operator
isInfixOp :: QName -> Bool
isInfixOp = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents a list
isListId :: QName -> Bool
isListId (m, i) = m `elem` ["Prelude", ""] && i == "[]"

--- Check whether an identifier represents a tuple
isTupleId :: QName -> Bool
isTupleId (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

--- Indentation
indent :: Doc -> Doc
indent = nest 2
