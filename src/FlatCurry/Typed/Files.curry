------------------------------------------------------------------------------
--- This library supports meta-programming, i.e., the manipulation of
--- Curry programs in Curry. This library defines I/O actions
--- to read Curry programs and transform them into this representation.
---
--- This module is based on FlatCurry.Annotated.Files by Michael Hanus
--- from October 2015
---
--- @author Kai-Oliver Prott
--- @version June 2018
--- @category meta
------------------------------------------------------------------------------

module FlatCurry.Typed.Files
  ( readTypedFlatCurry, readTypedFlatCurryWithParseOptions
  , readTypedFlatCurryFile
  , readTypedFlatCurryAsAnnotated, readTypedFlatCurryAsAnnotatedWithParseOptions
  , readTypedFlatCurryFileAsAnnotated) where

import FlatCurry.Typed.Types
import FlatCurry.Typed.Conversion
import FlatCurry.Annotated.Types

import System.Directory ( doesFileExist, getFileWithSuffix)
import System.FrontendExec
                        ( FrontendParams, FrontendTarget (..), defaultParams
                        , setQuiet
                        , callFrontend, callFrontendWithParams)
import System.CurryPath ( lookupModuleSourceInLoadPath, getLoadPathForModule
                        , inCurrySubdir, stripCurrySuffix
                        )
import System.FilePath  ( takeFileName, (</>), (<.>) )

--- Read a TypedFlatCurry program and convert it to AnnotatedFlatCurry
readTypedFlatCurryAsAnnotated :: String -> IO (AProg TypeExpr)
readTypedFlatCurryAsAnnotated progname =
  readTypedFlatCurry progname >>= (return . toAnnotatedFlatCurry)

--- Read a TypedFlatCurry program
readTypedFlatCurry :: String -> IO TProg
readTypedFlatCurry progname =
   readTypedFlatCurryWithParseOptions progname (setQuiet True defaultParams)

--- Read a TypedFlatCurry program with given FrontendParameters to
--- create the file and convert it to AnnotatedFlatCurry
readTypedFlatCurryAsAnnotatedWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readTypedFlatCurryAsAnnotatedWithParseOptions progname options =
  readTypedFlatCurryWithParseOptions progname options
    >>= (return . toAnnotatedFlatCurry)

--- Read a TypedFlatCurry program with given FrontendParameters to
--- create the file
readTypedFlatCurryWithParseOptions :: String -> FrontendParams -> IO TProg
readTypedFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix (typedFlatCurryFileName (takeFileName progname)) [""]
                                loadpath
      readTypedFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams TFCY options progname
      readTypedFlatCurryFile (typedFlatCurryFileName (dir </> takeFileName progname))

typedFlatCurryFileName :: String -> String
typedFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

--- Read a file containing a TypedFlatCurry representation and convert it
--- to AnnotatedFlatCurry
readTypedFlatCurryFileAsAnnotated :: String -> IO (AProg TypeExpr)
readTypedFlatCurryFileAsAnnotated filename =
  readTypedFlatCurryFile filename >>= (return . toAnnotatedFlatCurry)

--- Read a file containing a TypedFlatCurry representation.
readTypedFlatCurryFile :: String -> IO TProg
readTypedFlatCurryFile filename = do
  filecontents <- readTypedFlatCurryFileRaw filename
  return (read filecontents)

readTypedFlatCurryFileRaw :: String -> IO String
readTypedFlatCurryFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Typed FlatCurry file '" ++ filename ++
                        "' does not exist")
