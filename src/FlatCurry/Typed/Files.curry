module FlatCurry.Typed.Files
  ( readTypedFlatCurry, readTypedFlatCurryWithParseOptions
  , readTypedFlatCurryFile
  , readTypedFlatCurryAsAnnotated, readTypedFlatCurryAsAnnotatedWithParseOptions
  , readTypedFlatCurryFileAsAnnotated) where

import FlatCurry.Typed.Types
import FlatCurry.Typed.Conversion
import FlatCurry.Annotated.Types

import Directory       (doesFileExist)
import Distribution    ( FrontendParams, FrontendTarget (..), defaultParams
                       , setQuiet, inCurrySubdir, stripCurrySuffix
                       , callFrontend, callFrontendWithParams
                       , lookupModuleSourceInLoadPath, getLoadPathForModule
                       )
import FileGoodies     (getFileInPath, lookupFileInPath)
import FilePath        (takeFileName, (</>), (<.>))

readTypedFlatCurryAsAnnotated :: String -> IO (AProg TypeExpr)
readTypedFlatCurryAsAnnotated progname =
  readTypedFlatCurry progname >>= (return . toAnnotatedFlatCurry)

readTypedFlatCurry :: String -> IO TProg
readTypedFlatCurry progname =
   readTypedFlatCurryWithParseOptions progname (setQuiet True defaultParams)

readTypedFlatCurryAsAnnotatedWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readTypedFlatCurryAsAnnotatedWithParseOptions progname options =
  readTypedFlatCurryWithParseOptions progname options
    >>= (return . toAnnotatedFlatCurry)

readTypedFlatCurryWithParseOptions :: String -> FrontendParams -> IO TProg
readTypedFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (typedFlatCurryFileName (takeFileName progname)) [""]
                                loadpath
      readTypedFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams TFCY options progname
      readTypedFlatCurryFile (typedFlatCurryFileName (dir </> takeFileName progname))

typedFlatCurryFileName :: String -> String
typedFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

readTypedFlatCurryFileAsAnnotated :: String -> IO (AProg TypeExpr)
readTypedFlatCurryFileAsAnnotated filename =
  readTypedFlatCurryFile filename >>= (return . toAnnotatedFlatCurry)

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
