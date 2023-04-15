module Language.LSP.References

import Core.Context
import Core.Core
import Core.Directory
import Core.Env
import Core.Metadata
import Core.Options
import Data.List
import Data.String
import Data.String.Parser
import Data.URI
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Log
import Server.Utils
import System.File
import System.Path

mkLocation : Ref Ctxt Defs
           => Ref LSPConf LSPConfiguration
           => NonEmptyFC -> Core (Maybe Location)
mkLocation fc = do
  let (origin, (sline, scol), (eline, ecol)) = fc
  defs <- get Ctxt
  let PhysicalIdrSrc modIdent = origin
    | _ => logD GotoReferences "Origin doesn't have an Idris file attached to it \{show origin}" >> pure Nothing
  let wdir = defs.options.dirs.working_dir
  let pkg_dirs = filter (/= ".") (defs.options.dirs.extra_dirs ++ defs.options.dirs.package_dirs)
  let exts = show <$> listOfExtensions
  Just fname_abs <- catch
      (Just . (wdir </>) <$> nsToSource replFC modIdent) -- Try local source first
      -- if not found, try looking for the file amongst the loaded packages.
      (const $ firstAvailable $ do
        pkg_dir <- pkg_dirs
        -- Assume that if the package dir is relative, then it must be relative to
        -- the working directory of the running Idris2 instance.
        let pkg_dir_abs = ifThenElse (isRelative pkg_dir) (wdir </> pkg_dir) pkg_dir
        ext <- exts
        pure (pkg_dir_abs </> toPath modIdent <.> ext))
    | _ => logD GotoReferences "Can't find file for module \{show modIdent}" >> pure Nothing

  let fname_abs_uri = "file://\{systemPathToURIPath fname_abs}"

  -- Escape is needed, filename may contain characters reserved in URI, like whitespace
  let Right uri = escapeAndParseURI fname_abs_uri
    | Left err => do logE GotoReferences "URI parse error: \{err} \{show (fname_abs_uri, sline, scol)}"
                     pure Nothing

  pure $ Just $ MkLocation uri (MkRange (MkPosition sline scol) (MkPosition eline ecol))

export
mapCM : (a -> Core (Maybe b)) -> List a -> Core (Maybe (List b))
mapCM f [] = pure $ Just []
mapCM f (x :: xs) = do
  mx <- f x
  mxs <- mapCM f xs
  pure $ do
    x <- mx
    xs <- mxs
    pure $ x :: xs

export
gotoReferences : Ref Ctxt Defs
              => Ref MD Metadata
              => Ref LSPConf LSPConfiguration
              => ReferenceParams -> Core (Maybe (List Location))
gotoReferences params = do
  logI GotoReferences "Checking for \{show params.textDocument.uri} at \{show params.position}"
  logI GotoReferences "Searching for \{show params.textDocument.uri}"
  Just (uri, _) <- gets LSPConf openFile
    | Nothing => logE GotoReferences "No open file" >> pure Nothing
  let True = uri == params.textDocument.uri
    | False => do
        logD GotoReferences "Expected request for the currently opened file \{show uri}, instead received \{show params.textDocument.uri}"
        pure Nothing

  let line = params.position.line
  let col  = params.position.character
  nameLocs <- gets MD nameLocMap
  let Just ((origin, _, _), name) = findPointInTreeLoc (line, col) nameLocs
    | Nothing => logD GotoReferences "No name found at \{show line}:\{show col}}" >> pure Nothing
  logI GotoReferences "Found name \{show name}"

  matchingNames <- gets MD (nub . map fst . filter (\((_, _, _), n) => n == name) . toList . nameLocMap)
  logI GotoReferences "Found \{show $ length matchingNames} matches"

  mapCM mkLocation matchingNames
