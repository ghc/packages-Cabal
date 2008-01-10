-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hackage.Fetch
    (
     -- * Commands
     fetch
    , -- * Utilities
      fetchPackage
    , isFetched
    , readURI
    , downloadIndex
    ) where

import Network.URI (URI,parseURI,uriScheme,uriPath)
import Network.HTTP (ConnError(..), Response(..))

import Control.Exception (bracket)
import Control.Monad (filterM)
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Hackage.Types (UnresolvedDependency (..), Repo(..), repoURL,
                      PkgInfo, packageURL, pkgInfoId, packageFile, packageDir)
import Hackage.Dependency (resolveDependencies, packagesToInstall)
import Hackage.Utils
import Hackage.HttpUtils (getHTTP)

import Distribution.Package (showPackageId)
import Distribution.Simple.Compiler (Compiler, PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils (die, notice, debug)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>), (<.>))
import System.Directory (copyFile)
import System.IO (IOMode(..), hPutStr, Handle, hClose, openBinaryFile)


readURI :: URI -> IO String
readURI uri
    | uriScheme uri == "file:" = (readFile $ uriPath uri)
    | otherwise = do
        eitherResult <- getHTTP uri 
        case eitherResult of
           Left err -> die $ "Failed to download '" ++ show uri ++ "': " ++ show err
           Right rsp
               | rspCode rsp == (2,0,0) -> return (rspBody rsp)
               | otherwise -> die $ "Failed to download '" ++ show uri ++ "': Invalid HTTP code: " ++ show (rspCode rsp)

downloadURI :: FilePath -- ^ Where to put it
            -> URI      -- ^ What to download
            -> IO (Maybe ConnError)
downloadURI path uri
    | uriScheme uri == "file:" = do
        copyFile (uriPath uri) path
        return Nothing
    | otherwise = do
        eitherResult <- getHTTP uri
        case eitherResult of
           Left err -> return (Just err)
           Right rsp
               | rspCode rsp == (2,0,0) -> withBinaryFile path WriteMode (`hPutStr` rspBody rsp) 
                                                          >> return Nothing
               | otherwise -> return (Just (ErrorMisc ("Invalid HTTP code: " ++ show (rspCode rsp))))

downloadFile :: FilePath
             -> String
             -> IO (Maybe ConnError)
downloadFile path url
    = case parseURI url of
        Just parsed -> downloadURI path parsed
        Nothing -> return (Just (ErrorMisc ("Failed to parse url: " ++ show url)))


-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: Verbosity -> PkgInfo -> IO String
downloadPackage verbosity pkg
    = do let url = packageURL pkg
             dir = packageDir pkg
             path = packageFile pkg
         debug verbosity $ "GET " ++ show url
         createDirectoryIfMissing True dir
         mbError <- downloadFile path url
         case mbError of
           Just err -> die $ "Failed to download '" ++ showPackageId (pkgInfoId pkg) ++ "': " ++ show err
           Nothing -> return path

-- Downloads an index file to [config-dir/packages/serv-id].
downloadIndex :: Repo -> IO FilePath
downloadIndex repo
    = do let url = repoURL repo ++ "/" ++ "00-index.tar.gz"
             dir = repoCacheDir repo
             path = dir </> "00-index" <.> "tar.gz"
         createDirectoryIfMissing True dir
         mbError <- downloadFile path url
         case mbError of
           Just err -> die $ "Failed to download index '" ++ show err ++ "'"
           Nothing  -> return path

-- |Returns @True@ if the package has already been fetched.
isFetched :: PkgInfo -> IO Bool
isFetched pkg = doesFileExist (packageFile pkg)

-- |Fetch a package if we don't have it already.
fetchPackage :: Verbosity -> PkgInfo -> IO String
fetchPackage verbosity pkg
    = do fetched <- isFetched pkg
         if fetched
            then do notice verbosity $ "'" ++ showPackageId (pkgInfoId pkg) ++ "' is cached."
                    return (packageFile pkg)
            else do notice verbosity $ "Downloading '" ++ showPackageId (pkgInfoId pkg) ++ "'..."
                    downloadPackage verbosity pkg

-- |Fetch a list of packages and their dependencies.
fetch :: Verbosity
      -> PackageDB
      -> [Repo]
      -> Compiler
      -> ProgramConfiguration
      -> [UnresolvedDependency]
      -> IO ()
fetch verbosity packageDB repos comp conf deps
    = do depTree <- resolveDependencies verbosity packageDB repos comp conf deps
         case packagesToInstall depTree of
           Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
           Right pkgs   -> do ps <- filterM (fmap not . isFetched) $ map fst pkgs
                              mapM_ (fetchPackage verbosity) ps

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile name mode = bracket (openBinaryFile name mode) hClose
