{-# LANGUAGE ScopedTypeVariables #-}
module Lhs2TeX.SearchPath where

import System.Directory
import System.Environment
import System.FilePath
import Control.Exception.Extensible as Exception
import Control.Monad
import Data.List

import Lhs2TeX.File

-- | The current search path can be modified in one of three ways.
-- If the new search path starts with a search path separator (a colon on
-- Unix), then the new paths are prepended to the old search path. If
-- the new path ends in a separator, it is appended. In all other cases,
-- the new search path replaces the old path completely.
modifySearchPath :: [FilePath] -> String -> [FilePath]
modifySearchPath p np
  | isSearchPathSeparator (head np) = p ++ splitSearchPath (tail np)
  | isSearchPathSeparator (last np) = splitSearchPath (init np) ++ p
  | otherwise                       = splitSearchPath np

-- | A search path can contain a whole subtree by letting it end in
-- two path separators (forward slashes on Unix).
deep :: FilePath -> FilePath
deep = (++ (replicate 2 pathSeparator))

-- | A search path can contain the value of an environment variable
-- by placing it in curly braces.
env :: String -> FilePath
env x = "{" ++ x ++ "}"

-- | Expand an enhanced search path. All environment variables and
-- recursive paths contained will be interpreted. The resulting list
-- of directories will not contain any specials anymore. TODO:
-- use list-transformed IO?
expandPath :: [FilePath] -> IO [FilePath]
expandPath s =
  let ss = concatMap splitSearchPath s
  in  do
        envs <- mapM expandEnvironment ss
        subs <- mapM findSubPaths      (concat envs)
        return (nub $ concat subs)

-- | Expand environment variables in paths.
expandEnvironment :: FilePath -> IO [String]
expandEnvironment s =
  case break (== '{') s of
    (s', "")      -> return [s]
    (s', '{' : r) -> case break (== '}') r of
                       (e, "")       -> return [s]
                       (e, '}' : r') -> do
                                          rs <- expandEnvironment r'
                                          insertEnvironment e s' rs

-- | If the path is not the root directory (purely as a sanity
-- precaution) and ends in two or more path separators, return
-- all subpaths. Otherwise, return the given file path.
findSubPaths :: FilePath -> IO [FilePath]
findSubPaths "" = return []
findSubPaths s  =
  if    all isPathSeparator (take 2 (reverse s))  -- ends in //
     && not (all isPathSeparator s)               -- not root
  then descendFrom (normalise s)
  else return [normalise s]

-- | Recursively find the subdirectories of the given directory.
descendFrom :: FilePath -> IO [FilePath]
descendFrom s =
  Exception.catch
    (do
       cs <- getDirectoryContents s
       let fs = map (s </>) $
                filter (\ x -> not (null x) && not ("." `isPrefixOf` x)) cs
       ds <- filterM doesDirectoryExist fs
       rs <- mapM descendFrom ds
       return (s : concat rs))
    (\ (_ :: IOException) -> return [s])


-- | The first argument is the environment variable to be inserted.
-- The second argument is the prefix. The third argument is the list
-- of postfixes. In particular, if the environment variable does not
-- exist, an empty list is returned.
insertEnvironment :: String -> FilePath -> [FilePath] -> IO [FilePath]
insertEnvironment e pre posts =
  do
    erss <- Exception.catch (liftM (: []) (getEnv e))
                            (\ (_ :: IOException) -> return [])
    return [ pre ++ er ++ post |
             ers <- erss, er <- splitSearchPath ers, post <- posts ]

-- | Given a search path and a file name, try to find the file.
-- Returns the contents of the file and the path where it was found.
chaseFile :: [FilePath] -> FilePath -> IO (String, FilePath)
chaseFile p fn
  | isAbsolute fn = get fn             -- don't look for abs paths
  | null p        = chaseFile ["."] fn -- current dir is default
  | otherwise     = tryAll $ map (\ d -> get (d </> fn)) p
  where
    -- get a single file at a specific location
    get f = Exception.catch
              (liftM (\ x -> (x, f)) (readTextFile f))
              (\ (_ :: IOException) ->
               ioError $ userError $ "File `" ++ fn ++ "' not found.\n")

    -- try a list of file accesses
    tryAll = foldr (\ x r -> Exception.catch x (\ (_ :: IOException) -> r))
                   (ioError $ userError $
                    "File `" ++ fn ++ "' not found in search path:\n" ++
                    showSearchPath p)

    -- show the search path line by line
    showSearchPath = unlines . map (replicate 3 ' ' ++)

-- | Default search path. TODO: set properly
defaultSearchPath :: [FilePath]
defaultSearchPath = []

