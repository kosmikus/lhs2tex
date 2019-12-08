> {-# LANGUAGE ScopedTypeVariables #-}
>
> module FileNameUtils          ( extension
>                               , expandPath
>                               , chaseFile
>                               , readTextFile
>                               , openOutputFile
>                               , modifySearchPath
>                               , deep, env
>                               , module System.FilePath
>                               ) where
>
> import Prelude
> import System.IO              (  openFile, IOMode(..), hPutStrLn, stderr,
>                                  hSetEncoding, hGetContents, utf8, Handle() )
> import System.IO.Error        (  isDoesNotExistError, isPermissionError )
> import System.Directory
> import System.Environment
> import Data.List
> import Control.Monad (filterM)
> import Control.Exception as E
>                               (  try, catch, IOException )
> import System.FilePath
>
> import Auxiliaries

A searchpath can be added to the front or to the back of the current path
by pre- or postfixing it with a path separator. Otherwise the new search
path replaces the current one.

> modifySearchPath              :: [FilePath] -> String -> [FilePath]
> modifySearchPath p np
>   | isSearchPathSeparator (head np)                = p ++ split
>   | isSearchPathSeparator (last np)                = split ++ p
>   | otherwise                                      = split
>   where split = splitOn isSearchPathSeparator np

> -- relPath =  joinpath

> -- absPath ps  =  directorySeparator : relPath ps

> deep                          :: FilePath -> FilePath
> deep                          =  (++(replicate 2 pathSeparator))

> env                           :: String -> FilePath
> env x                         =  "{" ++ x ++ "}"

> extension                     :: FilePath -> Maybe String
> extension fn                  =  case takeExtension fn of
>                                    ""       ->  Nothing
>                                    (_:ext)  ->  Just ext

> -- dirname = takeDirectory
> -- filename = takeFilePath
> -- basename = takeBaseName

|expandPath| does two things: it replaces curly braced strings with
environment entries, if present; furthermore, if the path ends with
more than one directory separator, all subpaths are added ...

> expandPath                    :: [String] -> IO [String]
> expandPath s                  =  do let s' = concatMap splitSearchPath s
>                                     s''  <- mapM expandEnvironment s'
>                                     s''' <- mapM findSubPaths (concat s'')
>                                     return (nub $ concat s''')

> findSubPaths                  :: String -> IO [String]
> findSubPaths ""               =  return []
> findSubPaths s                =  let rs = reverse s
>                                      (sep,rs') = span isPathSeparator rs
>                                      s'   = reverse rs'
>                                      sep' = reverse sep
>                                  in  if   null s'
>                                      then return [[head sep']] {- we don't descend from root -}
>                                      else if   length sep < 2
>                                           then return [s]
>                                           else descendFrom s'

> descendFrom                   :: String -> IO [String]
> descendFrom s                 =  E.catch (do  d <- getDirectoryContents s
>                                               {- no hidden files, no parents -}
>                                               let d' = map (\x -> s </> x)
>                                                      . filter ((/='.') . head) . filter (not . null) $ d
>                                               d'' <- filterM doesDirectoryExist d'
>                                               d''' <- mapM descendFrom d''
>                                               return (s : concat d''')
>                                          )
>                                          (\ (_ :: IOException) -> return [s])

> expandEnvironment             :: String -> IO [String]
> expandEnvironment s           =  case break (=='{') s of
>                                    (_s',"")   -> return [s]
>                                    (s','{':r) -> case break (=='}') r of
>                                                    (_e,"") -> return [s]
>                                                    (e,'}':r') -> findEnvironment e s' r'
>                                                    _ -> impossible "expandEnvironment"
>                                    _          -> impossible "expandEnvironment"
>   where findEnvironment       :: String -> String -> String -> IO [String]
>         findEnvironment e a o =  do er <- try (getEnv e)
>                                     return $ either (\ (_ :: IOException) -> [])
>                                                     (map (\x -> a ++ x ++ o) . splitOn isSearchPathSeparator)
>                                                     er

> readTextFile                  :: FilePath -> IO String
> readTextFile f                =  do h <- openFile f ReadMode
>                                     hSetEncoding h utf8
>                                     hGetContents h

> openOutputFile                :: FilePath -> IO Handle
> openOutputFile f              =  do h <- openFile f WriteMode
>                                     hSetEncoding h utf8
>                                     return h

> chaseFile                     :: [String]    {- search path -}
>                               -> FilePath -> IO (String,FilePath)
> chaseFile p fn | isAbsolute fn=  E.catch (t fn) (handle fn (err "."))
>                | p == []      =  chaseFile ["."] fn
>                | otherwise    =  s $ map (\ d -> md d ++ fn) p
>   where
>   md cs | isPathSeparator (last cs)
>                               =  cs
>         | otherwise           =  addTrailingPathSeparator cs
>   t f                         =  readTextFile f >>= \x -> return (x,f)
>   s []                        =  err $ " in search path:\n" ++ showpath
>   s (x:xs)                    =  E.catch (t x) (handle x (s xs))
>   err extra                   =  ioError
>                               $  userError $ "File `" ++ fn ++ "' not found or not readable" ++ extra
>   handle :: FilePath -> IO (String,FilePath) -> IOException -> IO (String,FilePath)
>   handle x k e                =
>                                    if isDoesNotExistError e then k
>                                    else if isPermissionError e then do
>                                      hPutStrLn stderr $ "Warning: could not access " ++ x ++ " due to permission error."
>                                      k
>                                    else ioError e
>   showpath                    =  concatMap (\x -> "   " ++ x ++ "\n") p
