%-------------------------------=  --------------------------------------------
\subsection{Main program}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Main ( main )
> where
>
> import Control.Monad.State (MonadState(..))
> import System.IO
> import System.Directory ( copyFile )
> import System.Console.GetOpt
> import Text.Regex ( matchRegex, mkRegexWithOpts )
> import System.Environment
> import System.Exit
> import Control.Monad
> import Control.Monad.Except
> import Prelude hiding ( getContents, pi )
>
> import qualified Version as V
> import TeXCommands
> import Format
> import Options
> import State
> import StateT
> import qualified FiniteMap as FM
> import Value hiding ( str )
> import License
>
> import FileNameUtils
> --import Directory

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Main loop}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> main                          :: IO ()
> main                          =  getArgs >>= main'

> main'                         :: [String] -> IO ()
> main' args                    =  case getOpt Permute (options ++ hiddenOptions) args of
>   (o,n,[])                    -> do hSetEncoding stdin  utf8
>                                     hSetEncoding stdout utf8
>                                     hSetEncoding stderr utf8
>                                     (flags,initdirs,styles)
>                                        <- foldM (\(s,d,x) (sf,df,ns) -> do s' <- sf s
>                                                                            return (s',df d,ns ++ x))
>                                                 (state0,[],[]) o
>                                     case reverse styles of
>                                       []  -> lhs2TeX Poly flags (reverse initdirs) n
>                                           -- ks, 22.11.2005, changed default style to |Poly|
>                                       [Help]        -> quitSuccess (usageInfo uheader options)
>                                       [SearchPath]  -> quitSuccess (init . unlines $ V.searchPath)
>                                       [TeXSearchPath] ->
>                                         quitSuccess (init . unlines $
>                                           ["\\begin{code}"] ++ V.searchPath ++ ["\\end{code}"])
>                                       [Version]     -> quitSuccess programInfo
>                                       [Copying]     -> quitSuccess (programInfo ++ "\n\n" ++ copying)
>                                       [Warranty]    -> quitSuccess (programInfo ++ "\n\n" ++ warranty)
>                                       [Pre] | length n >= 3 -> preprocess flags (reverse initdirs) False n  -- used as preprocessor -pgmF -F
>                                       [Pre,Help] | length n >= 3 -> preprocess flags (reverse initdirs) True n  -- used as literate preprocessor -pgmL
>                                       [s]    -> lhs2TeX s flags (reverse initdirs) n
>                                       _      -> quitError (incompatibleStylesError styles)
>                                     when (output flags /= stdout) (hClose (output flags))
>   (_,_,errs)                  -> do hPutStrLn stderr $ concat errs
>                                     hPutStrLn stderr $ "Trying compatibility mode option handling ..."
>                                     cstyle args
>  where
>    quitSuccess s              =  do hPutStrLn stdout $ s
>                                     exitWith ExitSuccess
>    quitError s                =  do hPutStrLn stderr $ usageInfo (s ++ "\n" ++ uheader) options
>                                     exitFailure
>    incompatibleStylesError ss =  "only one style allowed from: "
>                                     ++ unwords (map (\s -> "--" ++ decode s) ss) ++ "\n"

> initState                     :: Style -> FilePath -> [FilePath] -> State -> State
> initState sty filePath ep s   =  s { style = sty,
>                                      file = filePath,
>                                      ofile = filePath,
>                                      searchpath = ep,
>                                      toggles = FM.fromList toggles0
>                                    }
>     where toggles0            =  --[(decode CodeOnly, Bool (sty == CodeOnly))]
>                                  [("style", Int (fromEnum sty))]
>                               ++ [("version", Int V.numversion)]
>                               ++ [("pre", Int V.pre)]
>                               ++ [("lang", Int (fromEnum (lang s)))]
>                               ++ [ (decode s', Int (fromEnum s')) | s' <- [(minBound :: Style) .. maxBound] ]
>                               ++ [ (decode s', Int (fromEnum s')) | s' <- [(minBound :: Lang) .. maxBound] ]
>                               -- |++ [ (s, Bool False) || s <- ["underlineKeywords", "spacePreserving", "meta", "array", "latex209", "times", "euler" ] ]|

> preprocess                    :: State -> [Class] -> Bool -> [String] -> IO ()
> preprocess flags dirs lit (f1:f2:f3:_)
>                               =  if (f1 == f2) && not lit
>                                  then copyFile f2 f3
>                                  else do c <- readFile f1
>                                          case matchRegex (mkRegexWithOpts "^%include" True False) c of
>                                            Nothing -> if lit then
>                                                          do h <- openOutputFile f3
>                                                             lhs2TeX NewCode (flags { output = h }) (Directive Include "lhs2TeX.fmt" : dirs) [f1]
>                                                             hClose h
>                                                       else copyFile f2 f3
>                                            Just _  -> -- supposed to be an lhs2TeX file
>                                                       do h <- openOutputFile f3
>                                                          lhs2TeX NewCode (flags { output = h }) dirs [f1]
>                                                          hClose h
> preprocess _ _ _ _            =  error "preprocess: too few arguments"

> lhs2TeX                       :: Style -> State -> [Class] -> [String] -> IO ()
> lhs2TeX s flags dirs files'   =  do (str, file') <- input files'
>                                     expandedpath <- expandPath (searchpath flags)
>                                     toIO (do put (initState s file' expandedpath flags)
>                                              formats (map (No 0) dirs) `catchError` abort
>                                              formatStr (addEndEOF str)
>                                              stopexternals)
>   where   addEndEOF           =  (++"%EOF\n") . unlines . lines

> input                         :: [String] -> IO (String, FilePath)
> input []                      =  do s <- getContents; return (s, "<stdin>")
> input ["-"]                   =  do s <- getContents; return (s, "<stdin>")
> input (filePath : _)          =  chaseFile [] filePath

Compatibility mode option handling.

> cstyle                        :: [String] -> IO ()
> cstyle args@(('-':a) : x)     =  case encode a of
>   Just sty                    -> cstyle' sty x
>   Nothing                     -> cstyle' Typewriter args
> cstyle args                   =  cstyle' Typewriter args

> cstyle'                       :: Style -> [String] -> IO ()
> cstyle' s args                =  let (dirs,files') = coptions args
>                                  in  lhs2TeX s state0 dirs files'

