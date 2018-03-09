%-------------------------------=  --------------------------------------------
\subsection{Main program}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Main ( main )
> where
>
> import Data.Char ( isSpace )
> import Data.List ( isPrefixOf )
> import System.IO
> import System.Directory ( copyFile )
> import System.Console.GetOpt
> import Text.Regex ( matchRegex, mkRegexWithOpts )
> import System.Environment
> import System.Exit
> import System.Process
> import Control.Arrow
> import Control.Monad
> import Control.Monad.Error
> import Control.Monad.State ( MonadState(..), modify )
> import Control.Monad.Trans
> import Prelude hiding ( getContents )
>
> import Version
> import TeXCommands
> import TeXParser
> import qualified Verbatim
> import qualified Typewriter
> import qualified Math
> import qualified MathPoly as Poly
> import qualified NewCode
> import Directives
> import Document
> import State
> import StateT
> import qualified FiniteMap as FM
> import Auxiliaries
> import Value
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
> main' args                    =  case getOpt Permute options args of
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
>                                       [SearchPath]  -> quitSuccess (init . unlines $ searchPath)
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

> type Formatter                =  XIO Exc State ()

> initState                     :: Style -> FilePath -> [FilePath] -> State -> State
> initState sty filePath ep s   =  s { style = sty,
>                                      file = filePath,
>                                      ofile = filePath,
>                                      searchpath = ep,
>                                      toggles = FM.fromList toggles0
>                                    }
>     where toggles0            =  --[(decode CodeOnly, Bool (sty == CodeOnly))]
>                                  [("style", Int (fromEnum sty))]
>                               ++ [("version", Int numversion)]
>                               ++ [("pre", Int pre)]
>                               ++ [("lang", Int (fromEnum (lang s)))]
>                               ++ [ (decode s, Int (fromEnum s)) | s <- [(minBound :: Style) .. maxBound] ]
>                               ++ [ (decode s, Int (fromEnum s)) | s <- [(minBound :: Lang) .. maxBound] ]
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
> lhs2TeX s flags dirs files    =  do (str, file) <- input files
>                                     expandedpath <- expandPath (searchpath flags)
>                                     toIO (do put (initState s file expandedpath flags)
>                                              formats (map (No 0) dirs) `catchError` abort
>                                              formatStr (addEndEOF str)
>                                              stopexternals)
>   where   addEndEOF           =  (++"%EOF\n") . unlines . lines

> input                         :: [String] -> IO (String, FilePath)
> input []                      =  do s <- getContents; return (s, "<stdin>")
> input ["-"]                   =  do s <- getContents; return (s, "<stdin>")
> input (filePath : _)          =  chaseFile [] filePath

Converting command line options into directives.

> uheader                       :: String
> uheader                       =  "lhs2TeX [ options ] files\n\nAvailable options:\n"

ks, 20.07.2003: The short option for @--align@ has been changed into @-A@. Otherwise
@-align@ would not trigger compatibility mode, but be interpreted as a valid option
usage.

ks, 24.03.2004: The long option @--verbose@ has been removed for now,
because with some versions of GHC it triggers ambiguity errors with
@--verb@.

> options                       :: [OptDescr (State -> IO State,[Class] -> [Class],[Style])]
> options                       =
>   [ Option ['h','?'] ["help"](NoArg (return, id, [Help]))                                 "get this help"
>   , Option ['v'] [] {- ["verbose"] -}
>                              (NoArg (\s -> return $ s { verbose = True }, id, []))        "be verbose"
>   , Option ['V'] ["version"] (NoArg (return, id, [Version]))                              "show version"
>   , Option []    ["tt"]      (NoArg (return, id, [Typewriter]))                           "typewriter style (deprecated)"
>   , Option []    ["math"]    (NoArg (return, id, [Math]))                                 "math style (deprecated)"
>   , Option []    ["poly"]    (NoArg (return, id, [Poly]))                                 "poly style (default)"
>   , Option []    ["code"]    (NoArg (return, id, [CodeOnly]))                             "code style (deprecated)"
>   , Option []    ["newcode"] (NoArg (return, id, [NewCode]))                              "new code style"
>   , Option []    ["verb"]    (NoArg (return, id, [Verb]))                                 "verbatim (deprecated)"
>   , Option []    ["haskell"] (NoArg (\s -> return $ s { lang = Haskell}, id, []))         "Haskell lexer (default)"
>   , Option []    ["agda"]    (NoArg (\s -> return $ s { lang = Agda}, id, []))            "Agda lexer"
>   , Option []    ["pre"]     (NoArg (return, id, [Pre]))                                  "act as ghc preprocessor"
>   , Option ['o'] ["output"]  (ReqArg (\f -> (\s -> do h <- openOutputFile f
>                                                       return $ s { output = h }, id, [])) "file") "specify output file"
>   , Option []    ["file-directives"]
>                              (NoArg (\s -> return $ s { fldir = True }, id, []))          "generate %file directives"
>   , Option []    ["no-pragmas"]
>                              (NoArg (\s -> return $ s { pragmas = False }, id, []))       "no LINE pragmas"
>   , Option ['A'] ["align"]   (ReqArg (\c -> (return, (Directive Align c:), [])) "col")    "align at <col>"
>   , Option ['i'] ["include"] (ReqArg (\f -> (return, (Directive Include f:), [])) "file") "include <file>"
>   , Option ['l'] ["let"]     (ReqArg (\s -> (return, (Directive Let s:), [])) "equation") "assume <equation>"
>   , Option ['s'] ["set"]     (ReqArg (\s -> (return, (Directive Let (s ++ " = True"):), [])) "flag")  "set <flag>"
>   , Option ['u'] ["unset"]   (ReqArg (\s -> (return, (Directive Let (s ++ " = False"):), [])) "flag") "unset <flag>"
>   , Option ['P'] ["path"]    (ReqArg (\p -> (\s -> return $ s { searchpath = modifySearchPath (searchpath s) p }, id , [])) "path")
>                                                                                       "modify search path"
>   , Option []    ["searchpath"]
>                              (NoArg (return, id, [SearchPath]))                           "show searchpath"
>   , Option []    ["copying"] (NoArg (return, id, [Copying]))                              "display license"
>   , Option []    ["warranty"](NoArg (return, id, [Warranty]))                             "info about warranty"
>   ]
>
> formatStr                     :: String -> Formatter
> formatStr str                 =  formats (texparse 1 str) `catchError` abort

Compatibility mode option handling.

> cstyle                        :: [String] -> IO ()
> cstyle args@(('-':a) : x)     =  case encode a of
>   Just sty                    -> cstyle' sty x
>   Nothing                     -> cstyle' Typewriter args
> cstyle args                   =  cstyle' Typewriter args

> cstyle'                       :: Style -> [String] -> IO ()
> cstyle' s args                =  let (dirs,files) = coptions args
>                                  in  lhs2TeX s state0 dirs files

> coptions                      :: [String] -> ([Class], [String])
> coptions                      =  foldr (<|) ([], [])
>   where
>   "-align" <| (ds, s : as)    =  (Directive Align s : ds, as)
>   "-i" <| (ds, s : as)        =  (Directive Include s : ds, as)
>   "-l" <| (ds, s : as)        =  (Directive Let s : ds, as)
>   ('-' : 'i' : s) <| (ds, as) =  (Directive Include s : ds, as)
>   ('-' : 'l' : s) <| (ds, as) =  (Directive Let s : ds, as)
>   s <| (ds, as)               =  (ds, s : as)


We abort immediately if an error has occured.

> abort                         :: Exc -> Formatter
> abort (msg, context)          =  do st <- get
>                                     liftIO (hPutStrLn stderr (text st))
>                                     liftIO (exitWith (ExitFailure 1))
>     where text st             =  "*** Error in " ++ at (file st) (lineno st) ++ ": \n"
>                               ++ unlines [ "included from " ++ at f l | (f, l) <- files st ]
>                               ++ msg ++ "\n"
>                               ++ unlines (take 4 (lines context))
>           at f n              =  "file " ++ f ++ " line " ++ show n

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Formatting}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> formats                       :: [Numbered Class] -> Formatter
> formats []                    =  return ()
> formats (No n  (Directive d s) : ts)
>     | conditional d           =  do modify (\st -> st{lineno = n})
>                                     st <- get
>                                     directive (lang st)
>                                               d s (file st,n)
>                                               (conds st) (toggles st) ts
> formats (No n t : ts)         =  do modify (\st -> st{lineno = n})
>                                     format t
>                                     formats ts

> format                        :: Class -> Formatter
> -- |format (Many ('%' : '%' : _))     =  return ()|   -- @%%@-comments used to be removed
> format (Many s)               =  out (Text s)
> format (Inline s)             =  inline s
> format (Command Hs s)         =  inline s
> format (Command (Vrb b) s)    =  out (Verbatim.inline b s)
> format (Command Eval s)       =  do st <- get
>                                     unless (style st `elem` [CodeOnly,NewCode]) $
>                                       do result <- external (map unNL s)
>                                          inline result
> format (Command Perform s)    =  do st <- get
>                                     unless (style st `elem` [CodeOnly,NewCode]) $
>                                       do result <- external (map unNL s)
>                                          modify (\st@State{file = f', lineno = l'} ->
>                                                    st{file = "<perform>", files = (f', l') : files st})
>                                          liftIO (when (verbose st) (hPutStr stderr $ "(" ++ "<perform>"))
>                                          formatStr (addEndNL result)
>                                          modify (\st'@State{files = (f, l) : fs} ->
>                                                    st'{file = f, lineno = l, files = fs})
>                                          liftIO (when (verbose st) (hPutStrLn stderr $ ")"))
>     where
>     addEndNL                  =  (++"\n") . unlines . lines

Remove trailing blank line.

>     trim                      =  reverse >>> skip >>> reverse
>
>     skip s | all isSpace t    =  u
>            | otherwise        =  s
>            where (t, u)       =  breakAfter (== '\n') s

> format (Environment Haskell_ s)
>                               =  display s
> format (Environment Code s)   =  display s
> format (Environment Spec s)   =  do st <- get
>                                     unless (style st `elem` [CodeOnly,NewCode]) $
>                                       display s
> format (Environment Evaluate s)
>                               =  do st <- get
>                                     unless (style st `elem` [CodeOnly,NewCode]) $
>                                       do result <- external s
>                                          display result
> format (Environment Hide s)   =  return ()
> format (Environment Ignore s) =  return ()
> format (Environment (Verbatim b) s)
>                               =  out (Verbatim.display 120 b s)
> format (Directive Format s)   =  do st <- get
>                                     b@(n,e) <- fromEither (parseFormat (lang st) s)
>                                     put (st{fmts = FM.add b (fmts st)})
> format (Directive Subst s)    =  do st <- get
>                                     b <- fromEither (parseSubst (lang st) s)
>                                     put (st{subst = FM.add b (subst st)})
> format (Directive Include arg)=  do st <- get
>                                     let d  = path st
>                                     let sp = searchpath st
>                                     modify (\st@State{file = f', lineno = l'} ->
>                                         st{file = f, files = (f', l') : files st, path = d ++ dir f})
>                                     -- |d <- liftIO getCurrentDirectory|
>                                     -- |liftIO (setCurrentDirectory (dir f))|
>                                     (str,f) <- liftIO (chaseFile sp (d ++ f))
>                                     modify (\st -> st { file = f })
>                                     liftIO (when (verbose st) (hPutStr stderr $ "(" ++ f))
>                                     formatStr (addEndNL str)
>                                     -- |liftIO (setCurrentDirectory d)|
>                                     modify (\st'@State{files = (f, l) : fs} ->
>                                         st'{file = f, lineno = l, files = fs, path = d})
>                                     liftIO (when (verbose st) (hPutStrLn stderr $ ")"))
>     where f                   =  withoutSpaces arg
>           addEndNL            =  (++"\n") . unlines . lines

ks, 25.01.2003: I added the above function at the suggestion of NAD, but
I am not completely sure if this is the right thing to do. Maybe we should
strip blank lines from the end of a file as well, maybe we should do nothing
at all. Hard to say what people think is intuitive. Anyway, the reason why
I added it is this: if an %include directive is immediately followed
by another line and the included file does not end in a blank line, then
there will not be a single space between the last character of the included
file and the first character of the following line. It would be possible
to split a TeX control sequence over two different files that way. Seems
strange. So we add a newline, or even two if none has been there before,
to make sure that exactly one linebreak ends up in the output, but not
more, as a double newline is interpreted as a \par by TeX, and that might
also not be desired.

> format (Directive Begin _)    =  modify (\st -> st{stack = fmts st : stack st})
> format (Directive End _)      =  do st <- get
>                                     when (null (stack st)) $
>                                       do liftIO (hPutStrLn stderr $ "unbalanced %} in line "
>                                                                       ++ show (lineno st))
>                                          modify (\st -> st{stack = [fmts st]})
>                                     modify (\st@State{stack = d:ds} -> st{fmts = d, stack = ds})

ks, 11.09.03: added exception handling for unbalanced grouping

\Todo{|toggles| should be saved, as well.}

> format (Directive Let s)      =  do st <- get
>                                     t <- fromEither (define (lang st) (toggles st) s)
>                                     put st{toggles = FM.add t (toggles st)}
> format (Directive Align s)
>     | all isSpace s           =  modify (\st -> st{align = Nothing, stacks  = ([], [])})
>     | otherwise               =  modify (\st -> st{align = Just (read s), stacks  = ([], [])})

\NB @%align@ also resets the left identation stacks.

Also, the @poly@ directives @%separation@ and @%latency@ reset
the corresponding indentation stack |pstack|.

> format (Directive Separation s )
>                               =  modify (\st -> st{separation = read s, pstack = []})
> format (Directive Latency s)  =  modify (\st -> st{latency = read s, pstack = []})

> format (Directive File s)     =  modify (\st -> st{file = withoutSpaces s})
> format (Directive Options s)  =  modify (\st -> st{opts = trim s})
>     where trim                =  dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse

> format (Error exc)            =  throwError exc

Printing documents.
%{
%format d1
%format d2

> eject                         :: Doc -> Formatter
> eject Empty                   =  return ()
> eject (Text s)                =  do  st <- get
>                                      let (ls,enl) = checkNLs 0 s
>                                      when (fldir st && not (null s) && atnewline st && (ofile st /= file st || olineno st /= lineno st)) $
>                                        do  liftIO (hPutStr (output st) ("%file " ++ show (lineno st) ++ " " ++ show (file st) ++ "\n"))
>                                            put (st { ofile = file st, olineno = lineno st })
>
>                                      liftIO (hPutStr (output st) s)
>                                      modify (\st -> st { olineno = olineno st + ls, atnewline = enl (atnewline st)})
>     where
>     checkNLs n ('\n':[])      =  (n+1,const True)
>     checkNLs n (_:[])         =  (n,const False)
>     checkNLs n []             =  (n,id)
>     checkNLs n ('\n':xs)      =  checkNLs (n+1) xs
>     checkNLs n (_:xs)         =  checkNLs n xs
> eject (d1 :^: d2)             =  eject d1 >> eject d2
> eject (Embedded s)            =  formatStr s
> eject (Sub s ds)              =  do st <- get; substitute (subst st)
>     where
>     substitute d              =  case FM.lookup s d of
>         Nothing               -> throwError (undef s, "")
>         Just sub              -> eject (sub ds)
>
> undef                         :: String -> String
> undef s                       =  "`" ++ s ++ "' is not defined;\n\
>                                  \perhaps you forgot to include \"polycode.fmt\" (or \"lhs2TeX.fmt\")?"

%}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Style dependent formatting}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> out                           :: Doc -> Formatter
> out d                         =  do st <- get; eject (select (style st))
>     where select CodeOnly     =  Empty
>           select NewCode      =  Empty
>           select _            =  d

> inline, display               :: String -> Formatter
> inline s                      =  do st <- get
>                                     d <- fromEither (select (style st) st)
>                                     eject d
>   where select Verb st        =  Right (Verbatim.inline False s)
>         select Typewriter st  =  Typewriter.inline (lang st) (fmts st) s
>         select Math st        =  Math.inline (lang st) (fmts st) (isTrue (toggles st) auto) s
>         select Poly st        =  Poly.inline (lang st) (fmts st) (isTrue (toggles st) auto) s
>         select CodeOnly st    =  return Empty
>         select NewCode st     =  return Empty   -- generate PRAGMA or something?

> display s                     =  do st <- get
>                                     (d, st') <- fromEither (select (style st) st)
>                                     put st'
>                                     eject d
>   where select Verb st        =  return (Verbatim.display 120 False s, st)
>         select Typewriter st  =  do d <- Typewriter.display (lang st) (fmts st) s; return (d, st)
>         select Math st        =  do (d, sts) <- Math.display (lang st) (fmts st) (isTrue (toggles st) auto) (stacks st) (align st) s
>                                     return (d, st{stacks = sts})
>         select Poly st        =  do (d, pstack') <- Poly.display (lang st) (lineno st + 1) (fmts st) (isTrue (toggles st) auto) (separation st) (latency st) (pstack st) s
>                                     return (d, st{pstack = pstack'})
>         select NewCode st     =  do d <- NewCode.display (lang st) (fmts st) s
>                                     let p = sub'pragma $ Text ("LINE " ++ show (lineno st + 1) ++ " " ++ show (takeFileName $ file st))
>                                     return ((if pragmas st then ((p <<>> sub'nl) <<>>) else id) d, st)
>         select CodeOnly st    =  return (Text (trim s), st)

> auto                          =  "autoSpacing"
> isTrue togs s                 =  bool (value togs s)

Delete leading and trailing blank line (only the first!).

> trim                          :: String -> String
> trim                          =  skip >>> reverse >>> skip >>> reverse
>     where
>     skip                      :: String -> String
>     skip ""                   =  ""
>     skip s | all isSpace t    =  u
>            | otherwise        =  s
>            where (t, u)       =  breakAfter (== '\n') s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Conditional directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

A stack of Boolean values holds the conditions of
@%if@-directives.  Perhaps surpsingly, each @%if@ gives rise
to \emph{two} entries; if @%elif@ is not used the second entry is
always |True|, otherwise it holds the negation of all previous
conditions of the current @%if@-chain.

ks, 16.08.2004: At the end of the input, we might want to check for unbalanced if's or
groups.

> directive                     :: Lang -> Directive -> String
>                               -> (FilePath,LineNo) -> [CondInfo] -> Toggles
>                               -> [Numbered Class] -> Formatter
> directive lang d s (f,l) stack togs ts
>                               =  dir d s stack
>   where
>   dir If s bs                 =  do b <- fromEither (eval lang togs s)
>                                     skipOrFormat ((f, l, bool b, True) : bs) ts
>   dir Elif s ((f,l,b2,b1):bs) =  do b <- fromEither (eval lang togs s)
>                                     skipOrFormat ((f, l, bool b, not b2 && b1) : bs) ts
>   dir Else _ ((f,l,b2,b1):bs) =  skipOrFormat ((f, l, not b2 && b1, True) : bs) ts
>   dir Endif _ ((f,l,b2,b1):bs)=  skipOrFormat bs ts
>   dir EOF _ []                =  return ()  -- nothing left to do
>   dir EOF s bs                =  throwError (init $ unlines (map unBalancedIf bs), s)
>   dir d s _                   =  throwError ("spurious %" ++ decode d, s)

> skipOrFormat                  :: [CondInfo] -> [Numbered Class] -> Formatter
> skipOrFormat stack ts         =  do  modify (\st -> st{conds = stack})
>                                      if andS stack  then formats ts
>                                                     else skip ts

> andS                          :: [CondInfo] -> Bool
> andS                          =  all (\(_,_,x,y) -> x && y)

> unBalancedIf                  :: CondInfo -> String
> unBalancedIf (f,l,_,_)        =  "%if at " ++ f ++ " line " ++ show l ++ " not closed"

> skip                          :: [Numbered Class] -> Formatter
> skip []                       =  return ()
> skip ts@(No n  (Directive d s) : _)
>     | conditional d           =  formats ts
> skip (t : ts)                 =  skip ts

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Active commands}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, 23.10.2003: extended to work with @ghci@, too.
ks, 03.01.2004: fixed to work with @ghci-6.2@, hopefully without breaking
@hugs@ or old @ghci@ compatibility.

New, 26.01.2006: we're now starting an external process @ghci@ or @hugs@
using the System.Process library. The process is then reused for subsequent
computations, which should dramatically improve compilation time for
documents that make extensive use of @\eval@ and @\perform@.

The function |external| can be used to call the process. It is discouraged
to call any programs except @ghci@ or @hugs@, because we make a number of
assumptions about the program being called. Input is the expression to evaluate.
Output is the result in string form.

> external                      :: String -> XIO Exc State String
> external expr                 =  do st <- get
>                                     let os  =  opts st
>                                         f   =  file st
>                                         ex  =  externals st
>                                         ghcimode       =  "ghci" `isPrefixOf` os
>                                         cmd
>                                           | ghcimode   =  os ++ " -v0 -ignore-dot-ghci " ++ f
>                                           | otherwise  =  (if null os then "hugs " else os ++ " ") ++ f
>                                         script         =  "putStrLn " ++ show magic ++ "\n"
>                                                             ++ expr ++ "\n"
>                                                             ++ "putStrLn " ++ show magic ++ "\n"
>                                     pi <- case FM.lookup f ex of
>                                             Just pi  ->  return pi
>                                             Nothing  ->  -- start new external process
>                                                          liftIO $ do
>                                                            when (verbose st) $
>                                                              hPutStrLn stderr $ "Starting external process: " ++ cmd
>                                                            runInteractiveCommand cmd
>                                     put (st {externals = FM.add (f,pi) ex})
>                                     let (pin,pout,_,_) = pi
>                                     liftIO $ do
>                                       -- hPutStrLn stderr ("sending: " ++ script)
>                                       hPutStr pin script
>                                       hFlush pin
>                                       extract' pout

This function can be used to stop all external processes by sending the
@:q@ command to them.

> stopexternals                 :: Formatter
> stopexternals                 =  do st <- get
>                                     let ex   =  externals st
>                                         pis  =  map (ex FM.!) (FM.keys ex)
>                                     when (not . null $ pis) $ liftIO $ do
>                                       when (verbose st) $
>                                         hPutStrLn stderr $ "Stopping external processes."
>                                       mapM_ (\(pin,_,_,pid) -> do  hPutStrLn pin ":q"
>                                                                    hFlush pin
>                                                                    waitForProcess pid) pis

To extract the answer from @ghci@'s or @hugs@' output
we use a simple technique which should work in
most cases: we print the string |magic| before and after
the expression we are interested in. We assume that everything
that appears before the first occurrence of |magic| on the same
line is the prompt, and everything between the first |magic|
and the second |magic| plus prompt is the result we look for.

> magic                         :: String
> magic                         =  "!@#$^&*"
>
> extract'                      :: Handle -> IO String
> extract' h                    =  fmap (extract . unlines) (readMagic 2)
>     where readMagic           :: Int -> IO [String]
>           readMagic 0         =  return []
>           readMagic n         =  do  l <- hGetLine h
>                                      -- hPutStrLn stderr ("received: " ++ l)
>                                      let n'  |  (null . snd . breaks (isPrefixOf magic)) l  =  n
>                                              |  otherwise                                   =  n - 1
>                                      fmap (l:) (readMagic n')

> extract                       :: String -> String
> extract s                     =  v
>     where (t, u)              =  breaks (isPrefixOf magic) s
>           -- t contains everything up to magic, u starts with magic
>           -- |u'                      =  tail (dropWhile (/='\n') u)|
>           pre                 =  reverse . takeWhile (/='\n') . reverse $ t
>           prelength           =  if null pre then 0 else length pre + 1
>           -- pre contains the prefix of magic on the same line
>           u'                  =  drop (length magic + prelength) u
>           -- we drop the magic string, plus the newline, plus the prefix
>           (v, _)              =  breaks (isPrefixOf (pre ++ magic)) u'
>           -- we look for the next occurrence of prefix plus magic

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Reading files}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> dir                           :: FilePath -> FilePath
> dir filePath
>     | null d                  =  ""
>     | otherwise               =  reverse d
>     where d                   =  dropWhile (/= '/') (reverse filePath)

