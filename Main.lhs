%-------------------------------=  --------------------------------------------
\subsection{Main program}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Main ( main )
> where
>
> import Char
> import IO ( hPutStrLn, stderr )
> import System
>
> import TeXCommands
> import TeXParser
> import qualified Verbatim
> import qualified Typewriter
> import qualified Math
> import Directives
> import Document
> import StateT
> import qualified FiniteMap as FM
> import Auxiliaries
> import Value
> --import Directory

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Main loop}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> main				:: IO ()
> main				=  getArgs >>= main'

> main'				:: [String] -> IO ()
> main' args@(('-': a) : x)	=  case encode a of
>     Just sty			-> lhs2TeX sty x
>     Nothing			-> lhs2TeX Typewriter args
> main' args			=  lhs2TeX Typewriter args

> type Formatter		=  XIO Exc State ()

State.

> data State			=  State { style   :: Style,
>				           file    :: FilePath,	-- also used for `hugs'
>				           lineno  :: LineNo,
>				           opts    :: String, -- options for `hugs'
>				           files   :: [(FilePath, LineNo)], -- includees (?)
>				           path    :: FilePath, -- for relative includes
>				           fmts    :: Formats,
>				           subst   :: Substs,
>				           stack   :: [Formats],	-- for grouping
>				           toggles :: Toggles,	-- @%let@ defined toggles
>					   conds   :: [Bool],	-- for conditional directives
>					   align   :: Maybe Int,	-- math: internal alignment column
>				           stacks  :: (Math.Stack, Math.Stack) }	-- math: indentation stacks

Initial state.

> state0			:: Style -> FilePath -> State
> state0 sty filePath		=  State { style   = sty,
>				           file    = filePath,
>				           lineno  = 0,
>				           opts    = "",
>				           files   = [],
>				           path    = "",
>				           fmts    = FM.fromList [],
>				           subst   = FM.fromList [],
>					   stack   = [],
>				           toggles = FM.fromList toggles0,
>				           conds   = [],
>				           align   = Nothing,
>				           stacks  = ([], []) }
>     where toggles0		=  --[(decode CodeOnly, Bool (sty == CodeOnly))]
>				   [("style", Int (fromEnum sty))]
>				++ [ (decode s, Int (fromEnum s)) | s <- [CodeOnly .. Math] ]
>				-- |++ [ (s, Bool False) || s <- ["underlineKeywords", "spacePreserving", "meta", "array", "latex209", "times", "euler" ] ]|
>
> lhs2TeX			:: Style -> [String] -> IO ()
> lhs2TeX s args		=  do let (dirs, files) = options args
>				      (str, file) <- input files
>				      toIO (do store (state0 s file)
>					       formats (map (No 0) dirs) `handle` abort
>				               formatStr str)

> input				:: [String] -> IO (String, FilePath)
> input []			=  do s <- getContents; return (s, "<stdin>")
> input (filePath : _)		=  do s <- readFile filePath; return (s, filePath)

Converting command line options into directives.

> options			:: [String] -> ([Class], [String])
> options			=  foldr (<|) ([], [])
>   where
>   "-align" <| (ds, s : as)	=  (Directive Align s : ds, as)
>   "-i" <| (ds, s : as)	=  (Directive Include s : ds, as)
>   "-l" <| (ds, s : as)	=  (Directive Let s : ds, as)
>   ('-' : 'i' : s) <| (ds, as)	=  (Directive Include s : ds, as)
>   ('-' : 'l' : s) <| (ds, as)	=  (Directive Let s : ds, as)
>   s <| (ds, as)		=  (ds, s : as)
>
> formatStr			:: String -> Formatter
> formatStr str			=  formats (texparse 1 str) `handle` abort

We abort immediately if an error has occured.

> abort				:: Exc -> Formatter
> abort (msg, context)		=  do st <- fetch
>				      fromIO (hPutStrLn stderr (text st))
>				      fromIO (exitWith (ExitFailure 1))
>     where text st		=  "*** Error in " ++ at (file st) (lineno st) ++ ": \n"
>				++ unlines [ "included from " ++ at f l | (f, l) <- (files st) ]
>				++ msg ++ "\n"
>				++ unlines (take 4 (lines context))
>           at f n		=  "file " ++ f ++ " line " ++ show n

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Formatting}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> formats			:: [Numbered Class] -> Formatter
> formats []			=  return ()
> formats (No n  (Directive d s) : ts)
>     | conditional d		=  do update (\st -> st{lineno = n})
>				      st <- fetch
>				      directive d s (conds st) (toggles st) ts
> formats (No n t : ts)		=  do update (\st -> st{lineno = n})
>				      format t
>				      formats ts

> format			:: Class -> Formatter
> --format (Many ('%' : '%' : _))	=  return ()	-- @%%@-Kommentare werden entfernt
> format (Many s)		=  out (Text s)
> format (Inline s)		=  inline s
> format (Command Hs s)		=  inline s
> format (Command (Vrb b) s)	=  out (Verbatim.inline b s)
> format (Command Eval s)	=  do st <- fetch
>				      result <- fromIO (hugs (file st) (opts st) (map unNL s))
>				      inline result
> format (Command Perform s)	=  do st <- fetch
>				      result <- fromIO (hugs (file st) (opts st) s)
>				      out (Text (trim result))
>     where

Remove trailing blank line.

>     trim			=  reverse .> skip .> reverse
>
>     skip s | all isSpace t	=  u
>            | otherwise	=  s
>            where (t, u) 	=  breakAfter (== '\n') s

> format (Environment Haskell s)=  display s
> format (Environment Code s)	=  display s
> format (Environment Spec s)	=  display s
> format (Environment  Evaluate s)
>				=  do st <- fetch
>				      result <- fromIO (hugs (file st) (opts st) (map unNL s))
>				      --fromIO (hPutStrLn stderr result)	-- TEST
>				      display result
> format (Environment Hide s)	=  return ()
> format (Environment Ignore s) =  return ()
> format (Environment (Verbatim b) s)
>				=  out (Verbatim.display 120 b s)
> format (Directive Format s)	=  do st <- fetch
>				      b <- fromEither (parseFormat s)
>				      store (st{fmts = FM.add b (fmts st)})
> format (Directive Subst s)	=  do st <- fetch
>				      b <- fromEither (parseSubst s)
>				      store (st{subst = FM.add b (subst st)})
> format (Directive Include arg)=  do st <- fetch
>				      let d = path st
>				      update (\st@State{file = f', lineno = l'} ->
>				          st{file = f, files = (f', l') : files st, path = d ++ dir f})
>				      -- |d <- fromIO getCurrentDirectory|
>				      --fromIO (setCurrentDirectory (dir f))
>				      str <- fromIO (readFile (d ++ f))
>				      formatStr str
>				      --fromIO (setCurrentDirectory d)
>				      update (\st'@State{files = (f, l) : fs} ->
>				          st'{file = f, lineno = l, files = fs, path = d})
>     where f			=  withoutSpaces arg
> format (Directive Begin _)	=  update (\st -> st{stack = fmts st : stack st})
> format (Directive End _)	=  update (\st@State{stack = d:ds} -> st{fmts = d, stack = ds})

\Todo{|toggles| should be saved, as well.}

> format (Directive Let s)	=  do st <- fetch
>				      t <- fromEither (define (toggles st) s)
>				      store st{toggles = FM.add t (toggles st)}
> format (Directive Align s)
>     | all isSpace s		=  update (\st -> st{align = Nothing, stacks  = ([], [])})
>     | otherwise		=  update (\st -> st{align = Just (read s), stacks  = ([], [])})

\NB @%align@ also resets the left identation stacks.

> format (Directive File s)	=  update (\st -> st{file = withoutSpaces s})
> format (Directive Options s)	=  update (\st -> st{opts = trim s})
>     where trim		=  dropWhile isSpace .> reverse .> dropWhile isSpace .> reverse
> format (Error exc)		=  raise exc

Printing documents.
%{
%format d1
%format d2

> eject				:: Doc -> Formatter
> eject Empty			=  return ()
> eject (Text s)		=  fromIO (putStr s)
> eject (d1 :^: d2)		=  eject d1 >> eject d2
> eject (Embedded s)		=  formatStr s
> eject (Sub s ds)		=  do st <- fetch; substitute (subst st)
>     where
>     substitute d		=  case FM.lookup s d of
>         Nothing		-> raise (undef s, "")
>         Just sub		-> eject (sub ds)
>
> undef				:: String -> String
> undef s			=  "`" ++ s ++ "' is not defined;\n\
>				   \perhaps you forgot to include \"lhs2TeX.fmt\"?"

%}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Style dependent formatting}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> out				:: Doc -> Formatter
> out d				=  do st <- fetch; eject (select (style st))
>     where select CodeOnly	=  Empty
>           select _		=  d

> inline, display		:: String -> Formatter
> inline s			=  do st <- fetch
>				      d <- fromEither (select (style st) st)
>				      eject d
>   where select Verb st	=  Right (Verbatim.inline False s)
>         select Typewriter st	=  Typewriter.inline (fmts st) s
>         select Math st	=  Math.inline (fmts st) (isTrue (toggles st) auto) s
>         select CodeOnly st	=  return Empty

> display s			=  do st <- fetch
>				      (d, st') <- fromEither (select (style st) st)
>				      store st'
>				      eject d
>   where select Verb st	=  return (Verbatim.display 120 False s, st)
>         select Typewriter st	=  do d <- Typewriter.display (fmts st) s; return (d, st)
>         select Math st	=  do (d, sts) <- Math.display (fmts st) (isTrue (toggles st) auto) (stacks st) (align st) s
>				      return (d, st{stacks = sts})
>         select CodeOnly st	=  return (Text (trim s), st)

> auto				=  "autoSpacing"
> isTrue togs s			=  bool (value togs s)

Delete leading and trailing blank line (only the first!).

> trim				:: String -> String
> trim				=  skip .> reverse .> skip .> reverse
>     where
>     skip			:: String -> String
>     skip ""			=  ""
>     skip s | all isSpace t	=  u
>            | otherwise	=  s
>            where (t, u) 	=  breakAfter (== '\n') s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Conditional directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

A stack of Boolean values holds the conditions of
@%if@-directives.  Perhaps surpsingly, each @%if@ gives rise
to \emph{two} entries; if @%elif@ is not used the second entry is
always |True|, otherwise it holds the negation of all previous
conditions of the current @%if@-chain.

> directive			:: Directive -> String -> [Bool] -> Toggles
>				-> [Numbered Class] -> Formatter
> directive d s stack togs ts	=  dir d s stack
>   where
>   dir If s bs			=  do b <- fromEither (eval togs s)
>				      skipOrFormat (bool b : True : bs) ts
>   dir Elif s (b2 : b1 : bs)	=  do b <- fromEither (eval togs s)
>				      skipOrFormat (bool b : (not b2 && b1) : bs) ts
>   dir Else _ (b2 : b1 : bs)	=  skipOrFormat ((not b2 && b1) : True : bs) ts
>   dir Endif _ (b2 : b1 : bs)	=  skipOrFormat bs ts
>   dir d s _			=  raise ("spurious %" ++ decode d, s)

> skipOrFormat			:: [Bool] -> [Numbered Class] -> Formatter
> skipOrFormat stack ts		=  do update (\st -> st{conds = stack})
>				      if and stack then formats ts
>				                   else skip ts

> skip				:: [Numbered Class] -> Formatter
> skip []			=  return ()
> skip ts@(No n  (Directive d s) : _)
>     | conditional d		=  formats ts
> skip (t : ts)			=  skip ts

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Active commands}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

A simple here-script is used to call @hugs@. \NB @.script@ and @.out@
are used as intermediate files.

> hugs				:: FilePath -> String -> String -> IO String
> hugs file opts expr		=  do writeFile ".script" script
>				      system ("chmod u+x .script")
>				      system ("./.script > .out")
>				      result <- readFile ".out"
>				      return (extract result)
>     where script		=  (if null opts then "hugs " else opts) ++ " -p'" ++ magic ++ "' " ++ file ++ " <<!\n" -- |file| instead of |nondir file|
>				++ expr ++ "\n"
>				++ ":quit\n"

To extract hugs' answer we use a simple technique which should work in
most cases. The hugs prompt is set to the string |magic|; Hugs's
response then lies between the first two occurences of |magic|.

> magic				:: String
> magic				=  "!@#$^&*"
>
> extract			:: String -> String
> extract s			=  v
>     where (t, u)		=  breaks (isPrefix magic) s
>           u'			=  tail (dropWhile (/='\n') u)
>           --u'			=  drop (length magic) u
>           (v, _)		=  breaks (isPrefix magic) u'

\NB It is important that hugs does \emph{not} use the @readline@ library.
Added |tail (dropWhile (/='\n') u)| to cope with this!

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Reading files}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> dir, nondir			:: FilePath -> FilePath
> dir filePath
>     | null d			=  "./"
>     | otherwise		=  reverse d
>     where d			=  dropWhile (/= '/') (reverse filePath)
>
> nondir			=  reverse . takeWhile (/= '/') . reverse








