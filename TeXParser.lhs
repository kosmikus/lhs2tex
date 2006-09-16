%-------------------------------=  --------------------------------------------
\subsection{Pseudo-\TeX\ Parser}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module TeXParser              (  texparse  )
> where
> import Data.Char              (  isSpace, isAlpha  )
> import TeXCommands
> import Auxiliaries hiding     (  breaks  )

%endif

Care is taken that no character of the input is lost; this is necessary
for reporting the correct line number if an error occurs.

> texparse                      :: LineNo -> String -> [Numbered Class]
> texparse n                    =  number n . compress . classify0 ""

To be able to catch errors the maximum length of arguments is
restricted to |maxChar| for commands and to |maxLine| for
environments.

> maxChar, maxLine              :: Int
> maxChar                       =  1000
> maxLine                       =  80 * 500

A simple Pseudo-\TeX-Parser. \NB Pseudo-\TeX\ environments must not be
nested:
\[
     @\begin{code}...\begin{code}...\end{code}...\end{code}@
\]
is not parsed properly.

|classify0| is only used at the start of a file or line; it recognizes
bird (and inverse bird) tracks.

> classify0                     :: String -> String -> [Class]
> classify0 _ []                =  []
> classify0 n ('>' : s)         =  Environment Code (n ++ ' ' : t) : classify0 "" u
>     where (t, u)              =  unbird '>' s
> classify0 n ('<' : s)         =  Environment Spec (n ++ ' ' : t) : classify0 "" u
>     where (t, u)              =  unbird '<' s
> classify0 n s                 =  Many n : classify s

\NB The preceding newline (if any) is put into the code section to be
able to suppress blank lines in the \LaTeX\ text.

> classify                      :: String -> [Class]
> classify []                   =  []
> classify ('\n' : s)           =  classify0 "\n" s

Commands disguised as comments (AKA pseudo-comments).
ks, 19.08.2004: changed |classify v| to |classify0 v| calls, to recognize
(incorrect-Haskell) bird tracks directly after a directive.

> classify ('%' : s)            =  case encode t of
>         Nothing               -> Many ('%' : t ++ arg)  :  classify0 "" v
>         Just cmd              -> Directive cmd arg      :  classify0 "" v
>     where (t, u)              =  break isSpace s
>           (arg, v)            =  breakAfter (== '\n') u

\NB Text starting with @%@ is ignored; in most cases this is what
you want (exception @\%@).

Environments.

> classify str@('\\' : s)       =  case span isIdChar s of
>     ("begin", '{' : t)        -> case span isIdChar t of
>         (env, '}' : u)        -> case encode env of
>             Nothing           -> cont
>             Just cmd
>                 | pred v      -> Environment cmd (arg ++ w) : classify x
>                 | otherwise   -> notFound end str :  cont
>                 where
>                 end           =  "\\end{" ++ env ++ "}"
>                 pred          =  isPrefix end
>                 (arg, v)      =  breaks maxLine pred u
>                 (w, x)        =  blank (drop (length end) v)
>         _                     -> cont

Inline verbatim commands are treated specially; otherwise @\verb|a|@
would be mistaken as inline code. Furthermore: then we are able to
write @\verb|\begin{code}|@.

>     ("verb*", c : t)          -> verbatim True c t
>     ("verb",  c : t)          -> verbatim False  c t

Commands.

>     (cmd, '{' : t)            -> case encode cmd of
>         Nothing               -> cont
>         Just cmd              -> case nested maxChar 0 t of
>             (a, '}' : u)      -> Command cmd a : classify u
>             _                 -> notFound "matching `}'" str : cont
>     _                         -> cont
>     where
>     cont                      =  One '\\' : classify s
>     verbatim b c t            =  case verb maxChar c t of
>         (u, c' : v) | c == c' -> Command (Vrb b) u : classify v
>         _                     -> notFound ("matching `" ++ [c] ++ "'") str : cont

Inline code.

> classify ('|' : '|' : s)      =  One '|' : classify s
> classify str@('|' : s)        =  case inline maxChar s of
>     (arg, '|' : t)            -> Inline arg : classify t
>     _                         -> notFound "matching `|'" str : One '|' : classify s

Short verb.

> classify ('@' : '@' : s)      =  One '@' : classify s
> classify str@('@' : s)        =  case shortverb maxChar s of
>     (arg, '@' : t)            -> Command (Vrb False) arg : classify t
>     _                         -> notFound "matching `@'" str : One '@' : classify s

Everything else.

> classify (c : s)              =  One c : classify s

> notFound                      :: String -> String -> Class
> notFound what s               =  Error (what ++ " not found", s)

> isIdChar                      :: Char -> Bool
> isIdChar c                    =  isAlpha c || c == '*'

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Parsing of arguments}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The parser satisfy
%
\begin{eqnarray*}
    |parse M.s = (M.l, M.r)|      & |==>| & |M.s = M.l ++ M.r|
\end{eqnarray*}
%
The function |nested n 0| recognizes arguments enclosed in matching
curly braces.

> nested                        :: Int -> Int -> String -> (String, String)
> nested n depth s              =  nest n s
>     where
>     nest 0 s                  =  ([], s)
>     nest n []                 =  ([], [])
>     nest n  ('}' : s)
>         | depth == 0          =  ([], '}' : s)
>         | otherwise           =  '}' <| nested (n - 1) (depth - 1) s
>     nest n ('{' : s)          =  '{' <| nested (n - 1) (depth + 1) s
>     nest n ('\\' : c : s)     =  '\\' <| c <| nest (n - 2) s
>     nest n (c : s)            =  c <| nest (n - 1) s

The function |verb n c| recognizes arguments enclosed in |c|.

> verb                          :: Int -> Char -> String -> (String, String)
> verb 0 c s                    =  ([], s)
> verb n c []                   =  ([], [])
> verb n c (c' : s)
>     | c == c'                 =  ([], c' : s) 
>     | otherwise               =  c' <| verb (n - 1) c s

The function |inline n| recognizes arguments enclosed in vertical bars
(and converts double bars into single bars; therefore it is \emph{not}
equivalent to |verb n '||'|).

> inline                        :: Int -> String -> (String, String)
> inline 0 s                    =  ([], s)
> inline n []                   =  ([], [])
> inline n ('|' : '|' : s)      =  '|' <| inline (n - 2) s
> inline n ('|' : s)            =  ([], '|' : s) 
> inline n (c : s)              =  c <| inline (n - 1) s
>
> shortverb                     :: Int -> String -> (String, String)
> shortverb 0 s                 =  ([], s)
> shortverb n []                =  ([], [])
> shortverb n ('@' : '@' : s)   =  '@' <| shortverb (n - 2) s
> shortverb n ('@' : s)         =  ([], '@' : s) 
> shortverb n (c : s)           =  c <| shortverb (n - 1) s

The function |unbird| recognizes code sections marked by bird tracks;
|blank| skips the next line if it is blank.

> unbird                        :: Char -> String -> (String, String)
> unbird c []                   =  ([], [])
> unbird c ('\n' : c' : s)
>     | c == c'                 =  '\n' <| ' ' <| unbird c s
> unbird c ('\n' : s)           =  '\n' <| blank s
> unbird c (c' : s)             =  c' <| unbird c s
>
> blank                         :: String -> (String, String)
> blank s | all isSpace t       =  (t, u)
>         | otherwise           =  ("", s)
>         where (t, u)          =  breakAfter (== '\n') s

|breaks n pred as| returns |(x, y)| such that |as = x ++ y|, |pred y|
holds and |x| is as small as possible (but at most of length |n|).

> breaks                        :: Int -> ([a] -> Bool) -> [a] -> ([a], [a])
> breaks n pred []              =  ([], [])
> breaks n pred as@(a : as')
>     | n == 0 || pred as       =  ([], as)
>     | otherwise               =  a <| breaks (n - 1) pred as'

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Post processing}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Collaps adjacent |One|'s into a |Many|.

> compress                      =  foldr (<|) []
>     where
>     One '\n' <| ts            =  Many "\n" : ts
>     Many s@('\n' : _) <| ts   =  Many s : ts
>     One c <| (Many s : ts)    =  Many (c : s) : ts
>     One c <| ts               =  Many [c] : ts
>     Many s <| (Many s' : ts)  =  Many (s ++ s') : ts
>     t <| ts                   =  t : ts

\NB The first two equations make |compress| incrementel (?); otherwise
\[
    |do s <- readFile "Examples/InfI.lhs"; mapM_ print (compress (map One s))|
\]
is silent until the complete input has been digested.

Adding line numbers.

> number                        :: LineNo -> [Class] -> [Numbered Class]
> number n []                   =  []
> number n (t : ts)             =  No n t : number (n + i) ts
>     where i                   =  case t of
>             One c             -> impossible "number"
>             Many s            -> newlines s
>             Inline s          -> newlines s
>             Command _ s       -> newlines s
>             Environment _ s   -> newlines s
>             Directive _ s     -> newlines s
>             Error _           -> 0

Number of newline characters in a string.

> newlines                      :: String -> Int
> newlines s                    =  length [ c | c <- s, c == '\n' ]
