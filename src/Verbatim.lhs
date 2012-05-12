%-------------------------------=  --------------------------------------------
\subsection{Verbatim formatter}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Verbatim               (  module Verbatim  )
> where
>
> import Data.Char
> import Data.List (intersperse)
>
> import Document
> import Auxiliaries

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Inline and display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The Boolean flag indicates whether a space should be typeset as \verb*| | (|True|) or not.

> inline                        :: Bool -> String -> Doc
> inline b                      =  latexs b .> sub'verb
>
> display                       :: Int -> Bool -> String -> Doc
> display width b               =  trim
>                               .> expand 0
>                               .> lines
>                               .> map (group width)
>                               .> map (map (latexs b))
>                               .> map splice
>                               .> intersperse sub'verbnl
>                               .> catenate
>                               .> sub'verbatim
>
> splice                        :: [Doc] -> Doc
> splice ds                     =  Text "~" <> catenate (intersperse nl ds)
>     where nl                  =  Text "!" <> sub'verbnl <> Text "!"
>
> latexs                        :: Bool -> String -> Doc
> latexs b                      =  catenate . map latex
>     where
>     latex ' '
>         | b                   =  Text "\\char32 "
>     latex c
>         | c `elem` " \t\n"    =  Text "~"
>         | isAlphaNum c        =  Text [c]
>         | otherwise           =  Text ("\\char" ++ show (fromEnum c) ++ "{}")

ks, 11.01.2005: I've added {} after @\char@ to prevent ligatures like
@--@ from applying.

\NB Comments are \emph{not} typeset in \TeX, hence the name of the
style. This is really a feature since the enclosed code need not be
Haskell code.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Deleting blank lines and expanding tabs}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Delete leading and trailing blank line(s).

> trim                          :: String -> String
> trim                          =  skip .> reverse .> skip .> reverse
>
> skip                          :: String -> String
> skip ""                       =  ""
> skip s | all isSpace t        =  skip u
>        | otherwise            =  s
>        where (t, u)           =  breakAfter (== '\n') s

Expanding tabs (assuming a tabulator width of $8$ characters).

> expand                        :: Int -> String -> String
> expand n []                   =  []
> expand n ('\n' : s)           =  '\n' : expand 0 s
> expand n ('\t' : s)           =  replicate (n' - n) ' ' ++ expand n' s
>     where n'                  =  (n + 8) `div` 8 * 8
> expand n (c : s)              =  c : expand (n + 1) s

