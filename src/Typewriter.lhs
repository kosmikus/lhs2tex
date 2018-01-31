%-------------------------------=  --------------------------------------------
\subsection{Typewriter formatter}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Typewriter             (  module Typewriter  )
> where
>
> import Control.Monad
>
> import Verbatim ( trim, expand )
> import Document
> import Directives
> import HsLexer
> import qualified FiniteMap as FM
> import Auxiliaries
> import TeXCommands ( Lang (..) )

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Inline and display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> inline, display               :: Lang -> Formats -> String -> Either Exc Doc
> inline lang dict              =   tokenize lang
>                               >=> lift (latexs sub'thin sub'thin dict)
>                               >=> lift sub'inline

> display lang dict             =   lift trim
>                               >=> lift (expand 0)
>                               >=> tokenize lang
>                               >=> lift (latexs sub'space sub'nl dict)
>                               >=> lift sub'code

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{\LaTeX\ encoding}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> latexs                        :: Doc -> Doc -> Formats -> [Token] -> Doc
> latexs sp nl dict             =  catenate . map (latex sp nl dict)
>
> latex                         :: Doc -> Doc -> Formats -> Token -> Doc
> latex sp nl dict              =  tex Empty
>     where
>     tex _ (Space s)           =  sub'spaces (convert False s)
>     tex q (Conid s)           =  replace q s (sub'conid (q <> convert False s))
>     tex _ (Varid "")          =  sub'dummy    -- HACK
>     tex q (Varid s)           =  replace q s (sub'varid (q <> convert False s))
>     tex q (Tyvarid s)         =  replace q s (sub'tyvarid (q <> convert False s))
>     tex q (Consym s)          =  replace q s (sub'consym (q <> convert False s))
>     tex q (Varsym s)          =  replace q s (sub'varsym (q <> convert False s))
>     tex _ (Numeral s)         =  replace Empty s (sub'numeral (convert True s)) -- NEU
>     tex _ (Char s)            =  sub'char (catenate (map conv' (init $ tail s))) -- NEW: remove quotes
>     tex _ (String s)          =  sub'string (catenate (map conv' (init $ tail s))) -- NEW: remove quotes
>     tex _ (Special c)         =  sub'special (replace Empty [c] (conv False c))
>     tex _ (Comment s)         =  sub'comment (Embedded s)
>     tex _ (Nested s)          =  sub'nested (Embedded s)
>     tex _ (Pragma s)          =  sub'pragma (Embedded s)
>     tex _ (Keyword s)         =  replace Empty s (sub'keyword (convert False s))
>     tex _ (TeX False d)       =  d
>     tex _ (TeX True d)        =  sub'tex d
>     tex _ t@(Qual ms t')      =  replace Empty (string t) (tex (catenate (map (\m -> tex Empty (Conid m) <> Text ".") ms)) t')
>     tex _ t@(Op t')           =  replace Empty (string t) (sub'backquoted (tex Empty t'))
>         where cmd | isConid t'=  sub'consym
>                   | otherwise =  sub'varsym
>
>     replace q s def           =  case FM.lookup s dict of
>         Just (_, _, [], ts)   -> q <> catenate (map (tex Empty) ts)
>         _                     -> def

\NB the directives @%format a = b@ and @%format b = a@ cause a loop.
 
\NB Only nullary macros are applied.

Conversion of strings and characters. The Boolean indicates whether we
want to convert a numeric literal which could contain an exponent.

>     convert                   :: Bool -> String -> Doc
>     convert isNum s           =  catenate (map (conv isNum) s)
>     conv                      :: Bool -> Char -> Doc
>     conv _ ' '                =  sp
>     conv _ '\n'               =  nl
>     conv isNum c
>       | c `elem` "#$%&"       =  Text ("\\" ++ [c])
>       | c `elem` "\"\\^_{}~"  =  Text (char c)
>       | isNum && c `elem` "-+"=  Text ("{" ++ [c] ++ "}")
>       | otherwise             =  Text [c]
>
>     conv' ' '                 =  Text "~" -- NEW: instead of |Text (char ' ')| -- for character and string literals
>     conv' c                   =  conv False c

\NB The character @"@ is not copied verbatim, to be able to use
@german.sty@ (@"@ is made active).

\NB The coding of characters is not independent of the \TeX\ font used,
eg @\/@ appears different in italics and typewriter (@\@ is
\texttt{\char'134} in typewriter, but \textit{\char'134} in italics).

> char                          :: Char -> String
> char c                        =  "\\char" ++ show (fromEnum c) ++ " "
