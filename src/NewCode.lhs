%-------------------------------=  --------------------------------------------
\subsection{New code formatter}
%-------------------------------=  --------------------------------------------

This is a more sophisticated code formatter that respects formatting
directives.

It should even respect formatting directives with arguments, in a
way that is compatible with the @poly@ or @math@ formatters.

%if codeOnly || showModuleHeader

> module NewCode                (  module NewCode  )
> where
>
> import Control.Arrow          (  (>>>) )
> import Control.Monad          (  (>=>) )
> import Data.List              (  partition )
>
> import Verbatim               (  trim, expand )
> import Document
> import Directives
> import HsLexer
> import qualified FiniteMap as FM
> import Auxiliaries
> import MathPoly               (  exprParse, substitute, number )
> import TeXCommands            (  Lang(..) )

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\NB We do not need an |inline| function because we are only interested
in the ``real'' program code. All comments are deleted.

> display                       :: Lang -> Formats -> String -> Either Exc Doc
> display lang fmts             =   lift trim
>                               >=> lift (expand 0)
>                               >=> tokenize lang
>                               >=> lift (number 1 1)
>                               >=> lift (partition (\t -> catCode t /= White))
>                               >=> exprParse *** return
>                               >=> lift (substitute fmts False) *** return
>                               >=> lift (uncurry merge)
>                               >=> lift (fmap token)
>                               >=> lift (latexs sub'space sub'nl fmts)
>                               >=> lift sub'code

ks, 2016-08-12:
Since we're now reusing this for markdown mode, we need an |inline| function
after all:

> inline                        :: Lang -> Formats -> String -> Either Exc Doc
> inline lang fmts              =   fmap unNL
>                               >>> tokenize lang
>                               >=> lift (number 1 1)
>                               >=> lift (partition (\t -> catCode t /= White))
>                               >=> exprParse *** return
>                               >=> lift (substitute fmts False) *** return
>                               >=> lift (uncurry merge)
>                               >=> lift (fmap token)
>                               >=> lift (latexs sub'space sub'nl fmts)
>                               >=> lift sub'inline

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Encoding}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, added 10.01.2004:
This is based on |latexs| in Typewriter, and therefore still named
this way, but it is a bit simpler and does not use anything \LaTeX ish:
the |latexs| and |latex| functions itself are copied literally, but
|convert| does not do anything except replacing newlines and spaces,
if specified by an appropriate @%subst@. It's questionable whether this
functionality is actually desired.

> latexs                        :: Doc -> Doc -> Formats -> [Token] -> Doc
> latexs sp nl dict             =  catenate . map (latex sp nl dict)
>
> latex                         :: Doc -> Doc -> Formats -> Token -> Doc
> latex sp nl dict              =  tex Empty
>     where
>     tex _ (Space s)           =  sub'spaces (convert s)
>     tex q (Conid s)           =  replace q s (sub'conid (q <<>> convert s))
>     tex _ (Varid "")          =  sub'dummy    -- HACK
>     tex q (Varid s)           =  replace q s (sub'varid (q <<>> convert s))
>     tex q (Consym s)          =  replace q s (sub'consym (q <<>> convert s))
>     tex q (Varsym s)          =  replace q s (sub'varsym (q <<>> convert s))
>     tex _ (Numeral s)         =  replace Empty s (sub'numeral (convert s)) -- NEU
>     tex _ (Char s)            =  sub'char (catenate (map conv (init $ tail s))) -- NEW: remove quotes
>     tex _ (String s)          =  sub'string (catenate (map conv (init $ tail s))) -- NEW: remove quotes
>     tex _ (Special c)         =  sub'special (replace Empty [c] (conv c))
>     tex _ (Comment s)         =  sub'comment (convert s)
>     tex _ (Nested s)          =  sub'nested (convert s)
>     tex _ (Pragma s)          =  sub'pragma (convert s)
>     tex _ (Keyword s)         =  replace Empty s (sub'keyword (convert s))
>     tex _ (TeX False d)       =  d
>     tex _ (TeX True d)        =  sub'tex d
>     tex _ t@(Qual ms t')      =  replace Empty (string t) (tex (catenate (map (\m -> tex Empty (Conid m) <<>> Text ".") ms)) t')
>     tex _ t@(Op t')           =  replace Empty (string t) (sub'backquoted (tex Empty t'))
>
>     replace q s def           =  case FM.lookup s dict of
>         Just (_, _, [], ts)   -> q <<>> catenate (map (tex Empty) ts)
>         _                     -> def

\NB the directives @%format a = b@ and @%format b = a@ cause a loop.

\NB Only nullary macros are applied.

Conversion of strings and characters.

>     convert                   :: String -> Doc
>     convert s                 =  catenate (map conv s)
>     conv                      :: Char -> Doc
>     conv ' '                  =  sp
>     conv '\n'                 =  nl
>     conv c                    =  Text [c]

