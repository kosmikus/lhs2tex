%-------------------------------=  --------------------------------------------
\subsection{New code formatter}
%-------------------------------=  --------------------------------------------

This is a more sophisticated code formatter that respects formatting
directives.

It should even respect formatting directives with arguments, in a
way that is compatible with the @poly@ or @math@ formatters.

%if codeOnly || showModuleHeader

> module NewCode		(  module NewCode  )
> where
>
> import Char
>
> import Verbatim ( trim, expand )
> import Document
> import Directives
> import HsLexer
> import qualified FiniteMap as FM
> import List ( partition )
> import Auxiliaries
> import MathPoly ( exprParse, substitute, number )
> import Typewriter ( latexs )

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\NB We do not need an |inline| function because we are only interested
in the ``real'' program code. All comments are deleted.

> display		        :: Formats -> String -> Either Exc Doc
> display fmts			=  lift trim
>				@> lift (expand 0)
>				@> tokenize
>                               @> lift (number 1 1)
>                               @> lift (partition (\t -> catCode t /= White))
>                               @> exprParse *** return
>                               @> lift (substitute fmts False) *** return
>                               @> lift (uncurry merge)
>                               @> lift (fmap token)
>				@> lift (latexs sub'space sub'nl fmts)
>				@> lift sub'code

