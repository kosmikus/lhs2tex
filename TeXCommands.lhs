%-------------------------------=  --------------------------------------------
\subsection{Pseudo-\TeX\ Commands}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module TeXCommands		(  module TeXCommands  )
> where
>
> import Maybe
> import FiniteMap		(  FiniteMap  )
> import qualified FiniteMap as FM
> import Auxiliaries

%endif

> data Style			=  Version | Help | Copying | Warranty | CodeOnly | NewCode | Verb | Typewriter | Poly | Math
>				   deriving (Eq, Show, Enum, Bounded)

\Todo{Better name for |Class|.}

> data Class			=  One                     Char		-- ordinary text
>				|  Many                    String	-- ditto
>				|  Inline                  String	-- @|..|@
>				|  Command     Command     String	-- @\cmd{arg}@
>				|  Environment Environment String	-- @\begin{cmd}..arg\end{cmd}@
>				|  Directive   Directive   String	-- @%cmd arg@
>				|  Error       Exc			-- parsing error
>				   deriving (Show)

> data Command			=  Hs | Eval | Perform | Vrb Bool
>				   deriving (Eq, Show)
>
> data Environment		=  Haskell | Code | Spec | Evaluate | Hide | Ignore | Verbatim Bool
>				   deriving (Eq, Show)

\NB |Hs|, |Haskell|, |Hide|, and |Ignore| are obsolete.

>
> data Directive		=  Format | Include | Let | File | Options
>				|  Align | Separation | Latency | Begin | End | Subst
>				|  If | Elif | Else | Endif
>				   deriving (Eq, Show)

> data Numbered a		=  No !LineNo a
>				   deriving (Show)
>

\NB The |Show| instances have been defined for debugging purposes, the
|Eq| instances are necessary for |decode|.

> conditional			:: Directive -> Bool
> conditional If		=  True
> conditional Elif		=  True
> conditional Else		=  True
> conditional Endif		=  True
> conditional _			=  False

Encoding and decoding of commands, environments, and directives.
\Todo{Better name for |Representation|.}

> class Representation a where
>     representation		:: [(String, a)]
> instance Representation Style where
>     representation		=  [ ("tt", Typewriter), ("math", Math), ("poly", Poly),
>				     ("verb", Verb), ("code", CodeOnly), ("newcode",NewCode),
>                                    ("version", Version),
>                                    ("copying", Copying), ("warranty", Warranty), ("help", Help) ]
> instance Representation Command where
>     representation		=  [ ("hs", Hs), ("eval", Eval),
>				     ("perform", Perform), ("verb*", Vrb True),
>				     ("verb", Vrb False) ]
> instance Representation Environment where
>     representation		=  [ ("haskell", Haskell), ("code", Code),
>				     ("spec", Spec), ("evaluate", Evaluate), ("hide", Hide),
>				     ("ignore", Ignore), ("verbatim*", Verbatim True),
>				     ("verbatim", Verbatim False) ]
> instance Representation Directive where
>     representation		=  [ ("format", Format), ("include", Include),
>				     ("if", If), ("elif", Elif),
>				     ("else", Else), ("endif", Endif),
>				     ("let", Let), ("file", File),
>				     ("options", Options), ("align", Align),
>                                    ("separation", Separation), ("latency", Latency),
>				     ("{", Begin), ("}", End), ("subst", Subst) ]
>
> encode			:: (Representation a) => String -> Maybe a
> encode s			=  FM.lookup s (FM.fromList representation)
>
> decode			:: (Eq a, Representation a) => a -> String
> decode a			=  fromJust (lookup a (inverse representation))

\NB We cannot use arrays for |decode|, because |Command| is neither an
enumerated nor a product type (|Vrb Bool|).
