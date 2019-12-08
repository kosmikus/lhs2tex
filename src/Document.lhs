%-------------------------------=  --------------------------------------------
\subsection{Document type}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Document ( module Document )
> where
>

%endif

> infixr 5 {-"\enskip"-} <<>>  -- same fixity as `|++|'

The pretty printer generate documents of type |Doc|.

> data Doc                      =  Empty
>                               |  Text String
>                               |  Doc :^: Doc
>                               |  Embedded String
>                               |  Sub String [Doc]
>                                  deriving (Eq, Show)

|Embedded| is used for embedded pseudo \TeX\ text (eg in comments);
|Sub s ds| is used for replacements (eg |Sub "inline" [..]|).

> (<<>>)                        :: Doc -> Doc -> Doc
> Empty <<>> d                  =  d
> d <<>> Empty                  =  d
> d1 <<>> d2                    =  d1 :^: d2
>
> catenate                      :: [Doc] -> Doc
> catenate                      =  foldr (<<>>) Empty

Substitution strings.

> sub'thin                      :: Doc
> sub'thin                      =  Sub "thinspace" []
> sub'space                     :: Doc
> sub'space                     =  Sub "space" []
> sub'nl                        :: Doc
> sub'nl                        =  Sub "newline" []
> sub'verbnl                    :: Doc
> sub'verbnl                    =  Sub "verbnl" []
> sub'blankline                 :: Doc
> sub'blankline                 =  Sub "blankline" []
> sub'dummy                     :: Doc
> sub'dummy                     =  Sub "dummy" []
>
> sub'spaces                    :: Doc -> Doc
> sub'spaces a                  =  Sub "spaces" [a]
> sub'special                   :: Doc -> Doc
> sub'special a                 =  Sub "special" [a]
> sub'verb                      :: Doc -> Doc
> sub'verb a                    =  Sub "verb" [a]
> sub'verbatim                  :: Doc -> Doc
> sub'verbatim a                =  Sub "verbatim" [a]
> sub'inline                    :: Doc -> Doc
> sub'inline a                  =  Sub "inline" [a]
> sub'code                      :: Doc -> Doc
> sub'code a                    =  Sub "code" [a]
> sub'conid                     :: Doc -> Doc
> sub'conid a                   =  Sub "conid" [a]
> sub'varid                     :: Doc -> Doc
> sub'varid a                   =  Sub "varid" [a]
> sub'consym                    :: Doc -> Doc
> sub'consym a                  =  Sub "consym" [a]
> sub'varsym                    :: Doc -> Doc
> sub'varsym a                  =  Sub "varsym" [a]
> sub'backquoted                :: Doc -> Doc
> sub'backquoted a              =  Sub "backquoted" [a]
> sub'numeral                   :: Doc -> Doc
> sub'numeral a                 =  Sub "numeral" [a]
> sub'char                      :: Doc -> Doc
> sub'char a                    =  Sub "char" [a]
> sub'string                    :: Doc -> Doc
> sub'string a                  =  Sub "string" [a]
> sub'comment                   :: Doc -> Doc
> sub'comment a                 =  Sub "comment" [a]
> sub'nested                    :: Doc -> Doc
> sub'nested a                  =  Sub "nested" [a]
> sub'pragma                    :: Doc -> Doc
> sub'pragma a                  =  Sub "pragma" [a]
> sub'tex                       :: Doc -> Doc
> sub'tex a                     =  Sub "tex" [a]
> sub'keyword                   :: Doc -> Doc
> sub'keyword a                 =  Sub "keyword" [a]
> sub'column1                   :: Doc -> Doc
> sub'column1 a                 =  Sub "column1" [a]
> sub'hskip                     :: Doc -> Doc
> sub'hskip a                   =  Sub "hskip" [a]
> sub'phantom                   :: Doc -> Doc
> sub'phantom a                 =  Sub "phantom" [a]
>
> sub'column3                   :: Doc -> Doc -> Doc -> Doc
> sub'column3 a1 a2 a3          =  Sub "column3" [a1, a2, a3]

Additional substitutions for the new @poly@ formatter.
Added by ks, 14.05.2003.

> sub'fromto                    :: String -> String -> Doc -> Doc
> sub'fromto b e a              =  Sub "fromto" [Text b,Text e,a]
> sub'column                    :: String -> Doc -> Doc
> sub'column n a                =  Sub "column" [Text n,a]
> sub'centered                  :: Doc
> sub'centered                  =  Sub "centered" []
> sub'left                      :: Doc
> sub'left                      =  Sub "left" []
> sub'dummycol                  :: Doc
> sub'dummycol                  =  Sub "dummycol" []
> sub'indent                    :: Doc -> Doc
> sub'indent n                  =  Sub "indent" [n]
