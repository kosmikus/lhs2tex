%-------------------------------=  --------------------------------------------
\subsection{Document type}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Document ( module Document )
> where

%endif

> infixr 5 {-"\enskip"-} <>  -- same fixity as `|++|'

The pretty printer generate documents of type |Doc|.

> data Doc			=  Empty
>				|  Text String
>				|  Doc :^: Doc
>				|  Embedded String
>				|  Sub String [Doc]
>				   deriving (Eq, Show)

|Embedded| is used for embedded pseudo \TeX\ text (eg in comments);
|Sub s ds| is used for replacements (eg |Sub "inline" [..]|).

> (<>)				:: Doc -> Doc -> Doc
> Empty <> d			=  d
> d <> Empty			=  d
> d1 <> d2			=  d1 :^: d2
>
> catenate			:: [Doc] -> Doc
> catenate			=  foldr (<>) Empty

Substitution strings.

> sub'thin			=  Sub "thinspace" []
> sub'space			=  Sub "space" []
> sub'nl			=  Sub "newline" []
> sub'verbnl			=  Sub "verbnl" []
> sub'blankline			=  Sub "blankline" []
> sub'dummy			=  Sub "dummy" []
>
> sub'spaces a			=  Sub "spaces" [a]
> sub'special a			=  Sub "special" [a]
> sub'verb a			=  Sub "verb" [a]
> sub'verbatim a		=  Sub "verbatim" [a]
> sub'inline a			=  Sub "inline" [a]
> sub'code a			=  Sub "code" [a]
> sub'conid a			=  Sub "conid" [a]
> sub'varid a			=  Sub "varid" [a]
> sub'consym a			=  Sub "consym" [a]
> sub'varsym a			=  Sub "varsym" [a]
> sub'numeral a			=  Sub "numeral" [a]
> sub'char a			=  Sub "char" [a]
> sub'string a			=  Sub "string" [a]
> sub'comment a			=  Sub "comment" [a]
> sub'nested a			=  Sub "nested" [a]
> sub'pragma a                  =  Sub "pragma" [a]
> sub'keyword a			=  Sub "keyword" [a]
> sub'column1 a			=  Sub "column1" [a]
> sub'hskip a			=  Sub "hskip" [a]
> sub'phantom a			=  Sub "phantom" [a]
>
> sub'column3 a1 a2 a3		=  Sub "column3" [a1, a2, a3]

Additional substitutions for the new @poly@ formatter.
Added by ks, 14.05.2003.

> sub'fromto b e a              =  Sub "fromto" [Text b,Text e,a]
> sub'column n a                =  Sub "column" [Text n,a]
> sub'centered                  =  Sub "centered" []
> sub'left                      =  Sub "left" []
> sub'dummycol                  =  Sub "dummycol" []
> sub'indent n                  =  Sub "indent" [n]
