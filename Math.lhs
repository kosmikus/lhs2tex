%-------------------------------=  --------------------------------------------
\subsection{Math formatter}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Math			(  module Math  )
> where
>
> import Prelude hiding ( lines )
> import List ( partition )
> import Int ( fromInt )
> import Numeric ( showFFloat )
> import Monad ( MonadPlus(..) )
>
> import Verbatim ( expand, trim )
> import Typewriter ( latex )
> import Document
> import Directives
> import HsLexer
> import Parser
> import qualified FiniteMap as FM
> import Auxiliaries

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Inline and display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> inline			:: Formats -> Bool -> String -> Either Exc Doc
> inline fmts auto		=  fmap unNL
>				.> tokenize
>				@> lift (number 1 1)
>				@> when auto (lift (filter (isNotSpace . token)))
>				@> lift (partition (\t -> catCode t /= White))
>				@> exprParse *** return
>				@> lift (substitute fmts auto) *** return
>				@> lift (uncurry merge)
>				@> lift (fmap token)
>				@> when auto (lift addSpaces)
>				@> lift (latexs fmts)
>				@> lift sub'inline

> display			:: Formats -> Bool -> (Stack, Stack) -> Maybe Int
>				-> String -> Either Exc (Doc, (Stack,Stack))
> display fmts auto sts col	=  lift trim
>				@> lift (expand 0)
>				@> tokenize
>				@> lift (number 1 1)
>				@> when auto (lift (filter (isNotSpace . token)))
>				@> lift (partition (\t -> catCode t /= White))
>				@> exprParse *** return
>				@> lift (substitute fmts auto) *** return
>				@> lift (uncurry merge)
>				@> lift lines
>				@> lift (align col)
>				@> when auto (lift (fmap (fmap addSpaces)))
>				@> lift (leftIndent fmts auto sts)
>				@> lift sub'code *** return

> when True f			=  f
> when False f			=  return

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding positional information}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Row			=  Int
> type Col			=  Int
>
> data Pos a			=  Pos {row :: !Row, col :: !Col, ann :: a}
>				   deriving (Show)

%{
%format r1
%format r2
%format c1
%format c2

> instance Eq (Pos a) where
>     Pos r1 c1 _ == Pos r2 c2 _=  r1 == r2 && c1 == c2
> instance Ord (Pos a) where
>     Pos r1 c1 _ <= Pos r2 c2 _=  (r1, c1) <= (r2, c2)

%}

> instance (CToken tok) => CToken (Pos tok) where
>     catCode (Pos _ _ t)	=  catCode t
>     token (Pos _ _ t)		=  token t
>     inherit (Pos r c t') t	=  Pos r c (inherit t' t)
>     fromToken t		=  Pos 0 0 (fromToken t)

Tokenliste durchnumerieren.

> number			:: Row -> Col -> [Token] -> [Pos Token]
> number r c []			=  []
> number r c (t : ts)		=  Pos r c t : number r' c' ts
>     where (r', c')		=  count r c (string t)
>
> count				:: Row -> Col -> String -> (Row, Col)
> count r c []			=  (r, c)
> count r c (a : s)
>     | a == '\n'		=  count (r + 1) 1       s
>     | otherwise		=  count r       (c + 1) s

Tokenliste in Zeilen auftrennen.

> lines				:: [Pos a] -> [[Pos a]]
> lines				=  split 1
>     where
>     split _   []		=  []
>     split r ts		=  us : split (r + 1) vs
>         where (us, vs)	=  span (\t -> row t <= r) ts

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A very simple Haskell Parser}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Chunk a			=  [Item a]
>
> data Item a			=  Delim a
>				|  Apply [Atom a]
>				   deriving (Show)
>
> data Atom a			=  Atom a
>				|  Paren a (Chunk a) a
>				   deriving (Show)

The parser is based on the Smugweb parser.

> exprParse			:: (CToken tok, Show tok) => [Pos tok] -> Either Exc [Item (Pos tok)]
> exprParse s			=  case run chunk s of
>     Nothing			-> Left ("syntax error", show s) -- HACK: |show s|
>     Just e			-> Right e
>
> chunk				:: (CToken tok) => Parser (Pos tok) (Chunk (Pos tok))
> chunk				=  do a <- many atom
>				      as <- many (do s <- sep; a <- many atom; return ([Delim s] ++ offside a))
>				      return (offside a ++ concat as)
>     where offside []		=  []
>           -- old: |opt a =  [Apply a]|
>           offside (a : as)	=  Apply (a : bs) : offside cs
>               where (bs, cs)	=  span (\a' -> col' a < col' a') as
>           col' (Atom a)	=  col a
>	    col' (Paren a _ _)	=  col a
>
> atom				:: (CToken tok) => Parser (Pos tok) (Atom (Pos tok))
> atom				=  fmap Atom noSep
>				`mplus` do l <- left
>				           e <- chunk
>				           r <- right l
>				           return (Paren l e r)

Primitive parser.

> sep, noSep, left		:: (CToken tok) => Parser tok tok
> sep				=  satisfy (\t -> catCode t == Sep)
> noSep				=  satisfy (\t -> catCode t == NoSep)
> left				=  satisfy (\t -> case catCode t of Del c -> c `elem` "(["; _-> False)
> right l			=  satisfy (\c -> case (catCode l, catCode c) of
>				       (Del o, Del c) -> (o,c) `elem` zip "([" ")]" 
>				       _     -> False)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Making replacements}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Mode			=  Mandatory
>				|  Optional Bool

If |eval e| returns |Mandatory| then parenthesis around |e| must not be
dropped; |Optional True| indicates that it can be dropped; |Optional
False| indicates that the decision is up the caller.

> substitute			:: (CToken tok) => Formats -> Bool -> [Item tok] -> [tok]
> substitute d auto chunk	=  snd (eval chunk)
>   where
>   eval [e]			=  eval' e
>   eval chunk			=  (Optional False, concat [ snd (eval' i) | i <- chunk ])
>
>   eval' (Delim s)		=  (Optional False, [s])
>   eval' (Apply [])		=  impossible "eval'"
>   eval' (Apply (e : es))	=  eval'' False e es
>
>   eval'' _ (Atom s) es	=  case FM.lookup (string (token s)) d of
>     Nothing			-> (Optional False, s : args es)
>     Just (opt, opts, lhs, rhs)-> (Optional opt, set s (concat (fmap sub rhs)) ++ args bs)
>         where
>         (as, bs) | m <= n	=  (es ++ replicate (n - m) dummy, [])
>                  | otherwise	=  splitAt n es
>         n			=  length lhs
>         m			=  length es
>         binds			=  zip lhs [ snd (eval'' b a []) | (b, a) <- zip opts as ]
>         sub t@(Varid x)	=  case FM.lookup x (FM.fromList binds) of
>             Nothing		-> [fromToken t]
>             Just ts		-> ts
>         sub t			=  [fromToken t]

Wenn ein Token ersetzt bzw.~entfernt wird, dann erbt das jeweils erste
Token des Ersetzungstexts dessen Position.

>   eval'' opt (Paren l e r) es
>       | optional		=  (Mandatory, set l s ++ args es)
>       | otherwise		=  (Optional False, [l] ++ s ++ [r] ++ args es)
>       where (flag, s)		=  eval e
>             optional		=  catCode l == Del '(' && not (mandatory e)
>				&& case flag of Mandatory -> False; Optional f -> opt || f

\NB Es ist keine gute Idee Klammern um Atome wegzulassen, dann werden
auch bei @deriving (Eq)@ und @module M (a)@ die Klammern entfernt.

>   args es			=  concat [ sp ++ snd (eval'' False i []) | i <- es ] -- $\cong$ Applikation
>   sp | auto			=  [fromToken (TeX sub'space)]
>      | otherwise		=  []

Um Makros der Form @%format Parser (a) = a@ besser zu unterst"utzen.

> set				:: (CToken tok) => tok -> [tok] -> [tok]
> set s []			=  []
> set s (t : ts)		=  inherit s (token t) : ts
>
> mandatory			:: (CToken tok) => Chunk tok -> Bool
> mandatory e			=  False

Code before:

< mandatory e			=  null e		-- nullary tuple
<				|| or [ isComma i | i <- e ] -- tuple
<				|| isOp (head e)	-- left section
<				|| isOp (last e)	-- right section

> isComma, isOp			:: (CToken tok) => Item tok -> Bool
> isComma (Delim t)		=  case token t of
>     Special c			-> c == ','
>     _				-> False
> isComma _			=  False
>
> isOp (Delim t)		=  case token t of
>     Special c			-> c == '`'	-- f"ur @` div `@
>     Consym _			-> True
>     Varsym s			-> s /= "\\"
>     Op _			-> True
>     _				-> False
> isOp _			=  False

> dummy				:: (CToken tok) => Atom tok
> dummy				=  Atom (fromToken (Varid ""))

\NB We cannot use embedded \TeX\ text here, because |TeX| is not a
legal atom (|string| is applied to it).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Internal alignment}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\Todo{Internal alignment Spalte automatisch bestimmen. Vorsicht: die
Position von |=| oder |::| heranzuziehen ist gef"ahrlich; wenn z.B.
|let x = e| in einem |do|-Ausdruck vorkommt.}

> data Line a			=  Blank
>				|  Three a a a
>				|  Multi a
>
> align				:: (CToken tok) => Maybe Int -> [[Pos tok]] -> [Line [Pos tok]]
> align c			=  fmap (maybe Multi split3 c)
>   where
>   split3 i ts			=  case span (\t -> col t < i) ts of
>       ([], [])		-> Blank
>       ((_ : _), [])		-> Multi ts
>       (us, v : vs)
>           | col v == i && isInternal v
>				-> Three us [v] vs
>           | null us		-> Three [] [] (v : vs)
>           | otherwise		-> Multi ts
>
>
> isInternal			:: (CToken tok) => tok -> Bool
> isInternal t			=  case token t of
>     Consym _			-> True
>     Varsym _			-> True
>     Special _			-> True
>     _				-> False
>
> instance Functor Line where
>     fmap f Blank		=  Blank
>     fmap f (Three l c r)	=  Three (f l) (f c) (f r)
>     fmap f (Multi a)		=  Multi (f a)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding spaces}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Inserting spaces before and after keywords. We use a simple finite
automata with three states: |before b| means before a keyword, |b|
indicates whether to insert a space or not; |after| means immediately
after a keyword (hence |before b| really means not immediately after).

> addSpaces			:: (CToken tok) => [tok] -> [tok]
> addSpaces ts			=  before False ts
>     where
>     before b []		=  []
>     before b (t : ts)		=  case token t of
>         u | selfSpacing u	-> t : before False ts
>         Special c
>           | c `elem` ",;([{"	-> t : before False ts
>         Keyword _		-> [ fromToken (TeX sub'space) | b ] ++ t : after ts
>         _			-> t : before True ts
> 
>     after []			=  []
>     after (t : ts)		=  case token t of
>         u | selfSpacing u	-> t : before False ts
>         Special c
>           | c `elem` ",;([{"	-> fromToken (TeX sub'space) : t : before False ts
>         Keyword _		-> fromToken (TeX sub'space) : t : after ts
>         _			-> fromToken (TeX sub'space) : t : before True ts

Operators are `self spacing'.

> selfSpacing			:: Token -> Bool
> selfSpacing (Consym _)	=  True
> selfSpacing (Varsym _)	=  True
> selfSpacing (Op _)		=  True
> -- |selfSpacing (TeX _) =  True|
> selfSpacing _			=  False

\NB It's not a good idea to regard inline \TeX\ as self spacing consider,
for example, a macro like @%format mu = "\mu "@.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Left indentation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Auch wenn |auto = False| wird der Stack auf dem laufenden gehalten.

> type Stack			=  [(Col, Doc, [Pos Token])]
>
> leftIndent dict auto (lst, rst)
>				=  loop lst rst
>   where
>   copy d | auto		=  d
>          | otherwise		=  Empty

Die Funktion |isInternal| pr"uft, ob |v| ein spezielles Symbol wie
@::@, @=@ etc~oder ein Operator wie @++@ ist.

>   loop lst rst []		=  (Empty, (lst, rst))
>   loop lst rst (l : ls)	=  case l of
>       Blank			-> loop lst rst ls
>       Three l c r		-> (sub'column3 (copy lskip <> latexs dict l)
>				                (latexs dict c)
>				                (copy rskip <> latexs dict r) <> sep ls <> rest, st')
>           where (lskip, lst')	=  indent l lst
>                 (rskip, rst') =  indent r rst
>                 (rest, st') 	=  loop lst' rst' ls -- does not work: |if null l && null c then rst' else []|
>       Multi m			-> (sub'column1 (copy lskip <> latexs dict m) <> sep ls <> rest, st')
>           where (lskip, lst')	=  indent m lst
>                 (rest, st')	=  loop lst' [] ls
>
>   sep []			=  Empty
>   sep (Blank : _ )		=  sub'blankline
>   sep (_ : _)			=  sub'nl
>
>   indent			:: [Pos Token] -> Stack -> (Doc, Stack)
>   indent [] stack		=  (Empty, stack)
>   indent ts@(t : _) []	=  (Empty, [(col t, Empty, ts)])
>   indent ts@(t : _) (top@(c, skip, line) : stack)
>				=  case compare (col t) c of
>       LT			-> indent ts stack
>       EQ			-> (skip, (c, skip, ts) : stack)
>       GT			-> (skip', (col t, skip', ts) : top : stack)
>           where
>           skip'		=  case span (\u -> col u < col t) line of
>               (us, v : vs) | col v == col t
>				-> skip <> sub'phantom (latexs dict us)
>               -- does not work: |(us, _) -> skip ++ [Phantom (fmap token us), Skip (col t - last (c : fmap col us))]|
>               _		-> skip <> sub'hskip (Text em)
>                   where em	=  showFFloat (Just 2) (0.5 * fromInt (col t - c) :: Double) ""

< fromInt			:: Num a => Int -> a
< fromInt i			=  fromInteger (toInteger i)

M"ussen |v| und |t| zueinander passen?
%
\begin{verbatim}
where |a      =    where |Str c =    [    [    (    {
      |(b, c) =          |c@(..)=    ,    |    ,    ;
                                     ]    ]    )    }
\end{verbatim}

F"ur inline-code.

> latexs			:: (CToken tok) => Formats -> [tok] -> Doc
> latexs dict			=  catenate . fmap (latex sub'space sub'space dict . token)
