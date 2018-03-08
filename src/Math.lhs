%-------------------------------=  --------------------------------------------
\subsection{Math formatter}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Math                   (  module Math, substitute, number  )
> where
>
> import Prelude hiding         (  lines, (<>) )
> import Data.List              (  partition )
> import Numeric                (  showFFloat )
> import Control.Applicative    (  many )
> import Control.Arrow          (  (>>>) )
> import Control.Monad          (  MonadPlus(..), (>=>) )
>
> import Verbatim               (  expand, trim )
> import Typewriter             (  latex )
> import MathCommon
> import Document
> import Directives
> import HsLexer
> import Parser
> import qualified FiniteMap as FM
> import Auxiliaries
> import TeXCommands ( Lang(..) )

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Inline and display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> inline                        :: Lang -> Formats -> Bool -> String -> Either Exc Doc
> inline lang fmts auto         =   fmap unNL
>                               >>> tokenize lang
>                               >=> lift (number 1 1)
>                               >=> when auto (lift (filter (isNotSpace . token)))
>                               >=> lift (partition (\t -> catCode t /= White))
>                               >=> exprParse *** return
>                               >=> lift (substitute fmts auto) *** return
>                               >=> lift (uncurry merge)
>                               >=> lift (fmap token)
>                               >=> when auto (lift addSpaces)
>                               >=> lift (latexs fmts)
>                               >=> lift sub'inline

> display                       :: Lang -> Formats -> Bool -> (Stack, Stack) -> Maybe Int
>                               -> String -> Either Exc (Doc, (Stack,Stack))
> display lang fmts auto sts col=   lift trim
>                               >=> lift (expand 0)
>                               >=> tokenize lang
>                               >=> lift (number 1 1)
>                               >=> when auto (lift (filter (isNotSpace . token)))
>                               >=> lift (partition (\t -> catCode t /= White))
>                               >=> exprParse *** return
>                               >=> lift (substitute fmts auto) *** return
>                               >=> lift (uncurry merge)
>                               >=> lift lines
>                               >=> lift (align col)
>                               >=> when auto (lift (fmap (fmap addSpaces)))
>                               >=> lift (leftIndent fmts auto sts)
>                               >=> lift sub'code *** return

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A very simple Haskell Parser}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The parser is based on the Smugweb parser.
This variant cannot handle unbalanced parentheses.

> exprParse                     :: (CToken tok, Show tok) => [Pos tok] -> Either Exc [Item (Pos tok)]
> exprParse s                   =  case run chunk s of
>     Nothing                   -> Left ("syntax error", show s) -- HACK: |show s|
>     Just e                    -> Right e
>
> chunk                         :: (CToken tok) => Parser (Pos tok) (Chunk (Pos tok))
> chunk                         =  do a <- many atom
>                                     as <- many (do s <- sep; a <- many atom; return (Delim s : offside a))
>                                     return (offside a ++ concat as)
>     where offside []          =  []
>           -- old: |opt a =  [Apply a]|
>           offside (a : as)    =  Apply (a : bs) : offside cs
>               where (bs, cs)  =  span (\a' -> col' a < col' a') as
>           col' (Atom a)       =  col a
>           col' (Paren a _ _)  =  col a
>
> atom                          :: (CToken tok) => Parser (Pos tok) (Atom (Pos tok))
> atom                          =  fmap Atom noSep
>                               `mplus` do l <- left
>                                          e <- chunk
>                                          r <- right l
>                                          return (Paren l e r)

Primitive parser.

> sep, noSep, left              :: (CToken tok) => Parser tok tok
> sep                           =  satisfy (\t -> catCode t == Sep)
> noSep                         =  satisfy (\t -> catCode t == NoSep)
> left                          =  satisfy (\t -> case catCode t of Del c -> c `elem` "(["; _-> False)
> right l                       =  satisfy (\c -> case (catCode l, catCode c) of
>                                      (Del o, Del c) -> (o,c) `elem` zip "([" ")]" 
>                                      _     -> False)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Internal alignment}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\Todo{Internal alignment Spalte automatisch bestimmen. Vorsicht: die
Position von |=| oder |::| heranzuziehen ist gef"ahrlich; wenn z.B.
|let x = e| in einem |do|-Ausdruck vorkommt.}

> data Line a                   =  Blank
>                               |  Three a a a
>                               |  Multi a
>
> align                         :: (CToken tok) => Maybe Int -> [[Pos tok]] -> [Line [Pos tok]]
> align c                       =  fmap (maybe Multi split3 c)
>   where
>   split3 i ts                 =  case span (\t -> col t < i) ts of
>       ([], [])                -> Blank
>       ((_ : _), [])           -> Multi ts
>       (us, v : vs)
>           | col v == i && isInternal v
>                               -> Three us [v] vs
>           | null us           -> Three [] [] (v : vs)
>           | otherwise         -> Multi ts
>
>
> isInternal                    :: (CToken tok) => tok -> Bool
> isInternal t                  =  case token t of
>     Consym _                  -> True
>     Varsym _                  -> True
>     Special _                 -> True
>     _                         -> False
>
> instance Functor Line where
>     fmap f Blank              =  Blank
>     fmap f (Three l c r)      =  Three (f l) (f c) (f r)
>     fmap f (Multi a)          =  Multi (f a)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding spaces}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Inserting spaces before and after keywords. We use a simple finite
automata with three states: |before b| means before a keyword, |b|
indicates whether to insert a space or not; |after| means immediately
after a keyword (hence |before b| really means not immediately after).

> addSpaces                     :: (CToken tok) => [tok] -> [tok]
> addSpaces ts                  =  before False ts
>     where
>     before b []               =  []
>     before b (t : ts)         =  case token t of
>         u | selfSpacing u     -> t : before False ts
>         Special c
>           | c `elem` ",;([{"  -> t : before False ts
>         Keyword _             -> [ fromToken (TeX False sub'space) | b ] ++ t : after ts
>         _                     -> t : before True ts
> 
>     after []                  =  []
>     after (t : ts)            =  case token t of
>         u | selfSpacing u     -> t : before False ts
>         Special c
>           | c `elem` ",;([{"  -> fromToken (TeX False sub'space) : t : before False ts
>         Keyword _             -> fromToken (TeX False sub'space) : t : after ts
>         _                     -> fromToken (TeX False sub'space) : t : before True ts

Operators are `self spacing'.

> selfSpacing                   :: Token -> Bool
> selfSpacing (Consym _)        =  True
> selfSpacing (Varsym _)        =  True
> selfSpacing (Op _)            =  True
> -- |selfSpacing (TeX _) =  True|
> selfSpacing _                 =  False

\NB It's not a good idea to regard inline \TeX\ as self spacing consider,
for example, a macro like @%format mu = "\mu "@.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Left indentation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Auch wenn |auto = False| wird der Stack auf dem laufenden gehalten.

> type Stack                    =  [(Col, Doc, [Pos Token])]
>
> leftIndent dict auto (lst, rst)
>                               =  loop lst rst
>   where
>   copy d | auto               =  d
>          | otherwise          =  Empty

Die Funktion |isInternal| pr"uft, ob |v| ein spezielles Symbol wie
@::@, @=@ etc~oder ein Operator wie @++@ ist.

>   loop lst rst []             =  (Empty, (lst, rst))
>   loop lst rst (l : ls)       =  case l of
>       Blank                   -> loop lst rst ls
>       Three l c r             -> (sub'column3 (copy lskip <> latexs dict l)
>                                               (latexs dict c)
>                                               (copy rskip <> latexs dict r) <> sep ls <> rest, st')
>           where (lskip, lst') =  indent l lst
>                 (rskip, rst') =  indent r rst
>                 (rest, st')   =  loop lst' rst' ls -- does not work: |if null l && null c then rst' else []|
>       Multi m                 -> (sub'column1 (copy lskip <> latexs dict m) <> sep ls <> rest, st')
>           where (lskip, lst') =  indent m lst
>                 (rest, st')   =  loop lst' [] ls
>
>   sep []                      =  Empty
>   sep (Blank : _ )            =  sub'blankline
>   sep (_ : _)                 =  sub'nl
>
>   indent                      :: [Pos Token] -> Stack -> (Doc, Stack)
>   indent [] stack             =  (Empty, stack)
>   indent ts@(t : _) []        =  (Empty, [(col t, Empty, ts)])
>   indent ts@(t : _) (top@(c, skip, line) : stack)
>                               =  case compare (col t) c of
>       LT                      -> indent ts stack
>       EQ                      -> (skip, (c, skip, ts) : stack)
>       GT                      -> (skip', (col t, skip', ts) : top : stack)
>           where
>           skip'               =  case span (\u -> col u < col t) line of
>               (us, v : vs) | col v == col t
>                               -> skip <> sub'phantom (latexs dict us)
>               -- does not work: |(us, _) -> skip ++ [Phantom (fmap token us), Skip (col t - last (c : fmap col us))]|
>               _               -> skip <> sub'hskip (Text em)
>                   where em    =  showFFloat (Just 2) (0.5 * fromIntegral (col t - c) :: Double) ""

M"ussen |v| und |t| zueinander passen?
%
\begin{verbatim}
where |a      =    where |Str c =    [    [    (    {
      |(b, c) =          |c@(..)=    ,    |    ,    ;
                                     ]    ]    )    }
\end{verbatim}

