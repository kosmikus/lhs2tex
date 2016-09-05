%-------------------------------=  --------------------------------------------
\subsection{Common code for math and poly formatters}
%-------------------------------=  --------------------------------------------

ks, 15.06.2004: I have moved common code from the math and poly formatters
to this module. Poly has been created from a copy of the old math formatter,
therefore there has been much overlap between the two modules.

> module MathCommon             (  module MathCommon  )
> where

> import Typewriter ( latex )
> import Document
> import Directives ( Formats )
> import HsLexer
> import qualified FiniteMap as FM
> import Auxiliaries
>
> import Control.Applicative

> when :: Monad m => Bool -> (a -> m a) -> (a -> m a)
> when True f                   =  f
> when False _f                 =  return

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding positional information}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Row                      =  Int
> type Col                      =  Int
>
> data Pos a                    =  Pos {posrow :: !Row, poscol :: !Col, posann :: a}
>                                  deriving (Show)

%{
%format r1
%format r2
%format c1
%format c2

> instance Eq (Pos a) where
>     Pos r1 c1 _ == Pos r2 c2 _=  r1 == r2 && c1 == c2
> instance Ord (Pos a) where
>     Pos r1 c1 _ <= Pos r2 c2 _=  (r1, c1) <= (r2, c2)

> pos2string :: Pos a -> String
> pos2string (Pos r c _) = "'" ++ show r ++ "_" ++ show c

%}

> instance (CToken tok) => CToken (Pos tok) where
>     catCode (Pos _ _ t)       =  catCode t
>     token (Pos _ _ t)         =  token t
>     inherit (Pos r c t') t    =  Pos r c (inherit t' t)
>     fromToken t               =  Pos 0 0 (fromToken t)

Numbering the list of tokens.

> number                        :: Row -> Col -> [Token] -> [Pos Token]
> number _r _c []               =  []
> number r  c  (t : ts)         =  Pos r c t : number r' c' ts
>     where (r', c')            =  count r c (string t)
>
> count                         :: Row -> Col -> String -> (Row, Col)
> count r c []                  =  (r, c)
> count r c (a : s)
>     | a == '\n'               =  count (r + 1) 1       s
>     | otherwise               =  count r       (c + 1) s

Splitting the token list in lines.

> lines                         :: [Pos a] -> [[Pos a]]
> lines                         =  split 1
>     where
>     split _   []              =  []
>     split r ts                =  us : split (r + 1) vs
>         where (us, vs)        =  span (\t -> posrow t <= r) ts

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A very simple Haskell Parser}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, 27.06.2003: I'll add some explanation which reflects the way I understand
things. Since I don't know Smugweb and I haven't written the code below, it is
possible that the explanation is not adequate:

A |Chunk| is a sequence of \emph{delimiters} or \emph{applications}. Delimiters
are keywords or operators. Applications are everything else.

An |application| is a sequence of atoms that are forming a Haskell
function application. The list must never be empty, but can contain
a single element (for instance, in normal infix expressions such as |2 + 3|
this will occur frequently).

An |atom| is a single identifier (not an operator, though -- those are
delimiters), or a chunk in parentheses.

> type Chunk a                  =  [Item a]
>
> data Item a                   =  Delim a
>                               |  Apply [Atom a]
>                                  deriving (Show)
>
> data Atom a                   =  Atom a
>                               |  Paren a (Chunk a) a
>                                  deriving (Show)

The parser itself differs between the two styles. The math formatter
cannot handle unbalanced parentheses, the poly formatter has a heuristic
that allows successful parsing of unbalanced parentheses in many, but
not all cases.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Making replacements}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Mode                     =  Mandatory
>                               |  Optional Bool

If |eval e| returns |Mandatory| then parenthesis around |e| must not be
dropped; |Optional True| indicates that it can be dropped; |Optional
False| indicates that the decision is up the caller.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Making replacements}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, 23.07.2003: This substitute function does not work recursively.
To change this is on my TODO list. Substitutions without arguments,
hovewer, do work recursively because they are handled again at a later
stage (by the call to latexs, for instance in leftIndent).

> substitute                    :: (CToken tok,Show tok) => Formats -> Bool -> Chunk (Pos tok) -> [Pos tok]
> substitute d auto chunk       =  snd (eval chunk)
>   where
>   eval                        :: (CToken tok) => [Item (Pos tok)] -> (Mode,[Pos tok])
>   eval [e]                    =  eval' e
>   eval chunk'                 =  (Optional False, concat [ snd (eval' i) | i <- chunk' ])
>
>   eval'                       :: (CToken tok) => Item (Pos tok) -> (Mode,[Pos tok])
>   eval' (Delim s)             =  (Optional False, [s])
>   eval' (Apply [])            =  impossible "eval'"
>   eval' (Apply (e : es))      =  eval'' False e es
>
>   eval''                      :: (CToken tok) => Bool -> Atom (Pos tok) -> [Atom (Pos tok)] -> (Mode,[Pos tok])
>   eval'' _ (Atom s) es        =  case FM.lookup (string (token s) ++ pos2string s) d <|> FM.lookup (string (token s)) d of
>     Nothing                   -> (Optional False, s : args es)
>     Just (opt, opts, lhs, rhs)-> (Optional opt, set s (concat (fmap sub rhs)) ++ args bs)
>         where
>         (as, bs) | m <= n     =  (es ++ replicate (n - m) dummy, [])
>                  | otherwise  =  splitAt n es
>         n                     =  length lhs
>         m                     =  length es
>         binds                 =  zip lhs [ snd (eval'' b a []) | (b, a) <- zip opts as ]
>         sub t@(Varid x)       =  case FM.lookup x (FM.fromList binds) of
>             Nothing           -> [fromToken t]
>             Just ts           -> ts
>         sub t                 =  [fromToken t]

Whenever a token is replaced or removed, the first token of the replacement
inherits the position of the original token.

>   eval'' opt (Paren l e r) es
>       | isOptional            =  (Mandatory, set l s ++ args es)
>       | otherwise             =  (Optional False, [l] ++ s ++ [r] ++ args es)
>       where (flag, s)         =  eval e
>             isOptional        =  catCode l == Del "(" && not (mandatory e)
>                               && case flag of Mandatory -> False; Optional f -> opt || f

\NB It is not a good idea to remove parentheses around atoms, because
that would remove the parentheses in @deriving (Eq)@ and @module M (a)@
as well.

>   args                        :: (CToken tok) => [Atom (Pos tok)] -> [Pos tok]
>   args es                     =  concat [ sp ++ snd (eval'' False i []) | i <- es ] -- $\cong$ Applikation
>   sp                          :: (CToken tok) => [Pos tok]
>   sp | auto                   =  [fromToken (TeX False sub'space)]
>      | otherwise              =  []

To support macros of the form @%format Parser (a) = a@.

> set                           :: (CToken tok) => tok -> [tok] -> [tok]
> set _s []                     =  []
> set s  (t : ts)               =  inherit s (token t) : ts
>
> mandatory                     :: (CToken tok) => Chunk tok -> Bool
> mandatory _e                  =  False

Code before:

< mandatory e                   =  null e               -- nullary tuple
<                               || or [ isComma i | i <- e ] -- tuple
<                               || isOp (head e)        -- left section
<                               || isOp (last e)        -- right section

> isComma, isOp                 :: (CToken tok) => Item tok -> Bool
> isComma (Delim t)             =  case token t of
>     Special c                 -> c == ','
>     _                         -> False
> isComma _                     =  False
>
> isOp (Delim t)                =  case token t of
>     Special c                 -> c == '`'     -- f"ur @` div `@
>     Consym _                  -> True
>     Varsym s                  -> s /= "\\"
>     Op _                      -> True
>     _                         -> False
> isOp _                        =  False

> dummy                         :: (CToken tok) => Atom tok
> dummy                         =  Atom (fromToken (Varid ""))

\NB We cannot use embedded \TeX\ text here, because |TeX| is not a
legal atom (|string| is applied to it).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding spaces and indentation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

There are subtle differences between the two styles.

For inline-code.

> latexs                        :: (CToken tok) => Formats -> [tok] -> Doc
> latexs dict                   =  catenate . fmap (latex sub'space sub'space dict . token)
