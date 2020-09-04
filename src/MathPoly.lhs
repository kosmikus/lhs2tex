%-------------------------------=  --------------------------------------------
\subsection{Poly formatter}
%-------------------------------=  --------------------------------------------

ks, 28.07.2003: This is a new style that is based on the old @math@-style
and is intended to replace @math@ style in a future version. Because the
former @math@ style should remain compatible, I've copied the entire module.
Essentially, there are the same functions here doing the same job, but there
are subtle differences, and they will grow over time \dots

%if codeOnly || showModuleHeader

> module MathPoly               (  module MathPoly, substitute, number  )
> where
>
> import Prelude hiding         (  lines )
> import Data.List              (  partition, nub, sort, transpose )
> import Control.Applicative
> import Control.Arrow          (  (>>>) )
> import Control.Monad          (  (>=>), mplus )
>
> import Verbatim               (  expand, trim )
> import MathCommon
> import Document
> import Directives
> import HsLexer
> import Parser
> import Auxiliaries
> import TeXCommands            (  Lang(..)  )
> -- import Debug.Trace ( trace )

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

> display                       :: Lang -> Int -> Formats -> Bool -> Int -> Int -> Stack
>                               -> String -> Either Exc (Doc, Stack)
> display lang line fmts auto sep lat _stack
>                               =   lift trim
>                               >=> lift (expand 0)
>                               >=> tokenize lang
>                               >=> lift (number line 1)
>       --                     |>=> when auto (lift (filter (isNotSpace . token)))|
>                               >=> lift (partition (\t -> catCode t /= White))
>                               >=> exprParse *** return
>                               >=> lift (substitute fmts auto) *** return
>                               >=> lift (uncurry merge)
>                               >=> lift lines
>                               >=> when auto (lift (fmap addSpaces))
>                               >=> lift (\ts -> (autoalign sep ts,ts))
>       --                     |>=> lift (\(x,y) -> trace ((unlines $ map show $ y) ++ "\n" ++ show x) (x,y))|
>                               >=> lift (\(cs,ts) -> let ats = align cs sep lat ts
>                                                         cs' = [("B",0)] ++ cs
>                                                            ++ [("E",error "E column")]
>                                                     in  (autocols cs' ats,ats)
>                                        )
>                               >=> return *** when auto (lift (fmap (fmap (filter (isNotSpace . token)))))
>       --                     |>=> return *** when auto (lift (fmap (fmap (addSpaces . filter (isNotSpace . token)))))|
>                               >=> lift (\((cs,z),ats) -> (cs,(z,ats)))
>                               >=> return *** lift (\(z,ats) -> leftIndent fmts auto z [] ats)
>       -- ks, 17.07.2003: i've changed "stack" into "[]" and thereby disabled
>       -- the global stack for now as it leads to unexepected behaviour
>                               >=> lift (\(cs,(d,stack)) -> (sub'code (columns cs <<>> d),stack))
>
> columns                       :: [(String,Doc)] -> Doc
> columns                       =  foldr (<<>>) Empty
>                               .  map (uncurry sub'column)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A very simple Haskell Parser}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The parser is based on the Smugweb parser.
This variant can handle unbalanced parentheses in some cases (see below).

> exprParse                     :: (CToken tok, Show tok) => [Pos tok] -> Either Exc (Chunk (Pos tok))
> exprParse s                   =  case run (chunk 0) s of
>     Nothing                   -> Left ("syntax error", show s) -- HACK: |show s|
>     Just e                    -> Right e
>
> chunk                         :: (CToken tok) => Int -> Parser (Pos tok) (Chunk (Pos tok))
> chunk d                       =  do a <- many (atom d)
>                                     as <- many (do s <- sep; a' <- many (atom d); return (Delim s : offside a'))
>                                     return (offside a ++ concat as)
>     where offside []          =  []
>           -- old: |opt a =  [Apply a]|
>           offside (a : as)    =  Apply (a : bs) : offside cs
>               where (bs, cs)  =  span (\a' -> col' a < col' a') as
>           col' (Atom a)       =  poscol a
>           col' (Paren a _ _)  =  poscol a
>
> atom                          :: (CToken tok) => Int -> Parser (Pos tok) (Atom (Pos tok))
> atom d                        =   fmap Atom noSep
>                               <|> do l <- left
>                                      e <- chunk (d+1)
>                                      r <- right l
>                                      return (Paren l e r)
>                               <|> if d == 0 then do r <- anyright
>                                                     return (Paren (fromToken $ TeX False Empty) [] r)
>                                             else empty

ks, 09.09.2003: Added handling of unbalanced parentheses, surely not in the
most elegant way. Both |chunk| and |atom| now take an integer argument
indicating the nesting level. Only on the top-level unbalanced right
parentheses are accepted. The end of file (end of code block) can be
parsed as an arbitrary amount of right parentheses.

Primitive parser.

> sep, noSep, left, anyright    :: (CToken tok) => Parser tok tok
> sep                           =  satisfy (\t -> catCode t == Sep)
> noSep                         =  satisfy (\t -> catCode t == NoSep)
> left                          =  satisfy (\t -> case catCode t of Del c -> c `elem` ["(","[","{","'["]; _ -> False)
> anyright                      =  satisfy (\t -> case catCode t of Del c -> c `elem` [")","]","}"]; _ -> False)
> right l                       =  satisfy (\c -> case (catCode l, catCode c) of
>                                      (Del o, Del c) -> (o,c) `elem` zip ["(","[","{","'["] [")","]","}","]"]
>                                      _     -> False)
>                                   `mplus` do eof
>                                              return (fromToken $ TeX False Empty)

ks, 06.09.2003: Modified the |right| parser to accept the end of file,
to allow for unbalanced parentheses. This behaviour is not (yet) backported
to |math| style. Also added |anyright|.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Internal alignment}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Line a                   =  Blank
>                               |  Poly  [((String,Int),a,Bool)]
>
> autoalign                     :: (Show tok,CToken tok) => Int              -- "Trennung"
>                                               -> [[Pos tok]]      -- positionierte tokens per Zeile
>                                               -> [(String,Int)]   -- alignment-info (Name, Spalte)
> autoalign sep toks            =  map (\x -> (show x,x))
>                               .  nub
>                               .  sort
>                               .  concat
>                               .  fmap findCols
>                               $  toks
>   where
>   findCols                    :: (CToken tok,Show tok) => [Pos tok] -> [Col]
>   findCols ts                 =  case {- |trace (show ts)| -}
>                                       (break (\t -> not . isNotSpace . token $ t) ts) of
>       (_, [])                 -> []   -- done
>       (_, [_v])               -> []   -- last token is whitespace, doesn't matter
>       (_, v:v':vs)
>         | posrow v' == 0 && poscol v' == 0
>                               -> findCols (v:vs)  -- skip internal tokens (automatically added spaces)
>         | length (string (token v)) >= sep
>                               -> {- |trace ("found: " ++ show (col v')) $| -} poscol v' : findCols (v':vs)
>         | otherwise           -> {- |trace ("found too short")|            -} findCols (v':vs)

ks, 21.11.2005: I've fixed a bug that was known to me since long ago, but I never got
around to investigate. When a parametrized formatting directive directly precedes a
token that should be aligned, then sometimes that token was not aligned. The reason
was that in |findCols| above, the recursive calls used |vs| instead of |(v':vs)|.

> align                         :: (CToken tok) => [(String,Int)]   -- alignment-info (Name, Spalte)
>                                               -> Int              -- "Trennung"
>                                               -> Int              -- "Traegheit"
>                                               -> [[Pos tok]]      -- positionierte tokens per Zeile
>                                               -> [Line [Pos tok]]
> align cs sep lat toks         =  fmap (\t -> {- |trace (show (map token t) ++ "\n") $| -}
>                                              let res = splitn ("B",0) False cs t
>                                              in  if null [x | x <- t
>                                                          , (posrow x /= 0 || poscol x /= 0) && isNotSpace (token x)]
>                                                      || null res
>                                              then Blank
>                                              else Poly res
>                                       ) toks
>   where
>   splitn _cc _ind [] []       =  []
>   splitn  cc  ind [] ts       =  [(cc,ts,ind)]
>   splitn  cc  ind ((n,i):oas) ts=
>     case span (\t -> poscol t < i) ts of
>       ([], vs)                -> splitn cc ind oas vs
>       (us, [])                -> [(cc,us,ind)]
>       (us, (v:vs))            ->
>         let lu = head [ u | u <- reverse us, poscol u /= 0 || posrow u /= 0 ]
>                                  -- again, we skip automatically added spaces
>             llu = length (string (token lu))
>         in case () of
>             _ | (lat /= 0 && isNotSpace (token lu)) || llu < lat || poscol v /= i
>                                  -- no alignment for this column
>                               -> splitn cc ind oas (us ++ (v:vs))
>               | not (isNotSpace (token lu)) && llu >= sep
>                               -> (cc,us,ind) : splitn (n,i) True oas (v:vs)
>               | otherwise
>                               -> (cc,us,ind) : splitn (n,i) False oas (v:vs)

The function |isInternal| returns |True| iff the argument is a symbol
or a special internal symbol. See @HsLexer@ for the list of special
symbols.

> isInternal                    :: (CToken tok) => tok -> Bool
> isInternal t                  =  case token t of
>     Consym _                  -> True
>     Varsym _                  -> True
>     Special _                 -> True
>     _                         -> False
>
> instance Functor Line where
>     fmap _f Blank             =  Blank
>     fmap  f (Poly ls)         =  Poly (map (\(x,y,z) -> (x,f y,z)) ls)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Automatically determining centered columns}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

We use a simple heuristic: a column that contains only single tokens and
at least one ``internal'' token is centered. For centered columns, we create
an additional ``end'' column to make sure that all entries are centered on the
same amount of space.

> autocols                      :: (CToken tok, Show tok) => [(String,Int)]   -- column info
>                                               -> [Line [Pos tok]] -- aligned tokens
>                                               -> ([(String,Doc)],[Col]) -- cols+alignment, plus centered columns
> autocols cs ats               = (\(x,y) -> (concat x,concat y)) $ unzip
>                               $ zipWith3 (\(cn,n) ml ai ->
>                                              if ml <= 2 && ai then ([(cn,sub'centered)
>                                                                     ,(cn ++ "E",sub'dummycol)
>                                                                     ],[n])
>                                                               else ([(cn,sub'left)],[])
>                                          ) cs maxlengths anyinternals
>                                 -- length 2, because space tokens are always there
>     where
>     cts                       = transpose (concatMap (deline cs) ats)
>     maxlengths                = {- |trace (show cts) $ |-} map (maximum . map length) cts
>     anyinternals              = map (any (any isInternal)) cts
>
>     deline                    :: [(String,Int)] -> Line [a] -> [[[a]]]
>     deline _cs' Blank         = []
>     deline  cs' (Poly ls)     = [decol cs' ls]
>
>     decol                     :: [(String, Int)] -> [((String, Int), [a], Bool)] -> [[a]]
>     decol cs' []              = replicate (length cs') []
>     decol ((cn,_):cs') r@(((cn',_),ts,_):rs)
>       | cn' == cn             = ts : decol cs' rs
>       | otherwise             = [] : decol cs' r
>     decol _ _                 = impossible "autocols.decol"

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Adding spaces}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Inserting spaces before and after keywords. We use a simple finite
automata with three states: |before b| means before a keyword, |b|
indicates whether to insert a space or not; |after| means immediately
after a keyword (hence |before b| really means not immediately after).

> addSpaces                     :: (CToken tok) => [tok] -> [tok]
> addSpaces ts0                 =  before False ts0
>     where
>     before _b []              =  []
>     before  b (t : ts)        =  case token t of
>         u | not (isNotSpace u)-> t : before b ts
>           | selfSpacing u     -> t : before False ts
>         Special c
>           | c `elem` ",;([{"  -> t : before False ts
>         Keyword _             -> [ fromToken (TeX False sub'space) | b ] ++ t : after ts
>         _                     -> t : before True ts
>
>     after []                  =  []
>     after (t : ts)            =  case token t of
>         u | not (isNotSpace u)-> t : after ts
>           | selfSpacing u     -> t : before False ts
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

\NB It's not a good idea to regard inline \TeX\ as self spacing -- consider,
for example, a macro like @%format mu = "\mu "@.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Left indentation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, 16.07.2003: I don't quite understand the meaning of |auto|. Even
if |auto = False|, the stack is still updated.

ks, 16.07.2003: I'm going to implement the following relatively simple
heuristic for indentation. Based on my current experience with the
@poly@-style I think that this is sufficient. If not, one can still
indent explicitly via annotations.

> type Stack                    =  [(Col, Line [Pos Token])]

The stack is a list of pairs of column numbers and tokens. The head
of the list is the largest column number, and the column numbers in
the list appear sorted and descending.

Indentations occur at the beginning of a line, and the position
of the first token of the line is relevant. First, the stack is
adjusted: all elements that have a higher or equal column number
than the current line are removed.

In the now topmost stack element, we then look for the final token
that occurs at a column less than or equal to the current element.
Relative to this token, we indent. Note: this happens in \emph{all}
situations, currently. Perhaps, there are a few situations where
this is not a good idea, but let's see.

As a final step, the current line is placed on the stack.

> leftIndent                    :: Formats -> Bool
>                               -> [Col]        -- centered columns
>                               -> Stack        -- current stack
>                               -> [Line [Pos Token]]
>                               -> (Doc, Stack)
> leftIndent dict _auto z stack0
>                               =  loop True stack0
>   where
>   loop                        :: Bool -> Stack -> [Line [Pos Token]] -> (Doc, Stack)
>   loop _first stack []        =  (Empty, stack)  -- done
>   loop  first stack (l:ls)    =  case l of
>       Blank                   -> loop True stack ls -- ignore blank lines
>    {-| Poly x || trace (show x) False -> undefined |-}
>       Poly []                 -> loop True stack ls -- next line
>       Poly (((_n,_c),[],_ind):rs)
>         | first               -> loop True stack (Poly rs:ls) -- ignore leading blank columns
>       Poly p@(((n,c),ts,_ind):rs)
>         | first               -> -- check indentation
>                                  let -- step 1: shrink stack
>                                      rstack  = dropWhile (\(rc',_) -> rc' >= c) stack
>                                      -- step 2: find relevant column
>                                      (rn,rc) = findrel (n,c) rstack
>                                      -- step 3: place line on stack
>                                      fstack  = (c,l) : rstack
>                                  in mkFromTo fstack rn n rc [fromToken $ TeX False (indent (rn,rc) (n,c))] p ls
>
>
>         | c `elem` z          -> mkFromTo stack n (n ++ "E") c ts rs ls
>                                                     -- treat centered lines special
>       Poly [((n,c),ts,_ind)]  -> mkFromTo stack n "E" c ts [] ls
>                                                     -- last columns
>       Poly (((n,c),ts,_ind):rs@(((nn,_),_,_):_))
>                               -> mkFromTo stack n nn  c ts rs ls
>
>   mkFromTo                    :: Stack -> String -> String -> Col -> [Pos Token] -> [((String, Int), [Pos Token], Bool)] -> [Line [Pos Token]] -> (Doc, Stack)
>   mkFromTo stack bn en _c ts rs ls
>     | bn == en                =  -- this can happen at the beginning of a line due to indentation
>                                  (rest,stack')
>     | otherwise               =  (sub'fromto bn en (latexs dict ts)
>                                     <<>> (if null rs then sep ls else Empty) <<>> rest
>                                  ,stack'
>                                  )
>     where
>       (rest,stack')           =  loop False  -- not first of a line
>                                       stack
>                                       (Poly rs : ls)
>
>
>   findrel                     :: (String,Col) -> Stack -> (String,Col)
>   findrel (n,c) []            =  (n,c)
>   findrel (n,c) ((_,Blank):r) =  findrel (n,c) r  -- should never happen
>   findrel (_n,c) ((_,Poly t):_)
>                               =  case break (\((_n',c'),_,_) -> c' > c) t of
>                                    ([],_)     -> error "findrel: the impossible happened"
>                                    (pre,_)    -> let ((rn,rc),_,_) = last pre
>                                                  in  (rn,rc)
>
>   sep []                      =  Empty
>   sep (Blank : _ )            =  sub'blankline
>   sep (_ : _)                 =  sub'nl
>
>   indent                      :: (String,Int) -> (String,Int) -> Doc
>   indent (_n,c) (_n',c')
>     | c /= c'                 =  sub'indent (Text (show (c' - c)))
>     | otherwise               =  Empty

%
\begin{verbatim}
where |a      =    where |Str c =    [    [    (    {
      |(b, c) =          |c@(..)=    ,    |    ,    ;
                                     ]    ]    )    }
\end{verbatim}

