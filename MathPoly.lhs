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
> import Data.List              (  partition, nub, insert, sort, transpose )
> import Numeric                (  showFFloat )
> import Control.Monad          (  MonadPlus(..) )
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
> import TeXCommands            (  Lang(..)  )
> -- import Debug.Trace ( trace )

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Inline and display code}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> inline                        :: Lang -> Formats -> Bool -> String -> Either Exc Doc
> inline lang fmts auto         =  fmap unNL
>                               .> tokenize lang
>                               @> lift (number 1 1)
>                               @> when auto (lift (filter (isNotSpace . token)))
>                               @> lift (partition (\t -> catCode t /= White))
>                               @> exprParse *** return
>                               @> lift (substitute fmts auto) *** return
>                               @> lift (uncurry merge)
>                               @> lift (fmap token)
>                               @> when auto (lift addSpaces)
>                               @> lift (latexs fmts)
>                               @> lift sub'inline

> display                       :: Lang -> Formats -> Bool -> Int -> Int -> Stack
>                               -> String -> Either Exc (Doc, Stack)
> display lang fmts auto sep lat stack
>                               =  lift trim
>                               @> lift (expand 0)
>                               @> tokenize lang
>                               @> lift (number 1 1)
>       --                     |@> when auto (lift (filter (isNotSpace . token)))|
>                               @> lift (partition (\t -> catCode t /= White))
>                               @> exprParse *** return
>                               @> lift (substitute fmts auto) *** return
>                               @> lift (uncurry merge)
>                               @> lift lines
>                               @> when auto (lift (fmap addSpaces))
>                               @> lift (\ts -> (autoalign sep ts,ts))
>       --                     |@> lift (\(x,y) -> trace ((unlines $ map show $ y) ++ "\n" ++ show x) (x,y))|
>                               @> lift (\(cs,ts) -> let ats = align cs sep lat ts
>                                                        cs' = [("B",0)] ++ cs 
>                                                           ++ [("E",error "E column")]
>                                                    in  (autocols cs' ats,ats)
>                                       )
>                               @> return *** when auto (lift (fmap (fmap (filter (isNotSpace . token)))))
>       --                     |@> return *** when auto (lift (fmap (fmap (addSpaces . filter (isNotSpace . token)))))|
>                               @> lift (\((cs,z),ats) -> (cs,(z,ats)))
>                               @> return *** lift (\(z,ats) -> leftIndent fmts auto z [] ats)
>       -- ks, 17.07.2003: i've changed "stack" into "[]" and thereby disabled
>       -- the global stack for now as it leads to unexepected behaviour
>                               @> lift (\(cs,(d,stack)) -> (sub'code (columns cs <> d),stack))
>
> columns                       :: [(String,Doc)] -> Doc
> columns                       =  foldr (<>) Empty 
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
>                                     as <- many (do s <- sep; a <- many (atom d); return ([Delim s] ++ offside a))
>                                     return (offside a ++ concat as)
>     where offside []          =  []
>           -- old: |opt a =  [Apply a]|
>           offside (a : as)    =  Apply (a : bs) : offside cs
>               where (bs, cs)  =  span (\a' -> col' a < col' a') as
>           col' (Atom a)       =  col a
>           col' (Paren a _ _)  =  col a
>
> atom                          :: (CToken tok) => Int -> Parser (Pos tok) (Atom (Pos tok))
> atom d                        =  fmap Atom noSep
>                               `mplus` do l <- left
>                                          e <- chunk (d+1)
>                                          r <- right l
>                                          return (Paren l e r)
>                               `mplus` if d == 0 then do r <- anyright
>                                                         return (Paren (fromToken $ TeX False Empty) [] r)
>                                                 else mzero

ks, 09.09.2003: Added handling of unbalanced parentheses, surely not in the
most elegant way. Both |chunk| and |atom| now take an integer argument
indicating the nesting level. Only on the top-level unbalanced right
parentheses are accepted. The end of file (end of code block) can be
parsed as an arbitrary amount of right parentheses.

Primitive parser.

> sep, noSep, left, anyright    :: (CToken tok) => Parser tok tok
> sep                           =  satisfy (\t -> catCode t == Sep)
> noSep                         =  satisfy (\t -> catCode t == NoSep)
> left                          =  satisfy (\t -> case catCode t of Del c -> c `elem` "([{"; _ -> False)
> anyright                      =  satisfy (\t -> case catCode t of Del c -> c `elem` ")]}"; _ -> False)
> right l                       =  (satisfy (\c -> case (catCode l, catCode c) of
>                                       (Del o, Del c) -> (o,c) `elem` zip "([{" ")]}" 
>                                       _     -> False)
>                                  ) `mplus` do eof
>                                               return (fromToken $ TeX False Empty)

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
>       (_, [v])                -> []   -- last token is whitespace, doesn't matter
>       (_, v:v':vs)
>         | row v' == 0 && col v' == 0
>                               -> findCols (v:vs)  -- skip internal tokens (automatically added spaces)
>         | length (string (token v)) >= sep
>                               -> {- |trace ("found: " ++ show (col v')) $| -} col v' : findCols (v':vs)
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
>                                                          , (row x /= 0 || col x /= 0) && isNotSpace (token x)]
>                                                      || null res 
>                                              then Blank
>                                              else Poly res
>                                       ) toks
>   where
>   splitn cc ind [] []         =  []
>   splitn cc ind [] ts         =  [(cc,ts,ind)]
>   splitn cc ind ((n,i):oas) ts=  
>     case span (\t -> col t < i) ts of
>       ([], vs)                -> splitn cc ind oas vs
>       (us, [])                -> [(cc,us,ind)]
>       (us, (v:vs))            -> 
>         let lu = head [ u | u <- reverse us, col u /= 0 || row u /= 0 ]
>                                  -- again, we skip automatically added spaces
>             llu = length (string (token lu))
>         in case () of
>             _ | (lat /= 0 && isNotSpace (token lu)) || llu < lat || col v /= i
>                                  -- no alignment for this column
>                               -> splitn cc ind oas (us ++ (v:vs))
>               | not (isNotSpace (token lu)) && llu >= sep
>                               -> (cc,us,ind) : splitn (n,i) True oas (v:vs)
>               | otherwise
>                               -> (cc,us,ind) : splitn (n,i) False oas (v:vs)

Die Funktion |isInternal| pr"uft, ob |v| ein spezielles Symbol wie
@::@, @=@ etc~oder ein Operator wie @++@ ist.

> isInternal                    :: (CToken tok) => tok -> Bool
> isInternal t                  =  case token t of
>     Consym _                  -> True
>     Varsym _                  -> True
>     Special _                 -> True
>     _                         -> False
>
> instance Functor Line where
>     fmap f Blank              =  Blank
>     fmap f (Poly ls)          =  Poly (map (\(x,y,z) -> (x,f y,z)) ls)

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
>     anyinternals              = map (or . map (any isInternal)) cts
>
>     -- deline                    :: [(String,Int)] -> Line [a] -> [[[a]]]
>     deline cs Blank           = []
>     deline cs (Poly ls)       = [decol cs ls]
>
>     decol cs []               = replicate (length cs) []
>     decol ((cn,_):cs) r@(((cn',_),ts,_):rs)
>       | cn' == cn             = ts : decol cs rs
>       | otherwise             = [] : decol cs r

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

\NB It's not a good idea to regard inline \TeX\ as self spacing consider,
for example, a macro like @%format mu = "\mu "@.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Left indentation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

ks, 16.07.2003: Die Bedeutung von |auto| verstehe ich nicht so ganz:
Auch wenn |auto = False| wird der Stack auf dem laufenden gehalten.

ks, 16.07.2003: Ich versuche nun, das folgende relativ einfache
Einrueckungsverhalten zu implementieren -- aus meiner bisherigen Erfahrung
mit dem @poly@-style heraus habe ich den Eindruck, als muesste das
weitgehend genuegen. Ansonsten kann man ja immer noch explizit die
Einrueckung mit Annotationen formatieren.

> type Stack                    =  [(Col, Line [Pos Token])]

Der Stack besteht also aus einer Liste von Paaren aus Spaltennummern
und Token. Der Kopf der Liste hat die hoechste Spaltennummer, und die
Spaltennummern in der Liste sind absteigend.

Einrueckung findet immer am Beginn einer neuen Zeile statt, wobei
die Position des ersten Tokens in der Zeile relevant ist.
Als erstes wird der Stack adjustiert: alle Elemente, die hoehere
oder gleiche Spaltennummern haben als die augenblickliche Zeile, 
werden entfernt.

Dann wird in dem nun obersten Stackelement nach dem letzten Token
gesucht, das eine Spaltenposition kleiner oder gleich dem aktuellen
Element hat. Bezueglich diesem wird nun eingerueckt.
Achtung: Derzeit findet das \emph{immer} statt. Das ist vielleicht
keine so gute Idee, aber mir fallen nur wenige Situationen ein,
in denen es von Schaden waere.

Letztlich wird die augenblickliche Zeile auf den Stack gelegt.

> leftIndent                    :: Formats -> Bool 
>                               -> [Col]        -- zentrierte Spalten
>                               -> Stack        -- augenblicklicher Stack
>                               -> [Line [Pos Token]]
>                               -> (Doc, Stack)
> leftIndent dict auto z stack
>                               =  loop True stack
>   where
>   copy d | auto               =  d
>          | otherwise          =  Empty

>   loop                        :: Bool -> Stack -> [Line [Pos Token]] -> (Doc, Stack)
>   loop first stack []         =  (Empty, stack)  -- fertig
>   loop first stack (l:ls)     =  case l of
>       Blank                   -> loop True stack ls -- Leerzeilen ignorieren
>    {-| Poly x || trace (show x) False -> undefined |-}
>       Poly []                 -> loop True stack ls -- naechste Zeile
>       Poly (((n,c),[],ind):rs)
>         | first               -> loop True stack (Poly rs:ls) -- ignoriere leere Spalten zu Beginn
>       Poly p@(((n,c),ts,ind):rs)
>         | first               -> -- ueberpruefe Einrueckung:
>                                  let -- Schritt 1: Stack verkleinern
>                                      rstack  = dropWhile (\(rc,_) -> rc >= c) stack
>                                      -- Schritt 2: relevante Spalte finden
>                                      (rn,rc) = findrel (n,c) rstack
>                                      -- Schritt 3: Zeile auf Stack legen
>                                      fstack  = (c,l) : rstack
>                                  in mkFromTo fstack rn n rc [fromToken $ TeX False (indent (rn,rc) (n,c))] p ls
>                                              
>
>         | c `elem` z          -> mkFromTo stack n (n ++ "E") c ts rs ls
>                                                     -- zentrierte Spalten gesondert behandeln
>       Poly [((n,c),ts,ind)]   -> mkFromTo stack n "E" c ts [] ls
>                                                     -- letzte Spalten
>       Poly (((n,c),ts,ind):rs@(((nn,_),_,_):_))
>                               -> mkFromTo stack n nn  c ts rs ls 
>
>   mkFromTo stack bn en c ts rs ls
>     | bn == en                =  -- dies kann am Beginn einer Zeile durch Einrueckung passieren
>                                  (rest,stack')
>     | otherwise               =  (sub'fromto bn en (latexs dict ts)
>                                     <> (if null rs then sep ls else Empty) <> rest
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
>   findrel (n,c) ((_,Poly t):_)
>                               =  case break (\((n',c'),_,_) -> c' > c) t of
>                                    ([],_)     -> error "findrel: the impossible happened"
>                                    (pre,_)    -> let ((rn,rc),_,_) = last pre
>                                                  in  (rn,rc)
>
>   sep []                      =  Empty
>   sep (Blank : _ )            =  sub'blankline
>   sep (_ : _)                 =  sub'nl
>
>   indent                      :: (String,Int) -> (String,Int) -> Doc
>   indent (n,c) (n',c')
>     | c /= c'                 =  (sub'indent (Text (show (c' - c))))
>     | otherwise               =  Empty

M"ussen |v| und |t| zueinander passen?
%
\begin{verbatim}
where |a      =    where |Str c =    [    [    (    {
      |(b, c) =          |c@(..)=    ,    |    ,    ;
                                     ]    ]    )    }
\end{verbatim}

