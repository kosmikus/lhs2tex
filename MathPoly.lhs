%-------------------------------=  --------------------------------------------
\subsection{Math formatter}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module MathPoly		(  module MathPoly  )
> where
>
> import Prelude hiding ( lines )
> import List ( partition, nub, insert, sort, transpose )
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
> import IOExts ( trace )

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

> display			:: Formats -> Bool -> Int -> Int -> Stack
>				-> String -> Either Exc (Doc, Stack)
> display fmts auto sep lat stack
>                               =  lift trim
>				@> lift (expand 0)
>				@> tokenize
>				@> lift (number 1 1)
>	--			@> when auto (lift (filter (isNotSpace . token)))
>				@> lift (partition (\t -> catCode t /= White))
>				@> exprParse *** return
>				@> lift (substitute fmts auto) *** return
>				@> lift (uncurry merge)
>				@> lift lines
>                               @> when auto (lift (fmap addSpaces))
>                               @> lift (\ts -> (autoalign sep ts,ts))
>				@> lift (\(cs,ts) -> let ats = align cs sep lat ts
>                                                        cs' = [("B",0)] ++ cs 
>                                                           ++ [("E",error "E column")]
>                                                    in  (autocols cs' ats,ats)
>                                       )
>                               @> return *** when auto (lift (fmap (fmap (filter (isNotSpace . token)))))
>       --                      @> return *** when auto (lift (fmap (fmap (addSpaces . filter (isNotSpace . token)))))
>                               @> lift (\((cs,z),ats) -> (cs,(z,ats)))
>                               @> return *** lift (\(z,ats) -> leftIndent fmts auto z [] ats)
>       -- ks, 17.07.2003: i've changed "stack" into "[]" and thereby disabled
>       -- the global stack for now as it leads to unexepected behaviour
>				@> lift (\(cs,(d,stack)) -> (sub'code (columns cs <> d),stack))
>
> columns                       :: [(String,Doc)] -> Doc
> columns                       =  foldr (<>) Empty 
>                               .  map (uncurry sub'column)

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

> exprParse			:: (CToken tok, Show tok) => [Pos tok] -> Either Exc (Chunk (Pos tok))
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

ks, 23.07.2003: This substitute function does not work recursively.
To change this is on my TODO list. Substitutions without arguments,
hovewer, do work recursively because they are handled again at a later
stage (by the call to latexs, for instance in leftIndent).

> substitute			:: (CToken tok,Show tok) => Formats -> Bool -> Chunk tok -> [tok]
> substitute d auto chunk	=  snd (eval chunk)
>   where
>   eval                        :: (CToken tok) => [Item tok] -> (Mode,[tok])
>   eval [e]			=  eval' e
>   eval chunk			=  (Optional False, concat [ snd (eval' i) | i <- chunk ])
>
>   eval'                       :: (CToken tok) => Item tok -> (Mode,[tok])
>   eval' (Delim s)		=  (Optional False, [s])
>   eval' (Apply [])		=  impossible "eval'"
>   eval' (Apply (e : es))	=  eval'' False e es
>
>   eval''                      :: (CToken tok) => Bool -> Atom tok -> [Atom tok] -> (Mode,[tok])
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

>   args                        :: (CToken tok) => [Atom tok] -> [tok]
>   args es			=  concat [ sp ++ snd (eval'' False i []) | i <- es ] -- $\cong$ Applikation
>   sp                          :: (CToken tok) => [tok]
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

> data Line a			=  Blank
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
>                               -> {- |trace ("found: " ++ show (col v')) $| -} col v' : findCols vs
>         | otherwise           -> {- |trace ("found too short")|            -} findCols vs
>
> align				:: (CToken tok) => [(String,Int)]   -- alignment-info (Name, Spalte)
>                                               -> Int              -- "Trennung"
>                                               -> Int              -- "Traegheit"
>                                               -> [[Pos tok]]      -- positionierte tokens per Zeile
>                                               -> [Line [Pos tok]]
> align cs sep lat toks         =  fmap (\t -> {- trace (show (map token t) ++ "\n") $ -}
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

> isInternal			:: (CToken tok) => tok -> Bool
> isInternal t			=  case token t of
>     Consym _			-> True
>     Varsym _			-> True
>     Special _			-> True
>     _				-> False
>
> instance Functor Line where
>     fmap f Blank		=  Blank
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

> addSpaces			:: (CToken tok) => [tok] -> [tok]
> addSpaces ts			=  before False ts
>     where
>     before b []		=  []
>     before b (t : ts)		=  case token t of
>         u | not (isNotSpace u)-> t : before b ts
>           | selfSpacing u	-> t : before False ts
>         Special c
>           | c `elem` ",;([{"	-> t : before False ts
>         Keyword _		-> [ fromToken (TeX sub'space) | b ] ++ t : after ts
>         _			-> t : before True ts
> 
>     after []			=  []
>     after (t : ts)		=  case token t of
>         u | not (isNotSpace u)-> t : after ts
>           | selfSpacing u	-> t : before False ts
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

ks, 16.07.2003: Die Bedeutung von |auto| verstehe ich nicht so ganz:
Auch wenn |auto = False| wird der Stack auf dem laufenden gehalten.

ks, 16.07.2003: Ich versuche nun, das folgende relativ einfache
Einrueckungsverhalten zu implementieren -- aus meiner bisherigen Erfahrung
mit dem @poly@-style heraus habe ich den Eindruck, als muesste das
weitgehend genuegen. Ansonsten kann man ja immer noch explizit die
Einrueckung mit Annotationen formatieren.

> type Stack			=  [(Col, Line [Pos Token])]

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
>				=  loop True stack
>   where
>   copy d | auto		=  d
>          | otherwise		=  Empty

>   loop                        :: Bool -> Stack -> [Line [Pos Token]] -> (Doc, Stack)
>   loop first stack []	        =  (Empty, stack)  -- fertig
>   loop first stack (l:ls)     =  case l of
>       Blank                   -> loop True stack ls -- Leerzeilen ignorieren
>    {- Poly x | trace (show x) False -> undefined -}
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
>                                  in mkFromTo fstack rn n rc [fromToken $ TeX (indent (rn,rc) (n,c))] p ls
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
>   sep []			=  Empty
>   sep (Blank : _ )		=  sub'blankline
>   sep (_ : _)			=  sub'nl
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

F"ur inline-code.

> latexs			:: (CToken tok) => Formats -> [tok] -> Doc
> latexs dict			=  catenate . fmap (latex sub'space sub'space dict . token)
