%-------------------------------=  --------------------------------------------
\subsection{Directives}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Directives             (  Formats, parseFormat, Equation, Substs, Subst, parseSubst, Toggles, eval, define, value, nrargs  )
> where
>
> import Data.Char              (  isSpace, isAlpha, isDigit  )
> import Control.Monad
> import Parser
> import TeXCommands
> import TeXParser
> import HsLexer
> import FiniteMap              (  FiniteMap  )
> import qualified FiniteMap as FM
> import FiniteMap ( (!) )
> import Auxiliaries
> import Document
> import Value

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{@%format@ directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Formats                  =  FiniteMap Char Equation
> type Equation                 =  (Bool, [Bool], [String], [Token])

ks, 20.07.03: The |Equation| type contains the following information:
does the definition have surrounding parentheses, do the arguments
have surrounding parentheses, what are the names of the arguments,
and the tokens to replace the macro with.

ks, 06.09.03: Adding the |nrargs| function that yields the number of
arguments a formatting directive expects.

> nrargs                        :: Equation -> Int
> nrargs (_,_,args,_)           =  length args

\NB Die Substution wird nicht als Funktion |[[Token]] -> [Token]|
repr"asentiert, da math |Pos Token| verlangt.

>
> parseFormat                   :: Lang -> String -> Either Exc (String, Equation)
> parseFormat lang s            =  parse lang (equation lang) (convert s)

Format directives. \NB @%format ( = "(\;"@ is legal.

> equation                      :: Lang -> Parser Token (String, Equation)
> equation lang                 =  do (opt, (f, opts, args)) <- optParen lhs
>                                     _ <- varsym lang "="
>                                     r <- many item
>                                     return (f, (opt, opts, args, r))
>                               `mplus` do f <- item
>                                          _ <- varsym lang "="
>                                          r <- many item
>                                          return (string f, (False, [], [], r))
>                               `mplus` do f <- satisfy isVarid `mplus` satisfy isConid
>                                          return (string f, (False, [], [], tex f))

\Todo{@%format `div1`@ funktioniert nicht.}

>     where
>     tex (Varid s)             =  subscript Varid s
>     tex (Conid s)             =  subscript Conid s
>     tex (Qual [] s)           =  tex s
>     tex (Qual (m:ms) s)       =  Conid m : tex (Qual ms s)
>      -- ks, 03.09.2003: was "tex (Qual m s) = Conid m : tex s"; 
>      -- seems strange though ...
>     subscript f s  
>       | null t && not (null w) && (null v || head w == '_')
>                               =  underscore f s
>       | otherwise             =  [f (reverse w)
>                                  , TeX False
>                                        (Text ((if   not (null v)
>                                                then "_{" ++ reverse v ++ "}" 
>                                                else ""
>                                               ) ++ reverse t))
>                                  ]
>         where s'              =  reverse s
>               (t, u)          =  span (== '\'') s'
>               (v, w)          =  span isDigit u

ks, 02.02.2004: I have added implicit formatting via |underscore|.
The above condition should guarantee that it is (almost) only used in 
cases where previously implicit formatting did not do anything useful.
The function |underscore| typesets an identifier such as
|a_b_c| as $a_{b_{c}}$. TODO: Instead of hard-coded subscripting a
substitution directive should be invoked here.

>     underscore f s
>                               =  [f t]
>                                  ++ if null u then []
>                                               else [TeX False (Text ("_{"))]
>                                                    ++
>                                                    proc_u
>                                                    ++
>                                                    [TeX False (Text ("}"))]
>         where (t, u)          =  break (== '_') s
>               tok_u           =  tokenize lang (tail u)
>               proc_u          =  case tok_u of
>                                    Left  _ -> [f (tail u)] -- should not happen
>                                    Right t -> t

> lhs                           :: Parser Token (String, [Bool], [String])
> lhs                           =  do f <- varid `mplus` conid
>                                     as <- many (optParen varid)
>                                     let (opts, args) = unzip as
>                                     return (f, opts, args)

> optParen                      :: Parser Token a -> Parser Token (Bool, a)
> optParen p                    =  do _ <- open'; a <- p; _ <- close'; return (True, a)
>                               `mplus` do a <- p ; return (False, a)
>
> item                          =  satisfy (\_ -> True)

> convert []                    =  []
> convert ('"' : '"' : s)       =  '"' : convert s
> convert ('"' : s)             =  '{' : '-' : '"' : convert' s
> convert (c : s)               =  c : convert s
>
> convert' []                   =  []
> convert' ('"' : '"' : s)      =  '"' : convert' s
> convert' ('"' : s)            =  '"' : '-' : '}' : convert s
> convert' (c : s)              =  c : convert' s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{@%subst@ directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Substs                   =  FiniteMap Char Subst
> type Subst                    =  [Doc] -> Doc

> parseSubst                    :: Lang -> String -> Either Exc (String, Subst)
> parseSubst lang s             =  parse lang (substitution lang) (convert s)
>
> substitution lang             =  do s <- varid
>                                     args <- many varid
>                                     _ <- varsym lang "="
>                                     rhs <- many (satisfy isVarid `mplus` satisfy isTeX)
>                                     return (s, subst args rhs)
>   where
>   subst args rhs ds           =  catenate (map sub rhs)
>       where sub (TeX _ d)     =  d
>             sub (Varid x)     =  FM.fromList (zip args ds) ! x

\Todo{unbound variables behandeln.}
ks, 24.10.2008: A bit messy: For Agda, we explicitly exclude "=" from the set
of varids accepted on the lhs of a directive, because according to the Agda
lexer, "=" is both a varid and a varsym. This shouldn't matter for Haskell,
because "=" will never occur in a Varid constructor.

> varid                         =  do x <- satisfy (\ x -> isVarid x && x /= (Varid "=")); return (string x)
> conid                         =  do x <- satisfy isConid; return (string x)
> varsym Agda s                 =  satisfy (\ x -> x == (Varsym s) || x == (Varid s)) -- Agda has no symbol/id distinction
> varsym Haskell s              =  satisfy (== (Varsym s))
>
> isTeX (TeX _ _)               =  True
> isTeX _                       =  False

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Conditional directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Toggles                  =  FiniteMap Char Value

Auswertung Boole'scher Ausdr"ucke.

> eval                          :: Lang -> Toggles -> String -> Either Exc Value
> eval lang togs                =  parse lang (expression lang togs)
>
> expression                    :: Lang -> Toggles -> Parser Token Value
> expression lang togs          =  expr
>   where
>   expr                        =  do e1 <- appl
>                                     e2 <- optional (do op <- varsym' lang; e <- expr; return (op, e))
>                                     return (maybe e1 (\(op, e2) -> sys2 op e1 e2) e2)
>   appl                        =  do f <- optional not'
>                                     e <- atom
>                                     return (maybe e (\_ -> onBool1 not e) f)
>   atom                        =  do Varid x <- satisfy isVarid; return (value togs x)
>                               `mplus` do _ <- true'; return (Bool True)
>                               `mplus` do _ <- false'; return (Bool False)
>                               `mplus` do s <- satisfy isString; return (Str (read (string s)))
>                               `mplus` do s <- satisfy isNumeral; return (Int (read (string s)))
>                               `mplus` do _ <- open'; e <- expr; _ <- close'; return e
>
> sys2 "&&"                     =  onBool2 (&&)
> sys2 "||"                     =  onBool2 (||)
> sys2 "=="                     =  onMatching (==) (==) (==)
> sys2 "/="                     =  onMatching (/=) (/=) (/=)
> sys2 "<"                      =  onMatching (<) (<) (<)
> sys2 "<="                     =  onMatching (<=) (<=) (<=)
> sys2 ">="                     =  onMatching (>=) (>=) (>=)
> sys2 ">"                      =  onMatching (>) (>) (>)
> sys2 "++"                     =  onStr2 (++)
> sys2 "+"                      =  onInt2 (+)
> sys2 "-"                      =  onInt2 (-)
> sys2 "*"                      =  onInt2 (*)
> sys2 "/"                      =  onInt2 (div)
> sys2 _                        =  \_ _ -> Undef

Definierende Gleichungen.

> define                        :: Lang -> Toggles -> String -> Either Exc (String, Value)
> define lang togs              =  parse lang (definition lang togs)
>
> definition                    :: Lang -> Toggles -> Parser Token (String, Value)
> definition lang togs          =  do Varid x <- satisfy isVarid
>                                     _ <- equal' lang
>                                     b <- expression lang togs
>                                     return (x, b)

Primitive Parser.

> not',  true', false', open', close'
>                               :: Parser Token Token
> equal'                        :: Lang -> Parser Token Token
> equal' lang                   =  varsym lang "="
> not'                          =  satisfy (== (Varid "not"))
> true'                         =  satisfy (== (Conid "True"))
> false'                        =  satisfy (== (Conid "False"))
> open'                         =  satisfy (== (Special '('))
> close'                        =  satisfy (== (Special ')'))

> varsym' lang                  =  do x <- satisfy (isVarsym lang); return (string x)
> isVarsym _ (Varsym _)         =  True
> isVarsym Agda (Varid _)       =  True  -- for Agda
> isVarsym _ _                  =  False
> isString (String _)           =  True
> isString _                    =  False
> isNumeral (Numeral _)         =  True
> isNumeral _                   =  False

Hilfsfunktionen.

> parse                         :: Lang -> Parser Token a -> [Char] -> Either Exc a
> parse lang p str              =  do ts <- tokenize lang str
>                                     let ts' = map (\t -> case t of TeX _ x -> TeX False x; _ -> t) .
>                                               filter (\t -> catCode t /= White || isTeX t) $ ts
>                                     maybe (Left msg) Right (run p ts')
>     where msg                 =  ("syntax error in directive", str)

Hack: |isTeX t| f"ur |parseSubst|.

> value                         :: Toggles -> String -> Value
> value togs x                  =  case FM.lookup x togs of
>     Nothing                   -> Undef
>     Just b                    -> b
