%-------------------------------=  --------------------------------------------
\subsection{Directives}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Directives		(  Formats, parseFormat, Equation, Substs, Subst, parseSubst, Toggles, eval, define, value  )
> where
>
> import Char			(  isSpace, isAlpha, isDigit  )
> import Monad
> import Parser
> import TeXCommands
> import TeXParser
> import HsLexer
> import FiniteMap		(  FiniteMap  )
> import qualified FiniteMap as FM
> import FiniteMap ( (!) )
> import Auxiliaries
> import Document
> import Value

%endif

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{@%format@ directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Formats			=  FiniteMap Char Equation
> type Equation			=  (Bool, [Bool], [String], [Token])

\NB Die Substution wird nicht als Funktion |[[Token]] -> [Token]|
repr"asentiert, da math |Pos Token| verlangt.

>
> parseFormat 			:: String -> Either Exc (String, Equation)
> parseFormat s			=  parse equation (convert s)

Formatanweisungen. \NB @%format ( = "(\;"@ is legal.

> equation			:: Parser Token (String, Equation)
> equation			=  do (opt, (f, opts, args)) <- optParen lhs
>				      _ <- varsym "="
>				      r <- many item
>				      return (f, (opt, opts, args, r))
>				`mplus` do f <- item
>				           _ <- varsym "="
>				           r <- many item
>				           return (string f, (False, [], [], r))
>				`mplus` do f <- satisfy isVarid `mplus` satisfy isConid
>				           return (string f, (False, [], [], tex f))

\Todo{@%format `div1`@ funktioniert nicht.}

>     where
>     tex (Varid s)		=  subscript Varid s
>     tex (Conid s)		=  subscript Conid s
>     tex (Qual m s)		=  Conid m : tex s
>     subscript f s		=  [f (reverse w), TeX (Text ("_{" ++ reverse v ++ "}" ++ reverse t))]
>         where s'		=  reverse s
>               (t, u)		=  span (== '\'') s'
>               (v, w)		=  span isDigit u

> lhs				:: Parser Token (String, [Bool], [String])
> lhs				=  do f <- varid `mplus` conid
>				      as <- many (optParen varid)
>				      let (opts, args) = unzip as
>				      return (f, opts, args)

> optParen			:: Parser Token a -> Parser Token (Bool, a)
> optParen p			=  do _ <- open'; a <- p; _ <- close'; return (True, a)
>				`mplus` do a <- p ; return (False, a)
>
> item				=  satisfy (\_ -> True)

> convert []			=  []
> convert ('"' : '"' : s)	=  '"' : convert s
> convert ('"' : s)		=  '{' : '-' : '"' : convert' s
> convert (c : s)		=  c : convert s
>
> convert' []			=  []
> convert' ('"' : '"' : s)	=  '"' : convert' s
> convert' ('"' : s)		=  '"' : '-' : '}' : convert s
> convert' (c : s)		=  c : convert' s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{@%subst@ directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Substs			=  FiniteMap Char Subst
> type Subst			=  [Doc] -> Doc

> parseSubst 			:: String -> Either Exc (String, Subst)
> parseSubst s			=  parse substitution (convert s)
>
> substitution			=  do s <- varid
>				      args <- many varid
>				      _ <- varsym "="
>				      rhs <- many (satisfy isVarid `mplus` satisfy isTeX)
>				      return (s, subst args rhs)
>   where
>   subst args rhs ds		=  catenate (map sub rhs)
>       where sub (TeX d)	=  d
>             sub (Varid x)	=  FM.fromList (zip args ds) ! x

\Todo{unbound variables behandeln.}

> varid				=  do x <- satisfy isVarid; return (string x)
> conid				=  do x <- satisfy isConid; return (string x)
> varsym s			=  satisfy (== (Varsym s))
>
> isTeX (TeX _)			=  True
> isTeX _			=  False

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Conditional directives}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> type Toggles			=  FiniteMap Char Value

Auswertung Boole'scher Ausdr"ucke.

> eval				:: Toggles -> String -> Either Exc Value
> eval togs			=  parse (expression togs)
>
> expression			:: Toggles -> Parser Token Value
> expression togs		=  expr
>   where
>   expr			=  do e1 <- appl
>				      e2 <- optional (do op <- varsym'; e <- expr; return (op, e))
>				      return (maybe e1 (\(op, e2) -> sys2 op e1 e2) e2)
>   appl			=  do f <- optional not'
>				      e <- atom
>				      return (maybe e (\_ -> onBool1 not e) f)
>   atom			=  do Varid x <- satisfy isVarid; return (value togs x)
>				`mplus` do _ <- true'; return (Bool True)
>				`mplus` do _ <- false'; return (Bool False)
>				`mplus` do s <- satisfy isString; return (Str (read (string s)))
>				`mplus` do s <- satisfy isNumeral; return (Int (read (string s)))
>				`mplus` do _ <- open'; e <- expr; _ <- close'; return e
>
> sys2 "&&"			=  onBool2 (&&)
> sys2 "||"			=  onBool2 (||)
> sys2 "=="			=  onMatching (==) (==) (==)
> sys2 "/="			=  onMatching (/=) (/=) (/=)
> sys2 "<"			=  onMatching (<) (<) (<)
> sys2 "<="			=  onMatching (<=) (<=) (<=)
> sys2 ">="			=  onMatching (>=) (>=) (>=)
> sys2 ">"			=  onMatching (>) (>) (>)
> sys2 "++"			=  onStr2 (++)
> sys2 "+"			=  onInt2 (+)
> sys2 "-"			=  onInt2 (-)
> sys2 "*"			=  onInt2 (*)
> sys2 "/"			=  onInt2 (div)
> sys2 _			=  \_ _ -> Undef

Definierende Gleichungen.

> define			:: Toggles -> String -> Either Exc (String, Value)
> define togs			=  parse (definition togs)
>
> definition			:: Toggles -> Parser Token (String, Value)
> definition togs		=  do Varid x <- satisfy isVarid
>				      _ <- equal'
>				      b <- expression togs
>				      return (x, b)

Primitive Parser.

> equal', not',  true', false', open', close'
>	   			:: Parser Token Token
> equal'			=  satisfy (== (Varsym "="))
> not'				=  satisfy (== (Varid "not"))
> true'				=  satisfy (== (Conid "True"))
> false'			=  satisfy (== (Conid "False"))
> open'				=  satisfy (== (Special '('))
> close'			=  satisfy (== (Special ')'))

> varsym'			=  do x <- satisfy isVarsym; return (string x)
> isVarsym (Varsym _)		=  True
> isVarsym _			=  False
> isString (String _)		=  True
> isString _			=  False
> isNumeral (Numeral _)		=  True
> isNumeral _			=  False

Hilfsfunktionen.

> parse				:: Parser Token a -> [Char] -> Either Exc a
> parse p str			=  do ts <- tokenize str
>				      let ts' = filter (\t -> catCode t /= White || isTeX t) ts
>				      maybe (Left msg) Right (run p ts')
>     where msg			=  ("syntax error in directive", str)

Hack: |isTeX t| f"ur |parseSubst|.

> value				:: Toggles -> String -> Value
> value togs x			=  case FM.lookup x togs of
>     Nothing			-> Undef
>     Just b			-> b
