%-------------------------------=  --------------------------------------------
\subsection{A Haskell lexer}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module HsLexer		(  module HsLexer ) --Token(..), isVarid, isConid, isNotSpace, string, tokenize  )
> where
> import Char hiding		(  lexLitChar  )
> import Monad
> import Document
> import Auxiliaries

%endif
Ein Haskell-Lexer. Modifikation der Prelude-Funktion \hs{lex}.

> data Token			=  Space String
>				|  Conid String
>				|  Varid String
>				|  Consym String
>				|  Varsym String
>				|  Numeral String
>				|  Char String
>				|  String String
>				|  Special Char
>				|  Comment String
>				|  Nested String
>				|  Keyword String
>				|  TeX Doc 		-- for inline \TeX
>				|  Qual String Token
>				|  Op Token
>				   deriving (Eq, Show)

> isVarid, isConid, isNotSpace	:: Token -> Bool
> isVarid (Varid _)		=  True
> isVarid (Qual _ t)		=  isVarid t
> isVarid _			=  False
>
> isConid (Conid s)		=  True
> isConid (Qual _ t)		=  isConid t
> isConid _			=  False
>
> isNotSpace (Space _)		=  False
> isNotSpace _			=  True

> string			:: Token -> String
> string (Space s)		=  s
> string (Conid s)		=  s
> string (Varid s)		=  s
> string (Consym s)		=  s
> string (Varsym s)		=  s
> string (Numeral s)		=  s
> string (Char s)		=  s
> string (String s)		=  s
> string (Special c)		=  [c]
> string (Comment s)		=  "--" ++ s
> string (Nested s)		=  "{-" ++ s ++ "-}"
> string (Keyword s)		=  s
> string (TeX (Text s))		=  "{-\"" ++ s ++ "\"-}"

This change is by ks, 14.05.2003, to make the @poly@ formatter work.
This should probably be either documented better or be removed again.

> string (TeX _)		=  "" -- |impossible "string"|
> string (Qual m s)		=  m ++ "." ++ string s
> string (Op s)			=  "`" ++ string s ++ "`"

> tokenize			:: [Char] -> Either Exc [Token]
> tokenize			=  lift tidyup @@ lift qualify @@ lexify

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 1}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

%{
%format lex' = "\Varid{lex}"

> lexify			:: [Char] -> Either Exc [Token]
> lexify []			=  return []
> lexify s@(_ : _)		=  case lex' s of
>     Nothing			-> Left ("lexical error", s)
>     Just (t, s')		-> do ts <- lexify s'; return (t : ts)
>
> lex'				:: String -> Maybe (Token, String)
> lex' ""			=  Nothing
> lex' ('\'' : s)		=  do let (t, u) = lexLitChar s
>				      v <- match "\'" u
>				      return (Char ("'" ++ t ++ "'"), v)
> lex' ('"' : s)		=  do let (t, u) = lexLitStr s
>				      v <- match "\"" u
>				      return (String ("\"" ++ t ++ "\""), v)
> lex' ('-' : '-' : s)		=  let (t, u) = break (== '\n') s
>				   in  return (Comment t, u)
> lex' ('{' : '-' : '"' : s)	=  do let (t, u) = inlineTeX s
>				      v <- match "\"-}" u
>				      return (TeX (Text t), v)
> lex' ('{' : '-' : s)		=  do let (t, u) = nested 0 s
>				      v <- match "-}" u
>				      return (Nested t, v)
> lex' (c : s)
>     | isSpace c		=  let (t, u) = span isSpace s in return (Space (c : t), u)
>     | isSpecial c		=  Just (Special c, s)
>     | isUpper c		=  let (t, u) = span isIdChar s in return (Conid (c : t), u)
>     | isLower c || c == '_'	=  let (t, u) = span isIdChar s in return (classify (c : t), u)
>     | c == ':'		=  let (t, u) = span isSymbol s in return (Consym (c : t), u)
>     | isSymbol c		=  let (t, u) = span isSymbol s in return (Varsym (c : t), u)
>     | isDigit c		=  do let (ds, t) = span isDigit s
>			              (fe, u)  <- lexFracExp t
>				      return (Numeral (c : ds ++ fe), u)
>     | otherwise		=  Nothing
>     where
>     classify s
>         | s `elem` keywords	=  Keyword s
>         | otherwise		=  Varid   s
>
>
> lexFracExp			:: String -> Maybe (String, String)
> lexFracExp s			=  do t <- match "." s
>				      (ds, u) <- lexDigits' t
>				      (e, v)  <- lexExp u
>				      return ('.' : ds ++ e, v)
>				`mplus` Just ("", s)
>
> lexExp			:: String -> Maybe (String, String)
> lexExp (e:s)
>      | e `elem` "eE" 		=  do (c : t) <- Just s
>				      if c `elem` "+-" then return () else Nothing
>				      (ds, u) <- lexDigits' t
>				      return (e : c : ds, u)
>				`mplus` do (ds, t) <- lexDigits' s
>				           return (e : ds, t)
> lexExp s			=  Just ("", s)
>
> lexDigits'			:: String -> Maybe (String, String)
> lexDigits' s			=  do (cs@(_ : _), t) <- Just (span isDigit s); return (cs, t)

%}

\NB `@'@' serves as an escape symbol in inline \TeX.

> inlineTeX			:: String -> (String, String)
> inlineTeX []			=  ([], [])
> inlineTeX ('\'' : 'n' : s)	=  '\n' <| inlineTeX s
> inlineTeX ('\'' : 'd' : s)	=  '"' <| inlineTeX s -- added 18.03.2001
> inlineTeX ('\'' : c : s)	=  c <| inlineTeX s
> inlineTeX ('"' : s)		=  ([], '"' : s)
> inlineTeX (c : s)		=  c <| inlineTeX s
>
> nested			:: Int -> String -> (String, String)
> nested _     []		=  ([], [])
> nested 0     ('-' : '}' : s)	=  ([], '-':'}':s)
> nested (n+1) ('-' : '}' : s)	=  '-' <| '}' <| nested n s
> nested n     ('{' : '-' : s)	=  '{' <| '-' <| nested (n + 1) s
> nested n     (c : s)		=  c <| nested n s

\NB GHC meldet bei |nested| f"alschlicherweise "`incomplete
patterns"'.

> lexLitChar, lexLitStr		:: String -> (String, String)
> lexLitChar []			=  ([], [])
> lexLitChar ('\'' : s)		=  ([], '\'' : s)
> lexLitChar ('\\' : c : s)	=  '\\' <| c <| lexLitChar s
> lexLitChar (c : s)		=  c <| lexLitChar s
>
> lexLitStr []			=  ([], [])
> lexLitStr ('"' : s)		=  ([], '"' : s)
> lexLitStr ('\\' : c : s)	=  '\\' <| c <| lexLitStr s
> lexLitStr (c : s)		=  c <| lexLitStr s

> isSpecial, isSymbol, isIdChar	:: Char -> Bool
> isSpecial c			=  c `elem` ",;()[]{}`"
> isSymbol c			=  c `elem` "!@#$%&*+./<=>?\\^|:-~"
> isIdChar c			=  isAlphaNum c || c `elem` "_'"

> match				:: String -> String -> Maybe String
> match p s
>     | p == t			=  Just u
>     | otherwise		=  Nothing
>     where (t, u)		=  splitAt (length p) s

\NB |match| entspricht eigentlich |lits|.

Schl"usselw"orter.

> keywords			:: [String]
> keywords			=  [ "case",     "class",    "data",  "default",
>				     "deriving", "do",       "else",  "if",
>				     "import",   "in",       "infix", "infixl",
>				     "infixr",   "instance", "let",   "module",
>				     "newtype",  "of",       "then",  "type",
>				     "where" ]

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 2}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Zusammenf"ugen von qualifizierten Namen.

> qualify			:: [Token] -> [Token]
> qualify []			=  []
> qualify (Conid m :  Varsym "." : t@(Conid i) : ts)
>				=  Qual m t : qualify ts
> qualify (Conid m :  Varsym "." : t@(Varid i) : ts)
>				=  Qual m t : qualify ts
> qualify (Conid m : Varsym ('.' : s@(':' : _)) : ts)
>				=  Qual m (Consym s) : qualify ts
> qualify (Conid m : Varsym ('.' : s) : ts)
>				=  Qual m (Varsym s) : qualify ts
> qualify (t : ts)		=  t : qualify ts

Backquoted ids zusammenfassen, da @`Prelude.div`@ zul"assig ist,
erst nach |qualify|.

> tidyup []			=  []
> tidyup (Special '`' : t@(Varid _) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Conid _) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Varid _)) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Conid _)) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (String s : ts)	=  strItems s ++ tidyup ts
> tidyup (Space s : ts)		=  splitSpace s ++ tidyup ts
> tidyup (t : ts)		=  t : tidyup ts

Beachte: @` div `@ wird nicht zusammengefa"st; damit wird eine
eventuelle Formatanweisung @%format `div` = ...@ ignoriert.

Breaking a string into string items.

> strItems []			=  impossible "strItems"
> strItems (c : s)		=  case breaks isGap s of
>     (item, '\\' : s')		-> String (c : item ++ "\\") : Space white : strItems rest
>         where (white, rest)	=  span isSpace s'
>     _				-> [String (c : s)]
>
> isGap				:: String -> Bool
> isGap ('\\' : c : s)		=  isSpace c
> isGap _			=  False

> splitSpace []			=  []
> splitSpace s			=  Space t : splitSpace u
>     where (t, u)		=  breakAfter (== '\n') s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A token class}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

We distinguish between white space, separators, delimiters and
non-separators.

> data CatCode			=  White
>				|  Sep
>				|  Del Char
>				|  NoSep
>				   deriving (Eq)

\Todo{Is the class |CToken| still necessary?}

> class CToken tok where
>     catCode			:: tok -> CatCode
>     token			:: tok -> Token
>     inherit			:: tok -> Token -> tok
>     fromToken			:: Token -> tok

|inherit old t| adds |old|'s attributes (eg positional information) to
|t|.

> instance CToken Token where
>     catCode (Space _)		=  White
>     catCode (Conid _)		=  NoSep
>     catCode (Varid _)		=  NoSep
>     catCode (Consym _)	=  Sep
>     catCode (Varsym _)	=  Sep
>     catCode (Numeral _)	=  NoSep
>     catCode (Char _)		=  NoSep
>     catCode (String _)	=  NoSep
>     catCode (Special c)
>         | c `elem` "([])"	=  Del c
>         | otherwise		=  Sep

\NB Only @([])@ are classified as delimiters; @{}@ are separators since
they do not bracket expressions.

>     catCode (Comment _)	=  White
>     catCode (Nested _)	=  White
>     catCode (Keyword _)	=  Sep
>     catCode (TeX (Text s))	=  White

The following change is by ks, 14.05.2003.
This is related to the change above in function |string|.

>     catCode (TeX _)		=  NoSep -- |impossible "catCode"|
>     catCode (Qual m t)	=  catCode t
>     catCode (Op _)		=  Sep
>     token			=  id
>     inherit _ t		=  t
>     fromToken			=  id

