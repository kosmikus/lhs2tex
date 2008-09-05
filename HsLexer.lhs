%-------------------------------=  --------------------------------------------
\subsection{A Haskell lexer}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module HsLexer                (  module HsLexer ) --Token(..), isVarid, isConid, isNotSpace, string, tokenize  )
> where
> import Data.Char 	(  isSpace, isUpper, isLower, isDigit, isAlphaNum  )
> import qualified Data.Char ( isSymbol )
> import Control.Monad
> import Control.Monad.Error ()
> import Document
> import Auxiliaries
> import TeXCommands    (  Lang(..)  )

%endif
Ein Haskell-Lexer. Modifikation der Prelude-Funktion \hs{lex}.

> data Token                    =  Space String
>                               |  Conid String
>                               |  Varid String
>                               |  Consym String
>                               |  Varsym String
>                               |  Numeral String
>                               |  Char String
>                               |  String String
>                               |  Special Char
>                               |  Comment String
>                               |  Nested String
>                               |  Pragma String
>                               |  Keyword String
>                               |  TeX Bool Doc        -- for inline \TeX (True) and format replacements (False)
>                               |  Qual [String] Token
>                               |  Op Token
>                                  deriving (Eq, Show)

ks, 03.09.2003: Modified the |Qual| case to contain a list
of strings rather than a single string, to add support for
hierarchical modules. Also added Pragma.

> isVarid, isConid, isNotSpace  :: Token -> Bool
> isVarid (Varid _)             =  True
> isVarid (Qual _ t)            =  isVarid t
> isVarid _                     =  False
>
> isConid (Conid _)             =  True
> isConid (Qual _ t)            =  isConid t
> isConid _                     =  False
>
> isNotSpace (Space _)          =  False
> isNotSpace _                  =  True

> string                        :: Token -> String
> string (Space s)              =  s
> string (Conid s)              =  s
> string (Varid s)              =  s
> string (Consym s)             =  s
> string (Varsym s)             =  s
> string (Numeral s)            =  s
> string (Char s)               =  s
> string (String s)             =  s
> string (Special c)            =  [c]
> string (Comment s)            =  "--" ++ s
> string (Nested s)             =  "{-" ++ s ++ "-}"
> string (Pragma s)             =  "{-#" ++ s ++ "#-}"
> string (Keyword s)            =  s
> string (TeX True (Text s))    =  "{-\"" ++ s ++ "\"-}"
> string (TeX False (Text s))   =  "\"" ++ s ++ "\""

This change is by ks, 14.05.2003, to make the @poly@ formatter work.
This should probably be either documented better or be removed again.

> string (TeX _ _)              =  "" -- |impossible "string"|
> string (Qual m s)             =  concatMap (++".") m ++ string s
> string (Op s)                 =  "`" ++ string s ++ "`"

The main function.

> tokenize                      :: Lang -> [Char] -> Either Exc [Token]
> tokenize lang                 =  lift tidyup @@ lift qualify @@ lexify lang

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 1}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

%{
%format lex' = "\Varid{lex}"

ks, 28.08.2008: New: Agda and Haskell modes.

> lexify                        :: Lang -> [Char] -> Either Exc [Token]
> lexify lang []                =  return []
> lexify lang s@(_ : _)         =  case lex' lang s of
>     Nothing                   -> Left ("lexical error", s)
>     Just (t, s')              -> do ts <- lexify lang s'; return (t : ts)
>
> lex'                          :: Lang -> String -> Maybe (Token, String)
> lex' lang ""                  =  Nothing
> lex' lang ('\'' : s)          =  do let (t, u) = lexLitChar s
>                                     v <- match "\'" u
>                                     return (Char ("'" ++ t ++ "'"), v)
> lex' lang ('"' : s)           =  do let (t, u) = lexLitStr s
>                                     v <- match "\"" u
>                                     return (String ("\"" ++ t ++ "\""), v)
> lex' lang ('-' : '-' : s)
>   | not (null s') && isSymbol lang (head s')
>                               =  case s' of
>                                    (c : s'') -> return (Varsym ("--" ++ d ++ [c]), s'')
>   | otherwise                 =  return (Comment t, u)
>   where (d, s') = span (== '-') s
>         (t, u)  = break (== '\n') s'
> lex' lang ('{' : '-' : '"' : s) 
>                               =  do let (t, u) = inlineTeX s
>                                     v <- match "\"-}" u
>                                     return (TeX True (Text t), v)
> lex' lang ('{' : '-' : '#' : s)
>                               =  do let (t, u) = nested 0 s
>                                     v <- match "#-}" u
>                                     return (Pragma t, v)
> lex' lang ('{' : '-' : s)     =  do let (t, u) = nested 0 s
>                                     v <- match "-}" u
>                                     return (Nested t, v)
> lex' lang (c : s)
>     | isSpace c               =  let (t, u) = span isSpace s in return (Space (c : t), u)
>     | isSpecial c             =  Just (Special c, s)
>     | isUpper c               =  let (t, u) = span (isIdChar lang) s in return (Conid (c : t), u)
>     | isLower c || c == '_'   =  let (t, u) = span (isIdChar lang) s in return (classify (c : t), u)
>     | c == ':'                =  let (t, u) = span (isSymbol lang) s in return (Consym (c : t), u)
>     | isSymbol lang c         =  let (t, u) = span (isSymbol lang) s in return (Varsym (c : t), u)
>     | isDigit c               =  do let (ds, t) = span isDigit s
>                                     (fe, u)  <- lexFracExp t
>                                     return (Numeral (c : ds ++ fe), u)
>     | otherwise               =  Nothing
>     where
>     classify s
>         | s `elem` keywords lang
>                               =  Keyword s
>         | otherwise           =  Varid   s
>
>
> lexFracExp                    :: String -> Maybe (String, String)
> lexFracExp s                  =  do t <- match "." s
>                                     (ds, u) <- lexDigits' t
>                                     (e, v)  <- lexExp u
>                                     return ('.' : ds ++ e, v)
>                               `mplus` Just ("", s)
>
> lexExp                        :: String -> Maybe (String, String)
> lexExp (e:s)
>      | e `elem` "eE"          =  do (c : t) <- Just s
>                                     if c `elem` "+-" then return () else Nothing
>                                     (ds, u) <- lexDigits' t
>                                     return (e : c : ds, u)
>                               `mplus` do (ds, t) <- lexDigits' s
>                                          return (e : ds, t)
> lexExp s                      =  Just ("", s)
>
> lexDigits'                    :: String -> Maybe (String, String)
> lexDigits' s                  =  do (cs@(_ : _), t) <- Just (span isDigit s); return (cs, t)

%}

\NB `@'@' serves as an escape symbol in inline \TeX.

> inlineTeX                     :: String -> (String, String)
> inlineTeX []                  =  ([], [])
> inlineTeX ('\'' : 'n' : s)    =  '\n' <| inlineTeX s
> inlineTeX ('\'' : 'd' : s)    =  '"' <| inlineTeX s -- added 18.03.2001
> inlineTeX ('\'' : c : s)      =  c <| inlineTeX s
> inlineTeX ('"' : s)           =  ([], '"' : s)
> inlineTeX (c : s)             =  c <| inlineTeX s
>
> nested                        :: Int -> String -> (String, String)
> nested _     []               =  ([], [])
> nested 0     ('#' : '-' : '}' : s)
>                               =  ([], '#':'-':'}':s)
> nested 0     ('-' : '}' : s)  =  ([], '-':'}':s)
> nested (n+1) ('-' : '}' : s)  =  '-' <| '}' <| nested n s
> nested n     ('{' : '-' : s)  =  '{' <| '-' <| nested (n + 1) s
> nested n     (c : s)          =  c <| nested n s

ks, 03.09.2003: The above definition of nested will actually
incorrectly reject programs that contain comments like the
following one: {- start normal, but close as pragma #-} ...
I don't expect this to be a problem, though.

\NB GHC meldet bei |nested| f"alschlicherweise "`incomplete
patterns"'. [ks: This is no longer true (with GHC 5.04.3).]

> lexLitChar, lexLitStr         :: String -> (String, String)
> lexLitChar []                 =  ([], [])
> lexLitChar ('\'' : s)         =  ([], '\'' : s)
> lexLitChar ('\\' : c : s)     =  '\\' <| c <| lexLitChar s
> lexLitChar (c : s)            =  c <| lexLitChar s
>
> lexLitStr []                  =  ([], [])
> lexLitStr ('"' : s)           =  ([], '"' : s)
> lexLitStr ('\\' : c : s)      =  '\\' <| c <| lexLitStr s
> lexLitStr (c : s)             =  c <| lexLitStr s

> isSpecial                     :: Char -> Bool
> isIdChar, isSymbol            :: Lang -> Char -> Bool
> isSpecial c                   =  c `elem` ",;()[]{}`"
> isSymbol Haskell c            =  c `elem` "!@#$%&*+./<=>?\\^|:-~" || Data.Char.isSymbol c
> isSymbol Agda c               =  isIdChar Agda c
> isIdChar Haskell c            =  isAlphaNum c || c `elem` "_'"
> isIdChar Agda c               =  not (isSpecial c || isSpace c)

> match                         :: String -> String -> Maybe String
> match p s
>     | p == t                  =  Just u
>     | otherwise               =  Nothing
>     where (t, u)              =  splitAt (length p) s

\NB |match| entspricht eigentlich |lits|.

Keywords

> keywords                      :: Lang -> [String]
> keywords Haskell              =  [ "case",     "class",    "data",  "default",
>                                    "deriving", "do",       "else",  "if",
>                                    "import",   "in",       "infix", "infixl",
>                                    "infixr",   "instance", "let",   "module",
>                                    "newtype",  "of",       "then",  "type",
>                                    "where" ]
> keywords Agda                 =  [ "let", "in", "where", "field", "with",
>                                    "postulate", "primitive", "open", "import",
>                                    "module", "data", "codata", "record", "infix",
>                                    "infixl", "infixr", "mutual", "abstract",
>                                    "private", "forall", "using", "hiding",
>                                    "renaming", "public" ]

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 2}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Merging qualified names.

ks, 27.06.2003: I have modified the fifth case of |qualify|
to only match if the |Varsym| contains at least one symbol
besides the dot. Otherwise the dot is an operator, not part
of a qualified name.
ks, 03.09.2003: Deal with hierarchical module names. This could
be made more efficient if that seems necessary.

> qualify                       :: [Token] -> [Token]
> qualify []                    =  []
> qualify (Conid m :  Varsym "." : t@(Conid _) : ts)
>                               =  qualify (Qual [m] t : ts)
> qualify (Conid m :  Varsym "." : t@(Varid _) : ts)
>                               =  Qual [m] t : qualify ts
> qualify (Conid m : Varsym ('.' : s@(':' : _)) : ts)
>                               =  Qual [m] (Consym s) : qualify ts
> qualify (Conid m : Varsym ('.' : s@(_ : _)) : ts)
>                               =  Qual [m] (Varsym s) : qualify ts
> qualify (Qual m (Conid m') : Varsym "." : t@(Conid _) : ts)
>                               =  qualify (Qual (m ++ [m']) t : qualify ts)
> qualify (Qual m (Conid m') : Varsym "." : t@(Varid _) : ts)
>                               =  Qual (m ++ [m']) t : qualify ts
> qualify (Qual m (Conid m') : Varsym ('.' : s@(':' : _)) : ts)
>                               =  Qual (m ++ [m']) (Consym s) : qualify ts
> qualify (Qual m (Conid m') : Varsym ('.' : s@(_ : _)) : ts)
>                               =  Qual (m ++ [m']) (Varsym s) : qualify ts
> qualify (t : ts)              =  t : qualify ts

Backquoted ids zusammenfassen, da @`Prelude.div`@ zul"assig ist,
erst nach |qualify|.

> tidyup                        :: [Token] -> [Token]
> tidyup []                     =  []
> tidyup (Special '`' : t@(Varid _) : Special '`' : ts)
>                               =  Op t : tidyup ts
> tidyup (Special '`' : t@(Conid _) : Special '`' : ts)
>                               =  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Varid _)) : Special '`' : ts)
>                               =  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Conid _)) : Special '`' : ts)
>                               =  Op t : tidyup ts
> tidyup (String s : ts)        =  strItems s ++ tidyup ts
> tidyup (Space s : ts)         =  splitSpace s ++ tidyup ts
> tidyup (t : ts)               =  t : tidyup ts

Beachte: @` div `@ wird nicht zusammengefa"st; damit wird eine
eventuelle Formatanweisung @%format `div` = ...@ ignoriert.

Breaking a string into string items.

> strItems                      :: String -> [Token]
> strItems []                   =  impossible "strItems"
> strItems (c : s)              =  case breaks isGap s of
>     (item, '\\' : s')         -> String (c : item ++ "\\") : Space white : strItems rest
>         where (white, rest)   =  span isSpace s'
>     _                         -> [String (c : s)]
>
> isGap                         :: String -> Bool
> isGap ('\\' : '\n' : _)       =  True
> isGap _                       =  False

ks, 12.01.2004: changed the definition of |isGap| to be |True|
only if the character following a backslash is a newline. Otherwise,
the sequence |"\\ "| will be incorrectly treated as a string gap.
I am not convinced that the special treatment of string gaps is a
good thing at all. String gaps don't work in newcode style, as it
is right now.

> splitSpace                    :: String -> [Token]
> splitSpace []                 =  []
> splitSpace s                  =  Space t : splitSpace u
>     where (t, u)              =  breakAfter (== '\n') s

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{A token class}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

We distinguish between white space, separators, delimiters and
non-separators.

> data CatCode                  =  White
>                               |  Sep
>                               |  Del Char
>                               |  NoSep
>                                  deriving (Eq)

\Todo{Is the class |CToken| still necessary?}

> class CToken tok where
>     catCode                   :: tok -> CatCode
>     token                     :: tok -> Token
>     inherit                   :: tok -> Token -> tok
>     fromToken                 :: Token -> tok

|inherit old t| adds |old|'s attributes (eg positional information) to
|t|.

ks, 29.08.2008: Made the non-backwards compatible change to add curly
braces to the set of parentheses. I did this because it's necessary for
Agda, but I also never understood why it isn't the case for Haskell.
This affects spacing of some constructs in Haskell mode, but I think it's
an improvement.

> instance CToken Token where
>     catCode (Space _)         =  White
>     catCode (Conid _)         =  NoSep
>     catCode (Varid _)         =  NoSep
>     catCode (Consym _)        =  NoSep -- !
>     catCode (Varsym _)        =  NoSep -- !
>     catCode (Numeral _)       =  NoSep
>     catCode (Char _)          =  NoSep
>     catCode (String _)        =  NoSep
>     catCode (Special c)
>         | c `elem` "([{}])"   =  Del c
>         | otherwise           =  Sep

\NB Only @([])@ are classified as delimiters; @{}@ are separators since
they do not bracket expressions.

>     catCode (Comment _)       =  White
>     catCode (Nested _)        =  White
>     catCode (Pragma _)        =  White
>     catCode (Keyword _)       =  Sep
>     catCode (TeX _ (Text _))  =  White

The following change is by ks, 14.05.2003.
This is related to the change above in function |string|.

>     catCode (TeX _ _)         =  NoSep -- |impossible "catCode"|
>     catCode (Qual _ t)        =  catCode t
>     catCode (Op _)            =  Sep
>     token                     =  id
>     inherit _ t               =  t
>     fromToken                 =  id

