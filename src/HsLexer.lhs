%-------------------------------=  --------------------------------------------
\subsection{A Haskell lexer}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> {-# LANGUAGE NPlusKPatterns #-}
> module HsLexer                (  module HsLexer ) --Token(..), isVarid, isConid, isNotSpace, string, tokenize  )
> where
> import Data.Char      (  isSpace, isUpper, isLower, isDigit, isAlphaNum, isPunctuation, toLower )
> import qualified Data.Char ( isSymbol )
> import Control.Applicative
> import Control.Monad
> import Document
> import Auxiliaries
> import TeXCommands    (  Lang(..)  )

%endif
A Haskell lexer, based on the Prelude function \hs{lex}.

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

> string (TeX _ _)              =  ""
> string (Qual m s)             =  concatMap (++".") m ++ string s
> string (Op s)                 =  "`" ++ string s ++ "`"

The main function.

> tokenize                      :: Lang -> String -> Either Exc [Token]
> tokenize lang                 =  lift tidyup <=< lift qualify <=< lexify lang

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 1}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

%{
%format lex' = "\Varid{lex}"

ks, 28.08.2008: New: Agda and Haskell modes.

> lexify                        :: Lang -> [Char] -> Either Exc [Token]
> lexify _lang []               =  return []
> lexify lang  s@(_ : _)        =  case lex' lang s of
>     Nothing                   -> Left ("lexical error", s)
>     Just (t, s')              -> do ts <- lexify lang s'; return (t : ts)
>
> lex'                          :: Lang -> String -> Maybe (Token, String)
> lex' _lang ""                 =  Nothing
> lex' _lang ('\'' : s)         =  do let (t, u) = lexLitChar s
>                                     v <- match "\'" u
>                                     return (Char ("'" ++ t ++ "'"), v)
> lex' _lang ('"' : s)          =  do let (t, u) = lexLitStr s
>                                     v <- match "\"" u
>                                     return (String ("\"" ++ t ++ "\""), v)
> lex' lang  ('-' : '-' : s)
>   | not (null s') && isSymbol lang (head s')
>                               =  case s' of
>                                    (c : s'') -> return (varsymid lang ("--" ++ d ++ [c]), s'')
>                                    [] -> impossible "lex'"
>   | otherwise                 =  return (Comment t, u)
>   where (d, s') = span (== '-') s
>         (t, u)  = break (== '\n') s'
> lex' _lang ('{' : '-' : '"' : s)
>                               =  do let (t, u) = inlineTeX s
>                                     v <- match "\"-}" u
>                                     return (TeX True (Text t), v)
> lex' _lang ('{' : '-' : '#' : s)
>                               =  do let (t, u) = nested 0 s
>                                     v <- match "#-}" u
>                                     return (Pragma t, v)
> lex' _lang ('{' : '-' : s)    =  do let (t, u) = nested 0 s
>                                     v <- match "-}" u
>                                     return (Nested t, v)
> lex' _lang ('0' : d : s)
>     -- octal and hexademical literals, see #74
>     -- Agda seems to support hex but not octal, but it seems harmless and easiest to me to
>     -- support this for both languages
>     | d `elem` "oO"           =  let (t, u) = span isOctDigit s in return (Numeral ('0' : d : t), u)
>     | d `elem` "xX"           =  let (t, u) = span isHexDigit s in return (Numeral ('0' : d : t), u)
> lex' lang  (c : s)
>     | isSpace c               =  let (t, u) = span isSpace s in return (Space (c : t), u)
>     | isSpecial lang c        =  Just (Special c, s)
>     | isUpper c               =  let (t, u) = span (isIdChar lang) s in return (Conid (c : t), u)
>     | isLower c || c == '_'   =  let (t, u) = span (isIdChar lang) s in return (classify (c : t), u)
>     | c == ':'                =  let (t, u) = span (isSymbol lang) s in return (consymid lang (c : t), u)
>     | isDigit c               =  do let (ds, t) = span isDigit s
>                                     (fe, u)  <- lexFracExp t
>                                     return (numeral lang (c : ds ++ fe), u)
>     | isSymbol lang c         =  let (t, u) = span (isSymbol lang) s in return (varsymid lang (c : t), u)
>     | otherwise               =  Nothing
>     where
>     numeral Agda              =  Varid
>     numeral Haskell           =  Numeral
>     classify s'
>         | s' `elem` keywords lang
>                               =  Keyword s'
>         | otherwise           =  Varid   s'
>
>
> lexFracExp                    :: String -> Maybe (String, String)
> lexFracExp s                  =   do t <- match "." s
>                                      (ds, u) <- lexDigits' t
>                                      (e, v)  <- lexExp u
>                                      return ('.' : ds ++ e, v)
>                               <|> lexExp s
>
> lexExp                        :: String -> Maybe (String, String)
> lexExp (e:s)
>      | e `elem` "eE"          =   do (c : t) <- Just s
>                                      unless (c `elem` "+-") Nothing
>                                      (ds, u) <- lexDigits' t
>                                      return (e : c : ds, u)
>                               <|> do (ds, t) <- lexDigits' s
>                                      return (e : ds, t)
> lexExp s                      =  Just ("", s)
>
> lexDigits'                    :: String -> Maybe (String, String)
> lexDigits' s                  =  do (cs@(_ : _), t) <- Just (span isDigit s); return (cs, t)

> varsymid :: Lang -> String -> Token
> varsymid Agda    = Varid
> varsymid Haskell = Varsym
>
> consymid :: Lang -> String -> Token
> consymid Agda    = Conid
> consymid Haskell = Consym

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

> isSpecial                     :: Lang -> Char -> Bool
> isIdChar, isSymbol            :: Lang -> Char -> Bool
> isSpecial Haskell c           =  c `elem` ",;()[]{}`"
> isSpecial Agda c              =  c `elem` ";(){}"
> isSymbol Haskell c            =  not (isSpecial Haskell c) && notElem c "'\"" &&
>                                  (c `elem` "!@#$%&*+./<=>?\\^|:-~" ||
>                                   Data.Char.isSymbol c || Data.Char.isPunctuation c)
> isSymbol Agda c               =  isIdChar Agda c
> isIdChar Haskell c            =  isAlphaNum c || c `elem` "_'"
> isIdChar Agda c               =  not (isSpecial Agda c || isSpace c)

> isOctDigit, isHexDigit        :: Char -> Bool
> isOctDigit c                  =  c `elem` "01234567"
> isHexDigit c                  =  isDigit c || toLower c `elem` "abcdef"

> match                         :: String -> String -> Maybe String
> match p s
>     | p == t                  =  Just u
>     | otherwise               =  Nothing
>     where (t, u)              =  splitAt (length p) s

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
>                                    "module", "data", "record", "infix",
>                                    "infixl", "infixr", "mutual", "abstract",
>                                    "private", "forall", "using", "hiding",
>                                    "renaming", "public" , "coinductive",  "constructor", "do",
>                                    "eta-equality", "inductive", "instance", "interleaved",
>                                    "macro", "no-eta-equality", "overlap", "pattern", "quote",
>                                    "quoteTerm", "rewrite", "syntax", "tactic", "unquote", 
>                                    "unquoteDecl", "unquoteDef", "variable" ]

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

Join backquoted ids -- because @`Prelude.div`@ is allowed,
we do this after |qualify|.

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

Note: @` div `@ is not joined; in such a case, a
potential format statement @%format `div` = ...@ is ignored.

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
>     catCode (Consym _)        =  Sep -- Sep is necessary for correct Haskell formatting
>     catCode (Varsym _)        =  Sep -- in Agda mode, Consym/Varsym don't occur
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

>     catCode (TeX _ _)         =  NoSep
>     catCode (Qual _ t)        =  catCode t
>     catCode (Op _)            =  Sep
>     token                     =  id
>     inherit _ t               =  t
>     fromToken                 =  id

