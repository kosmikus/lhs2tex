module Lhs2TeX.HsLexer
  (Lang(..), Token(..), TeXStyle(..), tokenize)
  where

import Control.Monad
import Control.Monad.Error
import qualified Data.Char as C
import Data.List

import Lhs2TeX.Document
import Lhs2TeX.Utils
import Lhs2TeX.Exception

-- | The languages that can be handled by the lexer. Currently only
-- Haskell and Agda.
data Lang = Haskell | Agda
  deriving (Eq, Show, Enum, Bounded)

-- | The type of tokens contains mainly the classes we'd expect to
-- occur in a Haskell program. We keep spaces, comments, pragmas, and
-- we add TeX tokens.
data Token =
    Space   String
  | Conid   String
  | Varid   String
  | Consym  String
  | Varsym  String
  | Numeral String
  | Char    String
  | String  String
  | Special Char
  | Comment String
  | Nested  String
  | Pragma  String
  | Keyword String
  | TeX     TeXStyle Doc
  | Qual    [String] Token  -- ^ for qualified names
  | Op      Token
  deriving (Eq, Show)

data TeXStyle = InlineTeX | FormatReplacement
  deriving (Eq, Show)

-- | In Agda, there is no difference between symbols and identifiers.
varsymid :: Lang -> String -> Token
varsymid Agda    = Varid
varsymid Haskell = Varsym

-- | In Agda, there is no difference between symbols and identifiers.
consymid :: Lang -> String -> Token
consymid Agda    = Conid
consymid Haskell = Consym

-- | In Agda, even numeric literals are just identifiers.
numeral :: Lang -> String -> Token
numeral Agda     = Varid
numeral Haskell  = Numeral


-- | Checks if the token is a (qualified) variable identifier.
isVarid :: Token -> Bool
isVarid (Varid _)     =  True
isVarid (Qual _ t)    =  isVarid t
isVarid _             =  False

-- | Checks if the token is a (qualified) constructor identifier.
isConid :: Token -> Bool
isConid (Conid _)     =  True
isConid (Qual _ t)    =  isConid t
isConid _             =  False

-- | Checks if the token is not s space.
isNotSpace :: Token -> Bool
isNotSpace (Space _)  =  False
isNotSpace _          =  True

-- | Checks if a character is special. Special characters are mainly those
-- that affect grouping.
isSpecial :: Char -> Bool
isSpecial = (`elem` ",;()[]{}`")

-- | Checks if a character is a symbol. For Agda, we make no difference
-- between identifiers and symbols and therefore count all identifier
-- characters also as symbols.
isSymbol :: Lang -> Char -> Bool
isSymbol Haskell  c  =  not (isSpecial c) && notElem c "'\"" &&
                        (c `elem` "!@#$%&*+./<=>?\\^|:-~" ||
                          C.isSymbol c || C.isPunctuation c)
isSymbol Agda     c  =  isIdChar Agda c

-- | Checks if the character is allowed to appear in an identifier name.
-- Agda is more liberal than Haskell here.
isIdChar :: Lang -> Char -> Bool
isIdChar Haskell  c  =  C.isAlphaNum c || c `elem` "_'"
isIdChar Agda     c  =  not (isSpecial c || C.isSpace c)

-- Keywords per language.
keywords :: Lang -> [String]
keywords Haskell =
  [ "case",     "class",    "data",  "default",
    "deriving", "do",       "else",  "if",
    "import",   "in",       "infix", "infixl",
    "infixr",   "instance", "let",   "module",
    "newtype",  "of",       "then",  "type",
    "where" ]
keywords Agda    =
  [ "let", "in", "where", "field", "with",
    "postulate", "primitive", "open", "import",
    "module", "data", "codata", "record", "infix",
    "infixl", "infixr", "mutual", "abstract",
    "private", "forall", "using", "hiding",
    "renaming", "public" ]

-- | Turns a token into a string in a relatively straight-forward way.
string :: Token -> String
string (Space s)    =  s
string (Conid s)    =  s
string (Varid s)    =  s
string (Consym s)   =  s
string (Varsym s)   =  s
string (Numeral s)  =  s
string (Char s)     =  s
string (String s)   =  s
string (Special c)  =  [c]
string (Comment s)  =  "--" ++ s
string (Nested s)   =  "{-" ++ s ++ "-}"
string (Pragma s)   =  "{-#" ++ s ++ "#-}"
string (Keyword s)  =  s
string (TeX InlineTeX          (Text s))
                    =  "{-\"" ++ s ++ "\"-}"
string (TeX FormatReplacement  (Text s))
                    =  "\"" ++ s ++ "\""
string (TeX _ _)    =  "" -- This used to be impossible, but the poly
                          -- formatter can create tokens of this form.
string (Qual m s)   =  concatMap (++".") m ++ string s
string (Op s)       =  "`" ++ string s ++ "`"

-- | Tokenizes a string. Takes the language as a parameter.
tokenize :: Lang -> String -> Either Exc [Token]
tokenize lang xs = liftM (tidyup . qualify) (lexify lang xs)

-- | The main tokenizer.
lexify :: Lang -> String -> Either Exc [Token]
lexify lang []  =  return []
lexify lang s   =
  case lexSingle lang s of
    Nothing       ->  Left ("lexical error", s)
    Just (t, s')  -> do ts <- lexify lang s'; return (t : ts)

-- | Splits off a single token.
lexSingle :: Lang -> String -> Maybe (Token, String)
lexSingle lang xs =
  case xs of
    ""            ->  Nothing
    '\'' : s      ->  let (t, u) = lexLitChar s
                      in  liftM ((,) (Char   ("'"  ++ t ++ "'" ))) (match "'"  u)
    '"'  : s      ->  let (t, u) = lexLitStr  s
                      in  liftM ((,) (String ("\"" ++ t ++ "\""))) (match "\"" u)
    '-' : '-' : s 
      | not (null s') && isSymbol lang (head s')  -- not a comment after all
                  ->  case s' of
                        (c : s'') ->
                          return (varsymid lang ("--" ++ d ++ [c]), s'')
      | otherwise ->  return (Comment t, u)
      where
        (d, s')  =  span   (== '-' )  s
        (t, u)   =  break  (== '\n')  s'
    '{' : '-' : '"' : s
                  ->  let (t, u) = inlineTeX s
                      in  liftM ((,) (TeX InlineTeX (Text t))) (match "\"-}" u)
    '{' : '-' : '#' : s
                  ->  let (t, u) = nested 0 s
                      in  liftM ((,) (Pragma t))               (match "#-}"  u)
    '{' : '-' : s ->  let (t, u) = nested 0 s
                      in  liftM ((,) (Nested t))               (match "-}"   u)
    c : s
      | C.isSpace c
                  ->  let (t, u) = span C.isSpace s
                      in  return (Space (c : t), u)
      | isSpecial c
                  ->  return (Special c, s)
      | C.isUpper c
                  ->  let (t, u) = span (isIdChar lang) s
                      in  return (Conid (c : t), u)
      | C.isLower c || c == '_'
                  ->  let (t, u) = span (isIdChar lang) s
                      in  return (classify (c : t), u)
      | c == ':'  ->  let (t, u) = span (isSymbol lang) s
                      in  return (consymid lang (c : t), u)
      | C.isDigit c
                  ->  let (ds, t) = span C.isDigit s
                      in  do  (fe, u) <- lexFracExp t
                              return (numeral lang (c : ds ++ fe), u)
      | isSymbol lang c
                  ->  let (t, u) = span (isSymbol lang) s
                      in  return (varsymid lang (c : t), u)
      | otherwise ->  mzero
  where
    classify s
      | s `elem` keywords lang  =  Keyword s
      | otherwise               =  Varid   s

-- | Lex the fractional part of a floating point literal.
lexFracExp :: String -> Maybe (String, String)
lexFracExp s =
  do
    t       <- match "." s
    (ds, u) <- lexDigits t
    (e, v)  <- lexExp u
    return ('.' : ds ++ e, v)
  `mplus` return ("", s)

-- | Lex the exponent of a floating point literal.
lexExp :: String -> Maybe (String, String)
lexExp (e:s)
  | e `elem` "eE"  =  do
                        (c : t) <- Just s
                        unless (c `elem` "+-") Nothing
                        (ds, u) <- lexDigits t
                        return (e : c : ds, u)
                      `mplus`
                      do
                        (ds, t) <- lexDigits s
                        return (e : ds, t)
lexExp s           =  Just ("", s)

-- | Lex as many digits as possible.
lexDigits :: String -> Maybe (String, String)
lexDigits s =
  do
    (cs@(_ : _), t) <- Just (span C.isDigit s)
    return (cs, t)

-- | Lex inline TeX. A double quote serves as termination character
-- for inline TeX. A single quote serves as escape character for
-- inline TeX. 
inlineTeX :: String -> (String, String)
inlineTeX []                =  ([], [])
inlineTeX ('\'' : 'n' : s)  =  '\n' <| inlineTeX s
inlineTeX ('\'' : 'd' : s)  =  '"' <| inlineTeX s -- added 18.03.2001
inlineTeX ('\'' : c : s)    =  c <| inlineTeX s
inlineTeX ('"' : s)         =  ([], '"' : s)
inlineTeX (c : s)           =  c <| inlineTeX s

-- | Lexer for nested comments and pragmas. We keep track of the
-- current level of nesting.
nested :: Int -> String -> (String, String)
nested _     []                     =  ([], [])
nested 0     ('#' : '-' : '}' : s)  =  ([], '#':'-':'}':s)
nested 0     ('-' : '}' : s)        =  ([], '-':'}':s)
nested (n+1) ('-' : '}' : s)        =  '-' <| '}' <| nested n s
nested n     ('{' : '-' : s)        =  '{' <| '-' <| nested (n + 1) s
nested n     (c : s)                =  c <| nested n s
-- ks, 03.09.2003: Known bug: The above definition of nested will
-- actually incorrectly reject programs that contain comments like
-- the following one: {- start normal, but close as pragma #-} ...
-- I don't expect this to be a problem, though.

-- | Lexes a character literal.
lexLitChar :: String -> (String, String)
lexLitChar []              =  ([], [])
lexLitChar ('\'' : s)      =  ([], '\'' : s)
lexLitChar ('\\' : c : s)  =  '\\' <| c <| lexLitChar s
lexLitChar (c : s)         =  c <| lexLitChar s

-- | Lexes a string literal.
lexLitStr :: String -> (String, String)
lexLitStr []               =  ([], [])
lexLitStr ('"' : s)        =  ([], '"' : s)
lexLitStr ('\\' : c : s)   =  '\\' <| c <| lexLitStr s
lexLitStr (c : s)          =  c <| lexLitStr s

-- | Takes a pattern `p' and a string `s'. Returns the part of `s'
-- that follows the first occurrence of `p'. Fails with `Nothing'
-- if that isn't possible.
match :: String -> String -> Maybe String
match p s
  | p == t     =  Just u
  | otherwise  =  Nothing
  where
    (t, u) = splitAt (length p) s

-- | Merges qualified names. Qualified names are first parsed as a sequence
-- of tokens. If they happen to be valid qualified identifier (or symbol) names,
-- they are merged after lexing using this function.
qualify :: [Token] -> [Token]
qualify []  =  []
qualify (Conid m :  Varsym "." : t@(Conid _) : ts)
                  =  qualify (Qual [m] t : ts)
qualify (Conid m :  Varsym "." : t@(Varid _) : ts)
                  =  Qual [m] t : qualify ts
qualify (Conid m : Varsym ('.' : s@(':' : _)) : ts)
                  =  Qual [m] (Consym s) : qualify ts
qualify (Conid m : Varsym ('.' : s@(_ : _)) : ts)
                  =  Qual [m] (Varsym s) : qualify ts
qualify (Qual m (Conid m') : Varsym "." : t@(Conid _) : ts)
                  =  qualify (Qual (m ++ [m']) t : qualify ts)
qualify (Qual m (Conid m') : Varsym "." : t@(Varid _) : ts)
                  =  Qual (m ++ [m']) t : qualify ts
qualify (Qual m (Conid m') : Varsym ('.' : s@(':' : _)) : ts)
                  =  Qual (m ++ [m']) (Consym s) : qualify ts
qualify (Qual m (Conid m') : Varsym ('.' : s@(_ : _)) : ts)
                  =  Qual (m ++ [m']) (Varsym s) : qualify ts
qualify (t : ts)  =  t : qualify ts
-- ks, 27.06.2003: I have modified the fifth case of |qualify|
-- to only match if the |Varsym| contains at least one symbol
-- besides the dot. Otherwise the dot is an operator, not part
-- of a qualified name.
-- ks, 03.09.2003: Now dealing with hierarchical module names. This could
-- be made more efficient if that seems necessary.


-- | Joins backquoted identifiers. This is done after merging
-- qualified identifiers, because qualified identifiers may appear
-- in backquotes.
tidyup                        :: [Token] -> [Token]
tidyup []                     =  []
tidyup (Special '`' : t@(Varid _) : Special '`' : ts)
                              =  Op t : tidyup ts
tidyup (Special '`' : t@(Conid _) : Special '`' : ts)
                              =  Op t : tidyup ts
tidyup (Special '`' : t@(Qual _ (Varid _)) : Special '`' : ts)
                              =  Op t : tidyup ts
tidyup (Special '`' : t@(Qual _ (Conid _)) : Special '`' : ts)
                              =  Op t : tidyup ts
tidyup (String s : ts)        =  strItems s ++ tidyup ts
tidyup (Space s : ts)         =  splitSpace s ++ tidyup ts
tidyup (t : ts)               =  t : tidyup ts
-- Known bug/feature: ` div ` is not joined; in such a case, a
-- potential format statement @%format `div` = ...@ is ignored.

-- | Breaks a multiline string literal into several string items.
strItems                      :: String -> [Token]
strItems []                   =  impossible "strItems"
strItems (c : s)              =  case breaks isGap s of
    (item, '\\' : s')         -> String (c : item ++ "\\") : Space white : strItems rest
        where (white, rest)   =  span C.isSpace s'
    _                         -> [String (c : s)]

-- | Tests if the given string starts with a backslash followed by a newline.
isGap :: String -> Bool
isGap = isPrefixOf "\\\n"
-- ks, 12.01.2004: changed the definition of |isGap| to be |True|
-- only if the character following a backslash is a newline. Otherwise,
-- the sequence |"\\ "| will be incorrectly treated as a string gap.
-- I am not convinced that the special treatment of string gaps is a
-- good thing at all. String gaps don't work in newcode style, as it
-- is right now.

splitSpace :: String -> [Token]
splitSpace []  =  []
splitSpace s   =  Space t : splitSpace u
  where
    (t, u) = breakAfter (== '\n') s


{-
> where
> import Data.Char 	(  isSpace, isUpper, isLower, isDigit, isAlphaNum, isPunctuation  )
> import qualified Data.Char ( isSymbol )
> import Control.Monad
> import Control.Monad.Error ()
> import Document
> import Auxiliaries
> import TeXCommands    (  Lang(..)  )

%endif
A Haskell lexer, based on the Prelude function \hs{lex}.





% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 2}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Merging qualified names.








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

>     catCode (TeX _ _)         =  NoSep -- |impossible "catCode"|
>     catCode (Qual _ t)        =  catCode t
>     catCode (Op _)            =  Sep
>     token                     =  id
>     inherit _ t               =  t
>     fromToken                 =  id
-}
