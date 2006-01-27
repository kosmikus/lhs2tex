%-------------------------------=  --------------------------------------------
\subsection{Deterministic parser}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Parser                 (  Parser, run, satisfy, lit, lits, wrap, nonnull, eof  )
> where
>
> import Char                   (  isSpace  )
> import Auxiliaries
> import Monad                  (  MonadPlus(..), filterM  )

%endif
Deterministische Mini-Parser.
%if style == math
%format (MkParser (p)) = p
%format (unParser (p)) = p
%else

> unParser (MkParser p)         =  p

%endif

> newtype Parser tok a          =  MkParser ([tok] -> Maybe (a, [tok]))
>
> run                           :: Parser tok a -> [tok] -> Maybe a
> run (MkParser p) inp          =  fmap fst (p inp)
>
> instance Functor (Parser tok) where
>     fmap f m                  =  m >>= \a -> return (f a)
> instance Monad (Parser tok) where
>     return a                  =  MkParser (\inp -> Just (a, inp))
>     m >>= k                   =  MkParser (\inp -> case unParser m inp of
>                                      Nothing        -> Nothing
>                                      Just (a, rest) -> unParser (k a) rest)
> instance MonadPlus  (Parser tok) where
>     mzero                     =  MkParser (\inp -> Nothing)
>     m `mplus` n               =  MkParser (\inp -> unParser m inp `mplus` unParser n inp)
>
> satisfy                       :: (tok -> Bool) -> Parser tok tok
> satisfy pred                  =  MkParser (\inp -> case inp of
>                                      a : rest | pred a -> Just (a, rest)
>                                      _                 -> Nothing)
>
> lit                           :: (Eq tok) => tok -> Parser tok tok
> lit c                         =  satisfy (== c)

ks, 06.09.2003: Adding eof that accepts succeeds only at the end of input.

> eof                           :: Parser tok ()
> eof                           =  MkParser (\inp -> case inp of
>                                      []                -> Just ((),[])
>                                      _                 -> Nothing)

|lits s| corresponds to |mapM_ lit_ s|.

> lits                          :: (Eq tok) => [tok] -> Parser tok ()
> lits s                        =  MkParser (\inp -> case splitAt (length s) inp of
>                                      (s', rest) | s == s' -> Just ((), rest)
>                                      _                    -> Nothing)

\Todo{Better name for |wrap|.}

> wrap                          :: ([tok] -> (a, [tok])) -> Parser tok a
> wrap f                        =  MkParser (\inp -> Just (f inp))
>
> nonnull                       :: ([tok] -> ([a], [tok])) -> Parser tok [a]
> nonnull f                     =  mfilter (not . null) (wrap f)

> mfilter p m                   =  m >>= \a -> if p a then return a else mzero

%if False

> {-
> lit_                          :: (Eq tok) => tok -> Parser tok ()
> lit_ c                        =  MkParser (\inp -> case inp of
>                                      c' : rest | c == c' -> Just ((), rest)
>                                      _                   -> Nothing)
> nonnull_ f                    =  MkParser (\inp -> case f inp of
>                                      res@((_ : _) ,_) -> Just res
>                                      _                -> Nothing)
> -}

%endif
