%-------------------------------=  --------------------------------------------
\subsection{State transformer}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module StateT                 (  module StateT  )
> where
>
> import Auxiliaries

%endif

|IO| mit internem Zustand und Fehlerbehandlung.

%if style == math
%format MkXIO (m) = m
%format unXIO (m) = m
%endif

> newtype XIO exc st a          =  MkXIO (st -> IO (Either exc a, st))

%if style /= math

> unXIO (MkXIO f)               =  f

%endif

\NB The state is preserved upon failure.

> instance Functor (XIO exc st) where
>     fmap f m                  =  m >>= \a -> return (f a)
>
> instance Monad (XIO exc st) where
>     return a                  =  MkXIO (\st -> return (Right a, st))
>     m >>= k                   =  MkXIO (\st -> do (r, st') <- unXIO m st
>                                                   case r of
>                                                     Left e  -> return (Left e, st')
>                                                     Right a -> unXIO (k a) st')

\NB We cannot replace |return (Left e, st')| by |return (r, st')| since
the type is not general enough then.

> fetch                         :: XIO exc st st
> fetch                         =  MkXIO (\st -> return (Right st, st))
>
> store                         :: st -> XIO exc st ()
> store st'                     =  MkXIO (\st -> return (Right (), st'))
>
> update                        :: (st -> st) -> XIO exc st ()
> update f                      =  do st <- fetch; store (f st)
>
> toIO                          :: XIO exc st a -> IO a
> toIO m                        =  do (a, _) <- unXIO m undefined; return (fromRight a)
>
> fromIO                        :: IO a -> XIO exc st a
> fromIO m                      =  MkXIO (\st -> do a <- m; return (Right a, st))
>
> raise                         :: exc -> XIO exc st a
> raise e                       =  MkXIO (\st -> return (Left e, st))
>
> try                           :: XIO exc st a -> XIO exc' st (Either exc a)
> try m                         =  MkXIO (\st -> do (r, st') <- unXIO m st; return (Right r, st'))
>
>
> handle                        :: XIO exc st a -> (exc -> XIO exc' st a) -> XIO exc' st a
> handle m h                    =  try m >>= either h return
>
> fromEither                    :: Either exc a -> XIO exc st a
> fromEither                    =  either raise return
