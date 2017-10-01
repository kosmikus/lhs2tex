%-------------------------------=  --------------------------------------------
\subsection{State transformer}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module StateT                 (  module StateT  )
> where
>
> import Control.Applicative
> import Control.Monad.Except
> import Control.Monad.State

%endif

|IO| with internal state and error handling.

%if style == math
%format MkXIO (m) = m
%format unXIO (m) = m
%endif

> newtype XIO exc st a          =  MkXIO (ExceptT exc (StateT st IO) a)
>   deriving (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError exc)

-- XIO exc st a ~= StateT st IO (Either exc a)
--              ~= ErrorT exc (StateT st IO) a

%if style /= math

> unXIO                         :: XIO exc st a -> ExceptT exc (StateT st IO) a
> unXIO (MkXIO f)               =  f

%endif

\NB The state is preserved upon failure.

> toIO                          :: XIO exc st a -> IO a
> toIO (MkXIO m)                =  do
>                                    (r, _) <- runStateT (runExceptT m)
>                                                        (error "no initial state supplied")
>                                    case r of Left  _ -> error "unhandled error"
>                                              Right x -> return x

> fromEither                    :: Either exc a -> XIO exc st a
> fromEither                    =  either throwError return
