
> module State
> where
>
> import System.IO
> import System.Process
>
> import Version
> import TeXCommands
> import qualified Math
> import qualified MathPoly as Poly
> import Directives
> import qualified FiniteMap as FM
> import Auxiliaries

> type CondInfo                 =  (FilePath, LineNo, Bool, Bool)
>
> data State                    =  State { style      :: Style,
>                                          lang       :: Lang,          -- Haskell or Agda, currently
>                                          verbose    :: Bool,
>                                          searchpath :: [FilePath],
>                                          linefile   :: Maybe FilePath,  -- The filepath to use for LINE pragmas, passed by the -h option by GHC in preprocessor mode
>                                          file       :: FilePath,      -- also used for `hugs'
>                                          lineno     :: LineNo,
>                                          ofile      :: FilePath,
>                                          olineno    :: LineNo,
>                                          atnewline  :: Bool,
>                                          fldir      :: Bool,          -- file/linenumber directives
>                                          pragmas    :: Bool,          -- generate LINE pragmas?
>                                          output     :: Handle,
>                                          opts       :: String,        -- options for `hugs'
>                                          files      :: [(FilePath, LineNo)], -- includees (?)
>                                          path       :: FilePath,      -- for relative includes
>                                          fmts       :: Formats,
>                                          subst      :: Substs,
>                                          stack      :: [Formats],     -- for grouping
>                                          toggles    :: Toggles,       -- @%let@ defined toggles
>                                          conds      :: [CondInfo],    -- for conditional directives
>                                          align      :: Maybe Int,     -- math: internal alignment column
>                                          stacks     :: (Math.Stack, Math.Stack),      -- math: indentation stacks
>                                          separation :: Int,           -- poly: separation
>                                          latency    :: Int,           -- poly: latency
>                                          pstack     :: Poly.Stack,    -- poly: indentation stack
>                                          externals  :: Externals      -- catchErrors for external processes (hugs,ghci)
>                                        }
>
> type Externals    =  FM.FiniteMap Char ProcessInfo
> type ProcessInfo  =  (Handle, Handle, Handle, ProcessHandle)

Initial state.

> state0                        :: State
> state0                        =  State { lang       = Haskell,
>                                          verbose    = False,
>                                          searchpath = searchPath,
>                                          lineno     = 0,
>                                          olineno    = 0,
>                                          atnewline  = True,
>                                          fldir      = False,
>                                          pragmas    = True,
>                                          output     = stdout,
>                                          opts       = "",
>                                          files      = [],
>                                          path       = "",
>                                          fmts       = FM.empty,
>                                          subst      = FM.empty,
>                                          stack      = [],
>                                          conds      = [],
>                                          align      = Nothing,
>                                          stacks     = ([], []),
>                                          separation = 2,
>                                          latency    = 2,
>                                          pstack     = [],
>                                          linefile   = Nothing,
>                                          -- ks, 03.01.04: added to prevent warnings during compilation
>                                          style      = error "uninitialized style",
>                                          file       = error "uninitialized filename",
>                                          ofile      = error "uninitialized filename",
>                                          toggles    = error "uninitialized toggles",
>                                          externals  = FM.empty
>                                        }


