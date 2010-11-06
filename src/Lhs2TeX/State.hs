-- | The program state for lhs2TeX.
module Lhs2TeX.State where

import System.IO

import Lhs2TeX.Flags
import Lhs2TeX.Eval             as Eval
import Lhs2TeX.Utils
import Lhs2TeX.Math.Classic     as Math
import Lhs2TeX.Math.Poly        as Poly
import Lhs2TeX.Directive.Format as Format
import Lhs2TeX.Directive.Subst  as Subst
import Lhs2TeX.Directive.Let    as Let
import Lhs2TeX.SearchPath

-- | All the program state for lhs2TeX. We currently do not distinguish between
-- read-only state and state that can actually be updated during the program run.
data State =
  State
    { style      :: Style,       -- ^ the style under which lhs2TeX operates
      lang       :: Lang,        -- ^ the language selected
      verbose    :: Bool,        -- ^ how much output should be generated
      searchpath :: [FilePath],  -- ^ where lhs2TeX looks for inputs
      file       :: FilePath,    -- ^ file currently being processed
      lineno     :: LineNo,      -- ^ current line number
      ofile      :: FilePath,    -- ^ old file: file being processed before
      olineno    :: LineNo,      -- ^ old line number
      atnewline  :: Bool,        -- ^ at beginning of new line?
      fldir      :: Bool,        -- ^ whether we generate %file directives
      pragmas    :: Bool,        -- ^ whether we generate LINE pragmas
      output     :: Handle,      -- ^ where the output goes
      opts       :: String,      -- ^ options for embedded code evaluation
      externals  :: Externals,   -- ^ handles for embedded code evaluation
      files      :: [(FilePath, LineNo)],
                                 -- ^ stack of files being processed
      path       :: FilePath,    -- ^ path of current file
      fmts       :: Formats,     -- ^ %format directives currently active
      substs     :: Substs,      -- ^ %subst directives currently active
      stack      :: [FilePath],  -- ^ %format directives of surrounding groups
      toggles    :: Toggles,     -- ^ %let directives currently active
      conds      :: [CondInfo],  -- ^ for %if directives
      
      -- The rest is style-specific state.
      
      align      :: Maybe Int,   -- ^ math style: alignment column
      stacks     :: (Math.Stack, Math.Stack),
                                 -- ^ math : indentation stacks
      separation :: Int,         -- ^ poly style: separation
      latency    :: Int,         -- ^ poly style: latency
      pstack     :: Poly.Stack   -- ^ poly style: indentation stack
    }  

-- | Information for conditionals. TODO: turn this into a datatype.
type CondInfo = (FilePath, LineNo, Bool, Bool)

-- | The state at the beginning of a run of lhs2TeX, before processing
-- command line options.
initialState :: State
initialState =
  State
    { lang       = Haskell, -- default language is Haskell
      verbose    = False,   -- by default, we are relatively silent
      searchpath = defaultSearchPath,
                            -- defined elsewhere
      lineno     = 0,       -- we start at the beginning
      olineno    = 0,
      atnewline  = True,    -- we start at a new line
      fldir      = False,   -- by default, no %file pragmas
      pragmas    = True,    -- by default, LINE pragmas
      output     = stdout,  -- default output is to stdout
      opts       = "",      -- embedded code eval will fail without %options directive
      externals  = Eval.empty,
      files      = [],      -- no files on stack yet
      path       = "",
      fmts       = Format.empty,
                            -- we start with no formatting directives
      substs     = Subst.empty,
                            -- and with not substitution directives
      stack      = [],
      conds      = [],
      align      = Nothing, -- alignment column is initially unset
      stacks     = ([], []),
      separation = 2,       -- by default, we expect 2 spaces for trigger alignment
      latency    = 2,       -- and 2 spaces to be picked up by an alignment column
      pstack     = [],
      style      = error "internal: uninitialized style",
      file       = error "internal: uninitialized file",
      ofile      = error "internal: uninitialized ofile",
      toggles    = error "internal: uninitialized toggles"
    }
