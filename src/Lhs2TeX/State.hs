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
import Lhs2TeX.SearchPath
import Lhs2TeX.Value
import Lhs2TeX.Version
import Lhs2TeX.Representation

-- | All the program state for lhs2TeX. We currently do not distinguish between
-- read-only state and state that can actually be updated during the program run.
data State =
  State
    { style      :: Style,       -- ^ the style under which lhs2TeX operates
      lang       :: Lang,        -- ^ the language selected
      verbose    :: Bool,        -- ^ how much output should be generated
      searchpath :: [FilePath],  -- ^ where lhs2TeX looks for inputs
      file       :: FilePath,    -- ^ file currently being processed
      linenumber :: LineNumber,  -- ^ current line number
      ofile      :: FilePath,    -- ^ original file of the line last printed
      olinenumber:: LineNumber,  -- ^ original number of the line last printed
      atnewline  :: Bool,        -- ^ at beginning of new line?
      fldir      :: Bool,        -- ^ whether we generate %file directives
      pragmas    :: Bool,        -- ^ whether we generate LINE pragmas
      output     :: Handle,      -- ^ where the output goes
      opts       :: String,      -- ^ options for embedded code evaluation
      externals  :: Externals,   -- ^ handles for embedded code evaluation
      files      :: [(FilePath, LineNumber)],
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
type CondInfo = (FilePath, LineNumber, Bool, Bool)

-- | The state at the beginning of a run of lhs2TeX, before processing
-- command line options.
initialState :: State
initialState =
  State
    { lang       = Haskell, -- default language is Haskell
      verbose    = False,   -- by default, we are relatively silent
      searchpath = defaultSearchPath,
                            -- defined elsewhere
      linenumber = 0,       -- we start at the beginning
      olinenumber= 0,
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

-- | Initialize the state with a couple of basic, but variable, settings.
-- Also initializes a number of variables that are available for
-- inspection during a run of lhs2TeX.
setupState :: Style -> FilePath -> [FilePath] -> State -> State
setupState newstyle newfile newpath state =
  state
    { style      = newstyle,
      file       = newfile,
      ofile      = newfile,
      searchpath = newpath,
      toggles    = fromList initialToggles }
  where
    initialToggles :: [(String, Value)]
    initialToggles =
      -- The following flags are set according to the flags lhs2TeX
      -- has been invoked with.
      [ ("style",   Int (fromEnum newstyle)),
        ("version", Int numversion),
        ("pre",     Int pre),
        ("lang",    Int (fromEnum (lang state))) ] ++
      -- The following flags are always the same. Since (for historic
      -- reasons?) the style and lang toggles are numeric and not strings,
      -- we have to have things to compare them with.
      toggleList (Proxy :: Proxy Style) ++
      toggleList (Proxy :: Proxy Lang)

-- | Helper function to generate a number of numeric values from
-- a given enumeration type. The argument is a dummy for the type
-- checker.
toggleList :: (Bounded a, Enum a, Representation a) =>
              Proxy a -> [(String, Value)]
toggleList x = [ (decode s, Int (fromEnum s)) |
                 s <- [minBound `asTypeOf` x .. maxBound] ]
