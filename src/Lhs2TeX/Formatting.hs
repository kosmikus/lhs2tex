-- | The actual interpreter for the lhs2TeX input language.
module Lhs2TeX.Formatting where

import System.FilePath
import Data.List as List

import Lhs2TeX.Utils
import Lhs2TeX.Monad
import Lhs2TeX.Directive
import Lhs2TeX.TeX.Syntax
import Lhs2TeX.TeX.Parser
import Lhs2TeX.State
import Lhs2TeX.Flags (Style(..))
import Lhs2TeX.Document
import Lhs2TeX.Verbatim        as Verbatim
import Lhs2TeX.Directive.Subst as Subst
import Lhs2TeX.Math.Classic    as Math
import Lhs2TeX.Math.Poly       as Poly
import Lhs2TeX.Typewriter      as Typewriter
import Lhs2TeX.NewCode         as NewCode

-- TODO: Look at the hardcoded 120 for `Verbatim.display' invocations.

-- | Format a string.
formatStr :: String -> Lhs2TeX ()
formatStr str = formats (parse 1 str)
-- TODO: `formatStr' is called from various places, for instance in
-- `eject' for embedded TeX. It seems wrong to always pass `1' as the
-- line number to the TeX parser.

-- | Interpreter for the lhs2TeX abstract syntax. This function only
-- deals with maintaining the line number information and delegates
-- the handling of individual tokens to `format' and `conditionalDirective'.
formats :: [Numbered Class] -> Lhs2TeX ()
formats []                                = return ()
formats (Numbered n (Directive d s) : ts)
  | conditional d                         =
  do
    modify (\ st -> st { linenumber = n })
    conditionalDirective d s ts
formats (Numbered n t               : ts) =
  do
    modify (\ st -> st { linenumber = n })
    format t
    formats ts

-- | Interpreter for an individual token. Delegates to a number of
-- helper functions for the different instructions. Those functions
-- take care of performing the right action depending on the selected
-- style.
format :: Class -> Lhs2TeX ()
format (Many s)                     = out (Text s)
format (Inline s)                   = Lhs2TeX.Formatting.inline s
format (Command     Hs           s) = Lhs2TeX.Formatting.inline s
format (Command     (Vrb b)      s) = out (Verbatim.inline b s)
format (Command     Eval         s) = eval s
format (Command     Perform      s) = perform s
format (Environment Evaluate     s) = evaluate s
format (Environment Haskell      s) = Lhs2TeX.Formatting.display s
format (Environment Code         s) = Lhs2TeX.Formatting.display s
format (Environment Spec         s) = spec s
format (Environment Hide         s) = return ()
format (Environment Ignore       s) = return ()
format (Environment (Verbatim b) s) = out (Verbatim.display 120 b s)

eval = undefined
perform = undefined
evaluate = undefined

-- | Handles the formatting of inline code.
inline :: String -> Lhs2TeX ()
inline txt = eject =<< select =<< gets style
  where
    select :: Style -> Lhs2TeX Doc
    select Verb       = return (Verbatim.inline False txt)
                          -- spaces not explicit
    select Typewriter = Typewriter.inline txt
    select Math       = Math.inline txt
    select Poly       = Poly.inline txt
    -- inline code is discarded in code modes
    select CodeOnly   = return Empty
    select NewCode    = return Empty

-- | Handles the formatting of displayed code.
display :: String -> Lhs2TeX ()
display txt = eject =<< select =<< gets style
  where
    select :: Style -> Lhs2TeX Doc
    select Verb       = return (Verbatim.display 120 False txt)
                          -- spaces not explicit
    select Typewriter = Typewriter.display txt
    select Math       = Math.display txt
    select Poly       = Poly.display txt
    -- displayed code is included in code modes
    select NewCode    = insertLinePragma `ap` NewCode.display txt
    select CodeOnly   = return (Text (trim txt))

-- | Handles the formatting of spec code. Such code is handled like
-- displayed code, except that it is discarded in code modes.
spec :: String -> Lhs2TeX ()
spec txt = select =<< gets style
  where
    select :: Style -> Lhs2TeX ()
    select NewCode  = return ()
    select CodeOnly = return ()
    select _        = Lhs2TeX.Formatting.display txt

-- | Insert a line pragma at the beginning of a document when requested.
-- TODO: be more clever about when a pragma is needed?
insertLinePragma :: Lhs2TeXPure (Doc -> Doc)
insertLinePragma =
  do
    st <- get
    let pragma = sub'pragma $ Text $
                   "LINE " ++ show (linenumber st + 1) ++ " " ++
                   show (takeFileName $ file st)
    return $ if pragmas st
               then \ d -> pragma <> sub'nl <> d
               else id

-- | Prints a document to the output file except is one of the
-- code modes has been selected.
out :: Doc -> Lhs2TeX ()
out d = eject . select =<< gets style
  where
    select :: Style -> Doc
    select CodeOnly = Empty
    select NewCode  = Empty
    select _        = d

-- | Prints a document to the output file.
eject :: Doc -> Lhs2TeX ()
eject Empty        = return ()
eject (Text s)     =
  do
    emitFilePragma s     -- only if requested and necessary
    putStrOut s
    adjustPositionInfo s -- ... for the %file pragmas
eject (d1 :^: d2)  = eject d1 >> eject d2
eject (Embedded s) = formatStr s
eject (Sub s ds)   = substitute s ds

-- | Tries to apply a substitution directive and prints the result to
-- the output file.
substitute :: String -> [Doc] -> Lhs2TeX ()
substitute s ds =
  do
    ss <- gets substs
    case Subst.lookup s ss of
      Nothing  -> throwError undefined
      Just sub -> eject (sub ds)

-- | Prints a %file pragma to the output file if %file
-- pragmas are enabled and if the flow of lines compared to
-- the previous %file pragma has been changed.
emitFilePragma :: String -> Lhs2TeX ()
emitFilePragma txt =
  do
    st <- get
    when (fldir st &&             -- %file directive enabled
          atnewline st &&         -- we are at the beginning of a line
          not (List.null txt) &&  -- next line isn't empty
          (ofile st /= file st || olinenumber st /= linenumber st)
                                  -- flow has changed
         ) $
      do
        putStrOutLn
          ("%file " ++ show (linenumber st) ++ " " ++ show (file st))
        put (st { ofile = file st, olinenumber = linenumber st })
          -- update the position information

adjustPositionInfo :: String -> Lhs2TeXPure ()
adjustPositionInfo txt =
  let
    ls  :: Int           -- lines to add
    enl :: Bool -> Bool  -- beginning of new line transformer
    (ls, enl) = go 0 txt
      where
        go :: Int -> String -> (Int, Bool -> Bool)
        go n ('\n' : [])  =  (n + 1, const True )
        go n (_    : [])  =  (n    , const False)
        go n         []   =  (n    , id         )
        go n ('\n' : xs)  =  go (n + 1) xs
        go n (_    : xs)  =  go  n      xs
  in
    modify (\ st -> st { olinenumber = olinenumber st + ls,
                         atnewline   = enl (atnewline st) })


conditionalDirective :: Directive -> String -> [Numbered Class] ->
                        Lhs2TeXPure ()
conditionalDirective d s ts =
  let
    dir :: Directive -> [CondInfo] -> Lhs2TeXPure ()
    dir If                      bs  = undefined
    dir Elif  ((f, l, b2, b1) : bs) = undefined
    dir Else  ((f, l, b2, b1) : bs) = undefined
    dir Endif ((f, l, b2, b1) : bs) = undefined
    dir EOF                     []  = return ()
    dir EOF                     bs  = throwError undefined
    dir d                       _   = undefined
  in
    dir d =<< gets conds