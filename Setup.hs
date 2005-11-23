import Distribution.Setup (CopyDest(..))
import Distribution.Simple
import Distribution.Simple.Configure (withPrograms)
import Distribution.Simple.Utils (die,rawSystemExit,maybeExit,copyFileVerbose)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..),mkDataDir)
import Distribution.Program (Program(..),ProgramConfiguration(..),
                             ProgramLocation(..),simpleProgram,lookupProgram,
                             rawSystemProgramConf)
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Compat.FilePath (joinFileName)
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import Control.Monad (when,unless)
import Text.Regex (matchRegex,matchRegexAll,mkRegex,mkRegexWithOpts,subRegex)
import System.Cmd (system)
import System.Exit
import System.IO (hGetContents,hClose,hPutStrLn,stderr)
import System.Process (runInteractiveProcess,waitForProcess)
import System.Directory

lhs2tex = "lhs2TeX"
minPolytableVersion = [0,8,2]
version = "1.11pre2"
numversion = 111
pre = 2

main = defaultMainWithHooks lhs2texHooks

lhs2texBuildInfoFile :: FilePath
lhs2texBuildInfoFile = "./.setup-lhs2tex-config"

data Lhs2texBuildInfo =
  Lhs2texBuildInfo { installPolyTable      ::  Maybe String,
                     rebuildDocumentation  ::  Bool }
  deriving (Show, Read)

lhs2texHooks = defaultUserHooks
                 { hookedPrograms = [simpleProgram "kpsewhich",
                                     simpleProgram "pdflatex",
                                     simpleProgram "mktexlsr"],
                   postConf       = lhs2texPostConf,
                   postBuild      = lhs2texPostBuild,
                   postInst       = lhs2texPostInst }

lhs2texPostConf a cf pd lbi =
    do  -- check polytable
        (_,b,_) <- runKpseWhichVar "TEXMFLOCAL"
	b       <- return . stripQuotes . stripNewlines $ b
	ex      <- return (not . all isSpace $ b) -- or check if directory exists?
        b       <- if ex then return b
                         else do  (_,b,_) <- (runKpseWhichVar "TEXMFMAIN")
	                          return . stripQuotes . stripNewlines $ b
	ex      <- return (not . all isSpace $ b) -- or check if directory exists?
        i       <- if ex then 
                   do  (_,p,_) <- runKpseWhich "polytable.sty"
                       p       <- return . stripNewlines $ p
                       ex      <- doesFileExist p
                       nec     <- if ex then do  message $ "Found polytable package at: " ++ p
                                                 x  <- readFile p
                                                 let vp = do  vs <- matchRegex (mkRegexWithOpts " v(.*) .polytable. package" True True) x
                                                              listToMaybe [ r | v <- vs, (r,"") <- readP_to_S parseVersion v ]
                                                 let (sv,nec) = case vp of
                                                                  Just n  -> (showVersion n,versionBranch n < minPolytableVersion)
                                                                  Nothing -> ("unknown",True)
                                                 message $ "Package polytable version: " ++ sv
                                                 return nec
                                        else return True
                       message $ "Package polytable installation necessary: " ++ showYesNo nec
                       when nec $ message $ "Using texmf tree at: " ++ b
                       return (if nec then Just b else Nothing)
                   else
                   do  message "No texmf tree found, polytable package cannot be installed"
                       return Nothing
        -- check documentation
        ex      <- doesFileExist $ "doc" `joinFileName` "Guide2.dontbuild"
        r       <- if ex then do message "Documentation will not be rebuilt unless you remove the file \"doc/Guide2.dontbuild\""
                                 return False
                         else do mProg <- lookupProgram "pdflatex" (withPrograms lbi)
                                 case mProg of
                                   Nothing  -> message "Documentation cannot be rebuilt without pdflatex" >> return False
                                   Just _   -> return True
        unless r $ message $ "Using pre-built documentation"
        writePersistLhs2texBuildConfig (Lhs2texBuildInfo { installPolyTable = i, rebuildDocumentation = r })
        mapM_ (\f -> do message $ "Creating " ++ f
                        readFile (f ++ ".in") >>= return .
                                                  replace "@VERSION@" version .
                                                  replace "@NUMVERSION@" (show numversion) .
                                                  replace "@PRE@" (show pre) >>= writeFile f)
              ["Version.lhs","lhs2TeX.1"]
        return ExitSuccess
  where runKpseWhich v = runCommandProgramConf 0 "kpsewhich" (withPrograms lbi) [v]
        runKpseWhichVar v = runKpseWhich $ "-expand-var='$" ++ v ++ "'"

lhs2texPostBuild a v pd lbi =
    do  ebi <- getPersistLhs2texBuildConfig
        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
        callLhs2tex v lbi "--code lhs2TeX.sty.lit" (lhs2texDir `joinFileName` "lhs2TeX.sty")
        callLhs2tex v lbi "--code lhs2TeX.fmt.lit" (lhs2texDir `joinFileName` "lhs2TeX.fmt")
        if rebuildDocumentation ebi then lhs2texBuildDocumentation a v pd lbi
                                    else copyFileVerbose v ("doc" `joinFileName` "Guide2.pdf") (lhs2texDocDir `joinFileName` "Guide2.pdf")
        return ExitSuccess

lhs2texBuildDocumentation a v pd lbi =
    do  let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
        createDirectoryIfMissing True lhs2texDocDir
        snippets <- do  guide <- readFile $ "doc" `joinFileName` "Guide2.lhs"
                        let s = matchRegexRepeatedly (mkRegexWithOpts "^.*input\\{(.*)\\}.*$" True True) guide
                        return s
        mapM_ (\s -> do  let snippet = "doc" `joinFileName` (s ++ ".lhs")
                         c <- readFile $ snippet
                         let inc = maybe ["poly"] id (matchRegex (mkRegexWithOpts "^%include (.*)\\.fmt" True True) c)
                         -- rewrite the path to ghc/hugs, and to the preprocessor
                         writeFile (lhs2texDir `joinFileName` snippet)
                                   ( -- replace "^%options ghc"        "%options ghc" .
                                     -- replace "^%options hugs"       "%options hugs" .
                                     replace "-pgmF \\.\\./lhs2TeX" ("-pgmF " ++ lhs2texBin ++ " -optF-Pdoc:") $ c )
                         let incToStyle ["verbatim"]   = "verb"
                             incToStyle ["stupid"]     = "math"
                             incToStyle ["tex"]        = "poly"
                             incToStyle ["polytt"]     = "poly"
                             incToStyle ["typewriter"] = "tt"
                             incToStyle [x]            = x
                             incToStyle []             = "poly"
                         callLhs2tex v lbi ("--" ++ incToStyle inc ++ " -Pdoc: " ++ lhs2texDir `joinFileName` snippet)
                                           (lhs2texDocDir `joinFileName` s ++ ".tex"))
              snippets
        callLhs2tex v lbi ("--poly -Pdoc: " ++ "doc" `joinFileName` "Guide2.lhs") (lhs2texDocDir `joinFileName` "Guide2.tex")
        copyFileVerbose v ("polytable" `joinFileName` "polytable.sty") (lhs2texDocDir `joinFileName` "polytable.sty")
        copyFileVerbose v ("polytable" `joinFileName` "lazylist.sty")  (lhs2texDocDir `joinFileName` "lazylist.sty")
        d <- getCurrentDirectory
        setCurrentDirectory lhs2texDocDir
        -- call pdflatex as long as necessary
        let loop = do rawSystemProgramConf v "pdflatex" (withPrograms lbi) ["Guide2.tex"]
                      x <- readFile "Guide2.log"
                      case matchRegex (mkRegexWithOpts "Warning.*Rerun" True True) x of
                        Just _  -> loop
                        Nothing -> return ()
        loop
        setCurrentDirectory d

lhs2texPostInst a v pd lbi =
    do  let dataPref = mkDataDir pd lbi NoCopyDest
        putStrLn $ dataPref
        exitWith ExitSuccess

matchRegexRepeatedly re str =
    case matchRegexAll re str of
      Just (_,_,r,[s]) -> s : matchRegexRepeatedly re r
      Nothing          -> []

replace re t x = subRegex (mkRegexWithOpts re True True) x t

showYesNo :: Bool -> String
showYesNo p | p          =  "yes"
            | otherwise  =  "no"

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s

stripNewlines :: String -> String
stripNewlines = filter (/='\n')

stripQuotes :: String -> String
stripQuotes ('\'':s@(_:_)) = init s
stripQuotes x              = x

callLhs2tex v lbi str outf =
    do  let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let cmd_line   = "\"" ++ lhs2texBin ++ "\" " ++ (if v > 4 then "-v " else "") ++ str ++ " >" ++ outf
        when (v > 0) $ putStrLn cmd_line
        maybeExit $ system cmd_line

runCommandRedirect  ::  Int                       -- ^ verbosity
                    ->  String                    -- ^ the command
                    ->  [String]                  -- ^ args
                    ->  FilePath                  -- ^ output file
                    ->  IO ExitCode
runCommandRedirect v cmd args f =
    do  (ex,out,err) <- runCommand v cmd args
        writeFile f out
        hPutStrLn stderr err
        return ex

runCommandProgramConf  ::  Int                    -- ^ verbosity
                       ->  String                 -- ^ program name
                       ->  ProgramConfiguration   -- ^ lookup up the program here
                       ->  [String]               -- ^ args
                       ->  IO (ExitCode,String,String)
runCommandProgramConf v progName programConf extraArgs =
    do  (prog,args) <- getProgram progName programConf
        runCommand v prog (args ++ extraArgs)

getProgram :: String -> ProgramConfiguration -> IO (String, [String])
getProgram progName programConf = 
             do  mProg <- lookupProgram progName programConf
                 case mProg of
                   Just (Program { programLocation = UserSpecified p,
                                   programArgs = args })  -> return (p,args)
                   Just (Program { programLocation = FoundOnSystem p,
                                   programArgs = args })  -> return (p,args)
                   _ -> (die (progName ++ " command not found"))

-- | Run a command in a specific environment and return the output and errors.
runCommandInEnv  ::  Int                   -- ^ verbosity
                 ->  String                -- ^ the command
                 ->  [String]              -- ^ args
                 ->  [(String,String)]     -- ^ the environment
                 ->  IO (ExitCode,String,String)
runCommandInEnv v cmd args env = 
                 do  when (v > 0) $ putStrLn (cmd ++ concatMap (' ':) args)
                     let env' = if null env then Nothing else Just env
                     (cin,cout,cerr,pid) <- runInteractiveProcess cmd args Nothing env'
                     hClose cin
                     out <- hGetContents cout
                     err <- hGetContents cerr
                     stringSeq out (hClose cout)
                     stringSeq err (hClose cerr)
                     exit <- waitForProcess pid
                     return (exit,out,err)

-- | Run a command and return the output and errors.
runCommand  ::  Int                    -- ^ verbosity
            ->  String                 -- ^ the command
            ->  [String]               -- ^ args
            ->  IO (ExitCode,String,String)
runCommand v cmd args = runCommandInEnv v cmd args []

-- | Completely evaluates a string.
stringSeq :: String -> b -> b
stringSeq []      c  =  c
stringSeq (x:xs)  c  =  stringSeq xs c

getPersistLhs2texBuildConfig :: IO Lhs2texBuildInfo
getPersistLhs2texBuildConfig = do
  e <- doesFileExist lhs2texBuildInfoFile
  let dieMsg = "error reading " ++ lhs2texBuildInfoFile ++ "; run \"setup configure\" command?\n"
  when (not e) (die dieMsg)
  str <- readFile lhs2texBuildInfoFile
  case reads str of
    [(bi,_)] -> return bi
    _        -> die dieMsg

writePersistLhs2texBuildConfig :: Lhs2texBuildInfo -> IO ()
writePersistLhs2texBuildConfig lbi = do
  writeFile lhs2texBuildInfoFile (show lbi)

