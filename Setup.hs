import Distribution.Simple.Setup (CopyDest(..),ConfigFlags(..),BuildFlags(..),
                                  CopyFlags(..),RegisterFlags(..),InstallFlags(..),
                                  defaultRegisterFlags,fromFlagOrDefault,Flag(..),
                                  defaultCopyFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
                            (LocalBuildInfo(..),absoluteInstallDirs)
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.InstallDirs
                            (InstallDirs(..))
import Distribution.Simple.Program 
                            (Program(..),ConfiguredProgram(..),ProgramConfiguration(..),
                             ProgramLocation(..),simpleProgram,lookupProgram,
                             rawSystemProgramConf)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Data.Char (isSpace, showLitChar)
import Data.List (isSuffixOf,isPrefixOf)
import Data.Maybe (listToMaybe,isJust)
import Data.Version
import Control.Monad (when,unless)
import Text.Regex (matchRegex,matchRegexAll,mkRegex,mkRegexWithOpts,subRegex)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit
import System.IO (hGetContents,hClose,hPutStr,stderr)
import System.IO.Error (try)
import System.Process (runInteractiveProcess,waitForProcess)
import System.Directory
import System.Info (os)
  

lhs2tex = "lhs2TeX"
minPolytableVersion = [0,8,2]
shortversion = show (numversion `div` 100) ++ "." ++ show (numversion `mod` 100)
version = shortversion ++ if ispre then "pre" ++ show pre else ""
numversion = 115
ispre = False
pre = 1

main = defaultMainWithHooks lhs2texHooks

sep =  if isWindows then ";" else ":"

lhs2texBuildInfoFile :: FilePath
lhs2texBuildInfoFile = "." `joinFileName` ".setup-lhs2tex-config"

generatedFiles = ["Version.lhs","lhs2TeX.1",
                  "doc" `joinFileName` "InteractiveHugs.lhs",
                  "doc" `joinFileName` "InteractivePre.lhs"]

data Lhs2texBuildInfo =
  Lhs2texBuildInfo { installPolyTable      ::  Maybe String,
                     rebuildDocumentation  ::  Bool }
  deriving (Show, Read)

lhs2texHooks = simpleUserHooks
                 { hookedPrograms = [simpleProgram "hugs",
                                     simpleProgram "kpsewhich",
                                     simpleProgram "pdflatex",
                                     simpleProgram "mktexlsr"],
                   postConf       = lhs2texPostConf,
                   postBuild      = lhs2texPostBuild,
                   postCopy       = lhs2texPostCopy,
                   postInst       = lhs2texPostInst,
                   regHook        = lhs2texRegHook,
                   cleanHook      = lhs2texCleanHook
                 }

lhs2texPostConf a cf pd lbi =
    do  let v = fromFlagOrDefault normal (configVerbosity cf)
        -- check polytable
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
                       nec     <- if ex then do  info v $ "Found polytable package at: " ++ p
                                                 x  <- readFile p
                                                 let vp = do  vs <- matchRegex (mkRegexWithOpts " v(.*) .polytable. package" True True) x
                                                              listToMaybe [ r | v <- vs, (r,"") <- readP_to_S parseVersion v ]
                                                 let (sv,nec) = case vp of
                                                                  Just n  -> (showVersion n,versionBranch n < minPolytableVersion)
                                                                  Nothing -> ("unknown",True)
                                                 info v $ "Package polytable version: " ++ sv
                                                 return nec
                                        else return True
                       info v $ "Package polytable installation necessary: " ++ showYesNo nec
                       when nec $ info v $ "Using texmf tree at: " ++ b
                       return (if nec then Just b else Nothing)
                   else
                   do  warn v "No texmf tree found, polytable package cannot be installed"
                       return Nothing
        -- check documentation
        ex      <- doesFileExist $ "doc" `joinFileName` "Guide2.dontbuild"
        r       <- if ex then do info v "Documentation will not be rebuilt unless you remove the file \"doc/Guide2.dontbuild\""
                                 return False
                         else do let mProg = lookupProgram (simpleProgram "pdflatex") (withPrograms lbi)
                                 case mProg of
                                   Nothing  -> info v "Documentation cannot be rebuilt without pdflatex" >> return False
                                   Just _   -> return True
        unless r $ info v $ "Using pre-built documentation"
        writePersistLhs2texBuildConfig (Lhs2texBuildInfo { installPolyTable = i, rebuildDocumentation = r })
        mapM_ (\f -> do info v $ "Creating " ++ f
                        let hugsExists = lookupProgram (simpleProgram "hugs") (withPrograms lbi)
                        hugs <- case hugsExists of
                                  Nothing -> return ""
                                  Just _  -> fmap fst (getProgram "hugs" (withPrograms lbi))
                        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
                        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
                        readFile (f ++ ".in") >>= return .
                                                  -- these paths could contain backslashes, so we
                                                  -- need to escape them.
                                                  replace "@prefix@"  (escapeChars $ prefix (absoluteInstallDirs pd lbi NoCopyDest)) .
                                                  replace "@stydir@" (escapeChars $ datadir (absoluteInstallDirs pd lbi NoCopyDest)) .
                                                  replace "@LHS2TEX@" lhs2texBin .
                                                  replace "@HUGS@" hugs .
                                                  replace "@VERSION@" version .
                                                  replace "@SHORTVERSION@" shortversion .
                                                  replace "@NUMVERSION@" (show numversion) .
                                                  replace "@PRE@" (show pre) >>= writeFile f)
              generatedFiles
  where runKpseWhich v = runCommandProgramConf silent "kpsewhich" (withPrograms lbi) [v]
        runKpseWhichVar v = runKpseWhich $ "-expand-var='$" ++ v ++ "'"

lhs2texPostBuild a bf@(BuildFlags { buildVerbosity = vf }) pd lbi =
    do  let v = fromFlagOrDefault normal vf
        ebi <- getPersistLhs2texBuildConfig
        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
        callLhs2tex v lbi ["--code", "lhs2TeX.sty.lit"] (lhs2texDir `joinFileName` "lhs2TeX.sty")
        callLhs2tex v lbi ["--code", "lhs2TeX.fmt.lit"] (lhs2texDir `joinFileName` "lhs2TeX.fmt")
        createDirectoryIfMissing True lhs2texDocDir
        if rebuildDocumentation ebi then lhs2texBuildDocumentation a bf pd lbi
                                    else copyFileVerbose v ("doc" `joinFileName` "Guide2.pdf") (lhs2texDocDir `joinFileName` "Guide2.pdf")

lhs2texBuildDocumentation a (BuildFlags { buildVerbosity = vf }) pd lbi =
    do  let v = fromFlagOrDefault normal vf
        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
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
                                     -- TODO: replace or replaceEscaped
                                     replace "-pgmF \\.\\./lhs2TeX" ("-pgmF " ++ lhs2texBin ++ " -optF-Pdoc" ++ sep) $ c )
                         let incToStyle ["verbatim"]   = "verb"
                             incToStyle ["stupid"]     = "math"
                             incToStyle ["tex"]        = "poly"
                             incToStyle ["polytt"]     = "poly"
                             incToStyle ["typewriter"] = "tt"
                             incToStyle [x]            = x
                             incToStyle []             = "poly"
                         callLhs2tex v lbi ["--" ++ incToStyle inc , "-Pdoc" ++ sep, lhs2texDir `joinFileName` snippet]
                                           (lhs2texDocDir `joinFileName` s ++ ".tex")
                ) snippets
        callLhs2tex v lbi ["--poly" , "-Pdoc" ++ sep, "doc" `joinFileName` "Guide2.lhs"]
                          (lhs2texDocDir `joinFileName` "Guide2.tex")
        copyFileVerbose v ("polytable" `joinFileName` "polytable.sty") (lhs2texDocDir `joinFileName` "polytable.sty")
        copyFileVerbose v ("polytable" `joinFileName` "lazylist.sty")  (lhs2texDocDir `joinFileName` "lazylist.sty")
        d <- getCurrentDirectory
        setCurrentDirectory lhs2texDocDir
        -- call pdflatex as long as necessary
        let loop = do rawSystemProgramConf v (simpleProgram "pdflatex") (withPrograms lbi) ["Guide2.tex"]
                      x <- readFile "Guide2.log"
                      case matchRegex (mkRegexWithOpts "Warning.*Rerun" True True) x of
                        Just _  -> loop
                        Nothing -> return ()
        loop
        setCurrentDirectory d

lhs2texPostCopy a (CopyFlags { copyDest = cdf, copyVerbosity = vf }) pd lbi =
    do  let v = fromFlagOrDefault normal vf
        let cd = fromFlagOrDefault NoCopyDest cdf
        ebi <- getPersistLhs2texBuildConfig
        let dataPref = datadir (absoluteInstallDirs pd lbi cd)
        createDirectoryIfMissing True dataPref
        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        -- lhs2TeX.{fmt,sty}
        mapM_ (\f -> copyFileVerbose v (lhs2texDir `joinFileName` f) (dataPref `joinFileName` f))
              ["lhs2TeX.fmt","lhs2TeX.sty"]
        -- lhs2TeX library
        fmts <- fmap (filter (".fmt" `isSuffixOf`)) (getDirectoryContents "Library")
        mapM_ (\f -> copyFileVerbose v ("Library" `joinFileName` f) (dataPref `joinFileName` f))
              fmts
        -- documentation difficult due to lack of docdir
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
        let docDir = if isWindows
                       then dataPref `joinFileName` "Documentation"
                       else docdir (absoluteInstallDirs pd lbi cd) `joinFileName` "doc"
        let manDir = if isWindows
                       then dataPref `joinFileName` "Documentation"
                       else datadir (absoluteInstallDirs pd lbi cd) `joinFileName` ".." `joinFileName` "man" `joinFileName` "man1"
        createDirectoryIfMissing True docDir
        copyFileVerbose v (lhs2texDocDir `joinFileName` "Guide2.pdf") (docDir `joinFileName` "Guide2.pdf")
        when (not isWindows) $
          do createDirectoryIfMissing True manDir
             copyFileVerbose v ("lhs2TeX.1") (manDir `joinFileName` "lhs2TeX.1")
        -- polytable
        case (installPolyTable ebi) of
          Just texmf -> do  let texmfDir = texmf
                                ptDir = texmfDir `joinFileName` "tex" `joinFileName` "latex"
                                                 `joinFileName` "polytable"
                            createDirectoryIfMissing True ptDir
                            stys <- fmap (filter (".sty" `isSuffixOf`))
                                         (getDirectoryContents "polytable")
                            mapM_ (\f -> copyFileVerbose v ("polytable" `joinFileName` f)
                                                           (ptDir `joinFileName` f))
                                  stys
          Nothing    -> return ()

lhs2texPostInst a (InstallFlags { installPackageDB = db, installVerbosity = v }) pd lbi =
    do  lhs2texPostCopy a (defaultCopyFlags { copyDest = Flag NoCopyDest, copyVerbosity = v }) pd lbi
        lhs2texRegHook pd lbi Nothing (defaultRegisterFlags { regPackageDB = db, regVerbosity = v })

lhs2texRegHook pd lbi _ (RegisterFlags { regVerbosity = vf }) =
    do  let v = fromFlagOrDefault normal vf
        ebi <- getPersistLhs2texBuildConfig
        when (isJust . installPolyTable $ ebi) $
          do  rawSystemProgramConf v (simpleProgram "mktexlsr") (withPrograms lbi) []
              return ()

lhs2texCleanHook pd lbi v pshs =
    do  cleanHook simpleUserHooks pd lbi v pshs
        try $ removeFile lhs2texBuildInfoFile
        mapM_ (try . removeFile) generatedFiles

matchRegexRepeatedly re str =
    case matchRegexAll re str of
      Just (_,_,r,[s]) -> s : matchRegexRepeatedly re r
      Nothing          -> []


replace re t x = subRegex (mkRegexWithOpts re True True) x (escapeRegex t)
    where
    -- subRegex requires us to escape backslashes
    escapeRegex []        = []
    escapeRegex ('\\':xs) = '\\':'\\': escapeRegex xs
    escapeRegex (x:xs)    = x : escapeRegex xs
    
escapeChars :: String -> String
escapeChars t = foldr showLitChar [] t

showYesNo :: Bool -> String
showYesNo p | p          =  "yes"
            | otherwise  =  "no"

stripNewlines :: String -> String
stripNewlines = filter (/='\n')

stripQuotes :: String -> String
stripQuotes ('\'':s@(_:_)) = init s
stripQuotes x              = x

callLhs2tex v lbi params outf =
    do  let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let args    =  [ "-P" ++ lhs2texDir ++ sep ]
                     ++ [ "-o" ++ outf ]
                     ++ (if v == deafening then ["-v"] else [])
                     ++ params
	(ex,_,err) <- runCommand v lhs2texBin args
	hPutStr stderr (unlines . lines $ err)
	maybeExit (return ex)

runCommandProgramConf  ::  Verbosity              -- ^ verbosity
                       ->  String                 -- ^ program name
                       ->  ProgramConfiguration   -- ^ lookup up the program here
                       ->  [String]               -- ^ args
                       ->  IO (ExitCode,String,String)
runCommandProgramConf v progName programConf extraArgs =
    do  (prog,args) <- getProgram progName programConf
        runCommand v prog (args ++ extraArgs)

getProgram :: String -> ProgramConfiguration -> IO (String, [String])
getProgram progName programConf = 
             do  let mProg = lookupProgram (simpleProgram progName) programConf
                 case mProg of
                   Just (ConfiguredProgram { programLocation = UserSpecified p,
                                             programArgs = args })  -> return (p,args)
                   Just (ConfiguredProgram { programLocation = FoundOnSystem p,
                                             programArgs = args })  -> return (p,args)
                   _ -> (die (progName ++ " command not found"))

-- | Run a command in a specific environment and return the output and errors.
runCommandInEnv  ::  Verbosity             -- ^ verbosity
                 ->  String                -- ^ the command
                 ->  [String]              -- ^ args
                 ->  [(String,String)]     -- ^ the environment
                 ->  IO (ExitCode,String,String)
runCommandInEnv v cmd args env = 
                 do  when (v >= verbose) $ putStrLn (cmd ++ concatMap (' ':) args)
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
runCommand  ::  Verbosity              -- ^ verbosity
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


-- HACKS because the Cabal API isn't sufficient:

-- Distribution.Compat.FilePath is supposed to be hidden in future
-- versions, so we need our own version of it:
joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir++fname
  | otherwise                  = dir++pathSeparator:fname
  where 
 isPathSeparator :: Char -> Bool
 isPathSeparator | isWindows = ( `elem` "/\\" )
                 | otherwise = ( == '/' )
 pathSeparator   | isWindows = '\\'
                 | otherwise = '/'

-- It would be nice if there'd be a predefined way to detect this
isWindows = "mingw" `isPrefixOf` os || "win" `isPrefixOf` os 
