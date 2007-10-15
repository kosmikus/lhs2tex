import Distribution.Setup (CopyDest(..),ConfigFlags(..),BuildFlags(..),
                           CopyFlags(..),RegisterFlags(..),InstallFlags(..))
import Distribution.Simple
import Distribution.Simple.Utils (die,rawSystemExit,maybeExit,copyFileVerbose)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..),mkDataDir,substDir,absolutePath)
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.PackageDescription (PackageDescription(..),setupMessage)
import Distribution.Program (Program(..),ProgramConfiguration(..),
                             ProgramLocation(..),simpleProgram,lookupProgram,
                             rawSystemProgramConf)
-- import Distribution.Compat.ReadP (readP_to_S)
import Data.Char (isSpace, showLitChar)
import Data.List (isSuffixOf,isPrefixOf)
import Data.Maybe (listToMaybe,isJust)
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
numversion = 113
ispre = True
pre = 1

main = defaultMainWithHooks lhs2texHooks

lhs2texBuildInfoFile :: FilePath
lhs2texBuildInfoFile = "." `joinFileName` ".setup-lhs2tex-config"

generatedFiles = ["Version.lhs","lhs2TeX.1",
                  "doc" `joinFileName` "InteractiveHugs.lhs",
                  "doc" `joinFileName` "InteractivePre.lhs"]

data Lhs2texBuildInfo =
  Lhs2texBuildInfo { installPolyTable      ::  Maybe String,
                     rebuildDocumentation  ::  Bool }
  deriving (Show, Read)

lhs2texHooks = defaultUserHooks
                 { hookedPrograms = [simpleProgram "hugs",
                                     simpleProgram "kpsewhich",
                                     simpleProgram "pdflatex",
                                     simpleProgram "mktexlsr"],
                   confHook       = lhs2texConfHook,
                   postConf       = lhs2texPostConf,
                   postBuild      = lhs2texPostBuild,
                   postCopy       = lhs2texPostCopy,
                   postInst       = lhs2texPostInst,
                   regHook        = lhs2texRegHook,
                   cleanHook      = lhs2texCleanHook
                 }

lhs2texConfHook pd cf =
    do  -- give status message
        setupMessage "Pre-Configuring" pd
        comp <- configCompilerAux (cf { configVerbose = 0 })
        let flavor = compilerFlavor comp
        let ver = compilerVersion comp
        pd <- if flavor == GHC && 
                 withinRange ver (EarlierVersion (Version [6,5] []))
              then do
                     when (configVerbose cf > 0) $ putStrLn "configure: adapting for ghc < 6.5"
                     return (pd { buildDepends =
                                  filter (\ (Dependency d _) -> d /= "regex-compat") (buildDepends pd) })
              else return pd
        confHook defaultUserHooks pd cf

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
                        hugsExists <- lookupProgram "hugs" (withPrograms lbi)
                        hugs <- case hugsExists of
                                  Nothing -> return ""
                                  Just (Program { programLocation = EmptyLocation })
                                          -> return ""
                                  Just _  -> fmap fst (getProgram "hugs" (withPrograms lbi))
                        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
                        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
                        readFile (f ++ ".in") >>= return .
                                                  -- these paths could contain backslashes, so we
                                                  -- need to escape them.
                                                  replace "@prefix@"  (escapeChars $ prefix lbi) .
                                                  replace "@datadir@" (escapeChars $ absolutePath pd lbi NoCopyDest (datadir lbi)) .
                                                  replace "@LHS2TEX@" lhs2texBin .
                                                  replace "@HUGS@" hugs .
                                                  replace "@VERSION@" version .
                                                  replace "@SHORTVERSION@" shortversion .
                                                  replace "@NUMVERSION@" (show numversion) .
                                                  replace "@PRE@" (show pre) >>= writeFile f)
              generatedFiles
        return ExitSuccess
  where runKpseWhich v = runCommandProgramConf 0 "kpsewhich" (withPrograms lbi) [v]
        runKpseWhichVar v = runKpseWhich $ "-expand-var='$" ++ v ++ "'"

lhs2texPostBuild a bf@(BuildFlags { buildVerbose = v }) pd lbi =
    do  ebi <- getPersistLhs2texBuildConfig
        let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let lhs2texDocDir = lhs2texDir `joinFileName` "doc"
        callLhs2tex v lbi ["--code", "lhs2TeX.sty.lit"] (lhs2texDir `joinFileName` "lhs2TeX.sty")
        callLhs2tex v lbi ["--code", "lhs2TeX.fmt.lit"] (lhs2texDir `joinFileName` "lhs2TeX.fmt")
        createDirectoryIfMissing True lhs2texDocDir
        if rebuildDocumentation ebi then lhs2texBuildDocumentation a bf pd lbi
                                    else copyFileVerbose v ("doc" `joinFileName` "Guide2.pdf") (lhs2texDocDir `joinFileName` "Guide2.pdf")
        return ExitSuccess

lhs2texBuildDocumentation a (BuildFlags { buildVerbose = v }) pd lbi =
    do  let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
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
                                     replace "-pgmF \\.\\./lhs2TeX" ("-pgmF " ++ lhs2texBin ++ " -optF-Pdoc:") $ c )
                         let incToStyle ["verbatim"]   = "verb"
                             incToStyle ["stupid"]     = "math"
                             incToStyle ["tex"]        = "poly"
                             incToStyle ["polytt"]     = "poly"
                             incToStyle ["typewriter"] = "tt"
                             incToStyle [x]            = x
                             incToStyle []             = "poly"
                         callLhs2tex v lbi ["--" ++ incToStyle inc , "-Pdoc:", lhs2texDir `joinFileName` snippet]
                                           (lhs2texDocDir `joinFileName` s ++ ".tex")
                ) snippets
        callLhs2tex v lbi ["--poly" , "-Pdoc:", "doc" `joinFileName` "Guide2.lhs"]
                          (lhs2texDocDir `joinFileName` "Guide2.tex")
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

lhs2texPostCopy a (CopyFlags { copyDest = cd, copyVerbose = v }) pd lbi =
    do  ebi <- getPersistLhs2texBuildConfig
        let dataPref = mkDataDir pd lbi cd
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
                       else absolutePath pd lbi cd (datadir lbi `joinFileName` "doc" `joinFileName` datasubdir lbi)
        let manDir = if isWindows
                       then dataPref `joinFileName` "Documentation"
                       else absolutePath pd lbi cd (datadir lbi `joinFileName` "man" `joinFileName` "man1")
        createDirectoryIfMissing True docDir
        copyFileVerbose v (lhs2texDocDir `joinFileName` "Guide2.pdf") (docDir `joinFileName` "Guide2.pdf")
        when (not isWindows) $
          do createDirectoryIfMissing True manDir
             copyFileVerbose v ("lhs2TeX.1") (manDir `joinFileName` "lhs2TeX.1")
        -- polytable
        case (installPolyTable ebi) of
          Just texmf -> do  let texmfDir = absolutePath pd lbi cd texmf
                                ptDir = texmfDir `joinFileName` "tex" `joinFileName` "latex"
                                                 `joinFileName` "polytable"
                            createDirectoryIfMissing True ptDir
                            stys <- fmap (filter (".sty" `isSuffixOf`))
                                         (getDirectoryContents "polytable")
                            mapM_ (\f -> copyFileVerbose v ("polytable" `joinFileName` f)
                                                           (ptDir `joinFileName` f))
                                  stys
          Nothing    -> return ()
        return ExitSuccess

lhs2texPostInst a (InstallFlags { installUserFlags = u, installVerbose = v }) pd lbi =
    do  lhs2texPostCopy a (CopyFlags { copyDest = NoCopyDest, copyVerbose = v }) pd lbi
        lhs2texRegHook pd lbi Nothing (RegisterFlags { regUser = u, regInPlace = False, regWithHcPkg = Nothing, regGenScript = False, regVerbose = v })
        return ExitSuccess

lhs2texRegHook pd lbi _ (RegisterFlags { regVerbose = v }) =
    do  ebi <- getPersistLhs2texBuildConfig
        when (isJust . installPolyTable $ ebi) $
          do  rawSystemProgramConf v "mktexlsr" (withPrograms lbi) []
              return ()

lhs2texCleanHook pd lbi v pshs =
    do  cleanHook defaultUserHooks pd lbi v pshs
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

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s

stripNewlines :: String -> String
stripNewlines = filter (/='\n')

stripQuotes :: String -> String
stripQuotes ('\'':s@(_:_)) = init s
stripQuotes x              = x

callLhs2tex v lbi params outf =
    do  let lhs2texDir = buildDir lbi `joinFileName` lhs2tex
        let lhs2texBin = lhs2texDir `joinFileName` lhs2tex
        let args    =  [ "-P" ++ lhs2texDir ++ ":" ]
                     ++ [ "-o" ++ outf ]
                     ++ (if v > 4 then ["-v"] else [])
                     ++ params
	(ex,_,err) <- runCommand v lhs2texBin args
	hPutStr stderr (unlines . lines $ err)
	maybeExit (return ex)

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
