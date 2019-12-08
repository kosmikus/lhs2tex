> module Options where
>
> import System.Console.GetOpt

> import FileNameUtils (modifySearchPath, openOutputFile)
> import State (State(..))
> import TeXCommands (Class(..), Directive(..), Lang(..), Style(..))

Converting command line options into directives.

> uheader :: String
> uheader = "lhs2TeX [ options ] files\n\nAvailable options:\n"

ks, 20.07.2003: The short option for @--align@ has been changed into @-A@. Otherwise
@-align@ would not trigger compatibility mode, but be interpreted as a valid option
usage.

ks, 24.03.2004: The long option @--verbose@ has been removed for now,
because with some versions of GHC it triggers ambiguity errors with
@--verb@.

> options :: [OptDescr (State -> IO State, [Class] -> [Class], [Style])]
> options =
>   [  Option ['h','?'] ["help"]
>        (NoArg (return, id, [Help]))
>        "get this help"
>
>   ,  Option ['v'] [] {- ["verbose"] -}
>        (NoArg (\ s -> return $ s { verbose = True }, id, []))
>        "be verbose"
>
>   ,  Option ['V'] ["version"]
>        (NoArg (return, id, [Version]))
>        "show version"
>
>   ,  Option [] ["tt"]
>        (NoArg (return, id, [Typewriter]))
>        "typewriter style (deprecated)"
>
>   ,  Option [] ["math"]
>        (NoArg (return, id, [Math]))
>        "math style (deprecated)"
>
>   ,  Option [] ["poly"]
>        (NoArg (return, id, [Poly]))
>        "poly style (default)"
>
>   ,  Option [] ["markdown"]
>        (NoArg (return, id, [Markdown]))
>        "markdown style"
>
>   ,  Option [] ["code"]
>        (NoArg (return, id, [CodeOnly]))
>        "code style (deprecated)"
>
>   ,  Option [] ["newcode"]
>        (NoArg (return, id, [NewCode]))
>        "new code style"
>
>   ,  Option [] ["verb"]
>        (NoArg (return, id, [Verb]))
>        "verbatim (deprecated)"
>
>   ,  Option [] ["haskell"]
>        (NoArg (\ s -> return $ s { lang = Haskell }, id, []))
>        "Haskell lexer (default)"
>
>   ,  Option [] ["agda"]
>        (NoArg (\ s -> return $ s { lang = Agda }, id, []))
>        "Agda lexer"
>
>   ,  Option [] ["pre"]
>        (NoArg (return, id, [Pre]))
>        "act as ghc preprocessor"
>
>   ,  Option ['o'] ["output"]
>        (ReqArg
>          (\ f ->
>            (\ s -> do
>              h <- openOutputFile f
>              return $ s { output = h }
>            , id
>            , []
>            )
>          )
>          "file"
>        )
>        "specify output file"
>
>   ,  Option [] ["file-directives"]
>        (NoArg (\ s -> return $ s { fldir = True }, id, []))
>        "generate %file directives"
>
>   ,  Option [] ["no-pragmas"]
>        (NoArg (\s -> return $ s { pragmas = False }, id, []))
>        "no LINE pragmas"
>
>   ,  Option ['A'] ["align"]
>        (ReqArg (\ c -> (return, (Directive Align c:), [])) "col")
>        "align at <col>"
>
>   ,  Option ['i'] ["include"]
>        (ReqArg (\ f -> (return, (Directive Include f:), [])) "file")
>        "include <file>"
>
>   ,  Option ['l'] ["let"]
>        (ReqArg (\ s -> (return, (Directive Let s:), [])) "equation")
>        "assume <equation>"
>
>   ,  Option ['s'] ["set"]
>        (ReqArg (\ s -> (return, (Directive Let (s ++ " = True"):), [])) "flag")
>        "set <flag>"
>
>   ,  Option ['u'] ["unset"]
>        (ReqArg (\ s -> (return, (Directive Let (s ++ " = False"):), [])) "flag")
>        "unset <flag>"
>
>   ,  Option ['P'] ["path"]
>        (ReqArg
>          (\ p ->
>            ( \ s ->
>              return $ s { searchpath = modifySearchPath (searchpath s) p }
>            , id
>            , []
>            )
>          )
>          "path"
>        )
>        "modify search path"
>
>   ,  Option [] ["searchpath"]
>        (NoArg (return, id, [SearchPath]))
>        "show searchpath"
>
>   ,  Option [] ["copying"]
>        (NoArg (return, id, [Copying]))
>        "display license"
>
>   ,  Option [] ["warranty"]
>        (NoArg (return, id, [Warranty]))
>        "info about warranty"
>   ]

Options that are not advertised in @--help@ output:

> hiddenOptions :: [OptDescr (State -> IO State, [Class] -> [Class], [Style])]
> hiddenOptions =
>   [  Option [] ["texsearchpath"]
>        (NoArg (return, id, [TeXSearchPath]))
>        "show searchpath in code environment"
>   ]

> coptions :: [String] -> ([Class], [String])
> coptions = foldr (<<|) ([], [])
>   where
>   "-align"         <<| (ds, s :  as) = (Directive Align    s : ds,      as)
>   "-i"             <<| (ds, s :  as) = (Directive Include  s : ds,      as)
>   "-l"             <<| (ds, s :  as) = (Directive Let      s : ds,      as)
>   ('-' : 'i' : s)  <<| (ds,      as) = (Directive Include  s : ds,      as)
>   ('-' : 'l' : s)  <<| (ds,      as) = (Directive Let      s : ds,      as)
>   s                <<| (ds,      as) = (                       ds, s :  as)


