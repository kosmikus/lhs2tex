%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}

>\documentclass{article}
>%include polycode.fmt
>\begin{document}
>This is the famous ``Hello world'' example, 
>written in Haskell:
>\begin{code}
>main  ::  IO ()
>main  =   putStrLn "Hello, world!"
>\end{code}
>\end{document}

\endgroup
