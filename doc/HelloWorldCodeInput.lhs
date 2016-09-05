%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}

>\begin{code}
>main  ::  IO ()
>main  =   putStrLn "Hello, world!"
>\end{code}

\endgroup
