%include verbatim.fmt
\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1#2{\origtt}

>\begin{spec}
>main  ::  IO ()
>main  =   putStrLn "Hello, world!"
>\end{spec}

\endgroup
