F"ur die Benutzer von lhs2TeX.

> module LaTeX (module LaTeX)
> where

> verbatim s			=  putStr (environment "verbatim" s)

> environment			:: String -> String -> String
> environment name m		=  "\\begin{" ++ name ++ "}" ++ m
>				++ "\\end{" ++ name ++ "}"

