> go	=  concat [ show n ++ ": " ++ [toEnum n] ++ ", \\symbol{" ++ show n ++ "}\\\\\n"
>          | n <- [32 .. 127] ]

> go' = unlines [ concat [ [toEnum i, ' '] | i <- [32 + 24 * n .. 55 + 24 * n] ] | n <- [0 .. 3] ]
