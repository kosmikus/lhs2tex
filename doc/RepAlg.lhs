%include poly.fmt

> rep_alg         =  (\  _          -> \m ->  Leaf m
>                    ,\  lfun rfun  -> \m ->  let  lt  = lfun m
>                                                  rt  = rfun m
>                                             in   Bin lt rt
>                    )
> replace_min' t  =  (cata_Tree rep_alg t) (cata_Tree min_alg t)
