%include poly.fmt

%format <| = "\lhd "

> options  ::  [String] -> ([Class],[String])
> options  =   foldr (<|) ([],[])
>   where  "-align"     <| (ds,s:  as) = (Dir Align    s :  ds,     as)
>          ('-':'i':s)  <| (ds,    as) = (Dir Include  s :  ds,     as)
>          ('-':'l':s)  <| (ds,    as) = (Dir Let      s :  ds,     as)
>          s            <| (ds,    as) = (                  ds,s :  as)

