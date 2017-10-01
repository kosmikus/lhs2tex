%-------------------------------=  --------------------------------------------
\subsection{Value type}
%-------------------------------=  --------------------------------------------

%if codeOnly || showModuleHeader

> module Value (module Value)
> where

%endif

> data Value                    =  Undef
>                               |  Str  String
>                               |  Bool Bool
>                               |  Int  Int

Dynamic conversion routines.

> str                           :: Value -> String
> str Undef                     =  ""
> str (Str s)                   =  s
> str (Bool b)                  =  if b then "True" else ""
> str (Int i)                   =  show i
>
> bool                          :: Value -> Bool
> bool Undef                    =  False
> bool (Str s)                  =  not (null s)
> bool (Bool b)                 =  b
> bool (Int i)                  =  i /= 0
>
> int                           :: Value -> Int
> int Undef                     =  0
> int (Str s)                   =  case reads s of
>     [(i, [])]                 -> i
>     _                         -> 0
> int (Bool b)                  =  if b then 1 else 0
> int (Int i)                   =  i

Lifting unsary and binary operations to |Value|.

> type Unary a                  =  a -> a
>
> onStr1                        :: Unary String -> Unary Value
> onStr1 f                      =  Str . f . str
>
> onBool1                       :: Unary Bool -> Unary Value
> onBool1 f                     =  Bool . f . bool
>
> onInt1                        :: Unary Int -> Unary Value
> onInt1 f                      =  Int . f . int

%{
%format s1
%format s2
%format b1
%format b2
%format i1
%format i2

> type Binary a                 =  a -> a -> a
>
> onStr2                        :: Binary String -> Binary Value
> onStr2 op v1 v2               =  Str (str v1 `op` str v2)
>
> onBool2                       :: Binary Bool -> Binary Value
> onBool2 op v1 v2              =  Bool (bool v1 `op` bool v2)
>
> onInt2                        :: Binary Int -> Binary Value
> onInt2 op v1 v2               =  Int (int v1 `op` int v2)

%align 41
{\setlength{\lwidth}{5.5cm}

> onMatching ::
>   (String -> String -> Bool)
>   -> (Bool -> Bool -> Bool)
>   -> (Int -> Int -> Bool)
>   -> Value -> Value -> Value
> onMatching  f _g _h Undef     (Str s2)   =  Bool (f (str Undef) s2)
> onMatching  f _g _h (Str s1)  Undef      =  Bool (f s1 (str Undef))
> onMatching  f _g _h (Str s1)  (Str s2)   =  Bool (f s1 s2)
> onMatching _f  g _h Undef     (Bool b2)  =  Bool (g (bool Undef) b2)
> onMatching _f  g _h (Bool b1) Undef      =  Bool (g b1 (bool Undef))
> onMatching _f  g _h (Bool b1) (Bool b2)  =  Bool (g b1 b2)
> onMatching _f _g  h Undef     (Int i2)   =  Bool (h (int Undef) i2)
> onMatching _f _g  h (Int i1)  Undef      =  Bool (h i1 (int Undef))
> onMatching _f _g  h (Int i1)  (Int i2)   =  Bool (h i1 i2)
> onMatching  _  _  _ _         _          =  Bool False

}
%}
