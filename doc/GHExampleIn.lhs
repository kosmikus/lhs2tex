%include verbatim.fmt
\begingroup
\let\origtt=\tt
\let\small\scriptsize
\def\tt#1#2{\origtt\makebox[0pt]{\phantom{X}}}

>%if style == newcode
>%format ^
>%format ^^    = " "
>%format ti(a) = "{|" a "|}"
>%format ki(a) = "{[" a "]}"
>%else
>%format ^     = " "
>%format ^^    = "\;"
>%format ti(a) = "\lty " a "\rty "
>%format ki(a) = "\lki " a "\rki "
>\newcommand{\lty}{\mathopen{\{\mskip-3.4mu||}}
>\newcommand{\rty}{\mathclose{||\mskip-3.4mu\}}}
>\newcommand{\lki}{\mathopen{\{\mskip-3.5mu[}}
>\newcommand{\rki}{\mathclose{]\mskip-3.5mu\}}}
>%format t1
>%format t2
>%format a1
>%format a2
>%format r_    = "\rho "
>%format s_    = "\sigma "
>%format k_    = "\kappa "
>%format forall a = "\forall " a
>%format .     = "."
>%format mapa  = map "_{" a "}"
>%format mapb  = map "_{" b "}"
>%format :*:   = "\times "
>%endif
>\begin{code}
>type Map^ki(*)         t1  t2       =   t1 -> t2
>type Map^ki(r_ -> s_)  t1  t2       =   forall a1 a2. Map^ki(r_) a1 a2 
>                                          -> Map^ki(s_) (t1 a1) (t2 a2)
>
>map^ti(t :: k_)                     ::  Map^ki(k_) t t
>map^ti(Unit)             Unit       =   Unit
>map^ti(Int)              i          =   i
>map^ti(Sum)   mapa mapb  (Inl  a)   =   Inl  (mapa a)
>map^ti(Sum)   mapa mapb  (Inr  b)   =   Inr  (mapb b)
>map^ti(Prod)  mapa mapb  (a :*: b)  =   mapa a :*: mapb b
>\end{code}

\endgroup
