\documentclass[fleqn]{article}

\usepackage[german]{babel}
\usepackage{moreverb}
\usepackage{boxedminipage}
\parindent0cm

\newcommand{\Show}[1]{\listinginput{1}{#1.snip}\input{#1.tex}}

%-------------------------------=  --------------------------------------------

%include ../lhs2TeX.sty

%-------------------------------=  --------------------------------------------

\begin{document}

%-------------------------------=  --------------------------------------------
\section{String gaps}
%-------------------------------=  --------------------------------------------

\Show{gap}

%-------------------------------=  --------------------------------------------
\section{Identifiers}
%-------------------------------=  --------------------------------------------

\Show{idents}

%-------------------------------=  --------------------------------------------
\section{Operators}
%-------------------------------=  --------------------------------------------

\Show{operators}

%-------------------------------=  --------------------------------------------
\section{Special symbols}
%-------------------------------=  --------------------------------------------

\Show{special}

%-------------------------------=  --------------------------------------------
\section{Spacing}
%-------------------------------=  --------------------------------------------

\Show{spacing}
\Show{braces}

%-------------------------------=  --------------------------------------------
\section{Indentation}
%-------------------------------=  --------------------------------------------

\Show{indentation}
%
\NB Die senkrechten Striche sind im ersten Beispiel etwas einger"uckt,
da \verb|\mid| als Operator gesetzt wird. Schlie"st man die linke Seite
in \verb|{..}| ein, dann wird |weird| nicht richtig gesetzt. Die rechte
Seite \emph{wird} in \verb|{..}| eingeschlossen, damit |list| und
|main| richtig gesetzt werden (auf diese Weise verliert \verb|\mid|
seinen Status als Operator).

%-------------------------------=  --------------------------------------------
\section{Format directives}
%-------------------------------=  --------------------------------------------

\Show{format}
\Show{parens}

%-------------------------------=  --------------------------------------------
\section{Errors}
%-------------------------------=  --------------------------------------------

\Show{errors}

%-------------------------------=  --------------------------------------------
\section{Meta-Haskell}
%-------------------------------=  --------------------------------------------

Erfordert \verb|-l'meta = True'| Kommandozeilenoption (wichtig: \emph{vor}
\verb@-i lhs2TeX.fmt@ angeben).
%
\Show{meta}

%-------------------------------=  --------------------------------------------
\section{Comments}
%-------------------------------=  --------------------------------------------

\Show{comments}

%-------------------------------=  --------------------------------------------
\section{Verbatim}
%-------------------------------=  --------------------------------------------

\verb|khadrkh| und \verb*|kjhsfd  kjghsdf|.
%
\begin{verbatim}
bass  sdakh asd asd
\end{verbatim}
%
\begin{verbatim*}
bass  sdakh asd asd
\end{verbatim*}

%-------------------------------=  --------------------------------------------
\section{Active commands}
%-------------------------------=  --------------------------------------------

|product [1..20]| yields \eval{product [1..20]}.
%if False

> default (Integer)

> group n			=  map (take n)
>				.  takeWhile (not . null)
>				.  iterate (drop n)
> rows				=  concat
>				.  intersperse " \\\\\n"
>				.  map (\(n, s) -> show n ++ " & " ++ s)
>				.  zip [1 ..]
>				.  group 60
> intersperse s []		=  []
> intersperse s [a]		=  [a]
> intersperse s (a1 : a2 : as)	=  a1 : s : intersperse s (a2 : as)
> out n				=  putStr (rows (show n))

%endif
|product [1..200]| yields
\[
\begin{array}{rl}
    \perform{out (product [1..200])}
\end{array}
\]

> twice f a			=  f (f a)

\eval{:type twice} und \eval{:type twice twice} haben den gleichen
Typ.

%-------------------------------=  --------------------------------------------
\section{Active commands with @ghci@}
%-------------------------------=  --------------------------------------------

%options ghci -fglasgow-exts -fno-monomorphism-restriction
|product [1..20]| yields \eval{product [1..20]}.
%if False

> main = undefined

%endif
|product [1..200]| yields
\[
\begin{array}{rl}
    \perform{out (product [1..200])}
\end{array}
\]

< twice f a			=  f (f a)

%format forall = "\forall "
%format .      = "."
\eval{:type twice} und \eval{:type twice twice} haben den gleichen
Typ.

\end{document}
