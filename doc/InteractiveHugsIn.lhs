%include verbatim.fmt

\begingroup
\let\origtt=\ttfamily
\def\ttfamily#1{\origtt}
%if False
module InteractiveHugs where
%endif
%format . = "."
%format forall a = "\forall" a

\begin{code}
%options hugs -98

> fix    ::  forall a. (a -> a) -> a
> fix f  =   f (fix f)

This function is of type \eval{:t fix},
and |take 10 (fix ('x':))| 
evaluates to \eval{take 10 (fix ('x':))}.
\end{code}
\endgroup
