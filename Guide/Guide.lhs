\documentclass{article}

\usepackage{moreverb}
\usepackage{boxedminipage}

%include ../lhs2TeX.sty
%let meta                       =  True
%let array                      =  True
%include ../lhs2TeX.fmt

\title{A Quick Guide to @lhs2TeX@}
\author{RALF HINZE
\\Institut f\"{u}r Informatik III, Universit\"{a}t Bonn
\\R\"{o}merstra\ss e 164, 53117 Bonn, Germany
\\@ralf@@informatik.uni-bonn.de@}

%-------------------------------=  --------------------------------------------
%if False

Private remarks or uninteresting code segments com here.

%endif
%-------------------------------=  --------------------------------------------

\begin{document}

\maketitle

\begin{abstract}
@lhs2TeX@ is a preprocessor for typesetting Haskell programs (well not
just Haskell) that combines some of the good features of @pphs@ and
@smugweb@.
\end{abstract}

%-------------------------------=  --------------------------------------------
\section{Installing @lhs2TeX@}
%-------------------------------=  --------------------------------------------

Adjust @binpath@ in the Makefile and type @make all@. That's it!?

%-------------------------------=  --------------------------------------------
\section{A quick guide to @lhs2TeX@}
%-------------------------------=  --------------------------------------------

Input is a \emph{legal} Haskell literate script; Figure~\ref{fig:sorts}
contains a small example (@Sort.lhs@). A \LaTeX-file is obtained
via
%
\begin{verbatim}
    lhs2TeX -math Sort.lhs > Sort.tex
\end{verbatim}
%
@lhs2TeX@ converts among other things code sections (identified by Bird
tracks) into \LaTeX-code. Figure~\ref{fig:sorts.math} displays the
result.
%
\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{sorts.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:sorts}Sample input}
\end{figure}
%
\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{sorts.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:sorts.math}Sample output}
\end{figure}

The example shows that the input file may also contain inline code
enclosed between vertical bars: @|mergeSort [5, 2, 7, 3]|@. Some
remarks are appropriate: @lhs2TeX@ uses a very simple parser; a
vertical bar always starts inline code, the next vertical bar
terminates it. This implies that a special convention is required if
the normal text or the code fragment shall contain a vertical bar. We
arrange that a single bar is represented by a double bar: the logical
disjunction `@||@' consequently requires four keystrokes. You must
use double bars, for instance, in column entries:
@\begin{array}{||c||}@ means @\begin{array}{|c|}@. There is
one exception to this rule. Since @lhs2TeX@ recognizes
@\verb@-commands and @verbatim@-environments you only have to
type a single bar here: both @\verb|@@|@ and @\verb@@|@@@ will
work.

Typewriter text is written as follows \verb|@typewriter@|. The same
remarks apply here: to get one \verb|@| you must type two.

@lhs2TeX@ also allows for inverse bird tracks (@<@) which mark
specification code. Specification code is treated by @lhs2TeX@ in
exactly the same way as program code. It only makes a difference to the
Haskell compiler which recognizes |mergeSort| but not |splitAt|
(Figure~\ref{fig:sorts}).  Specification code is normally used to give
(non-) executable specifications of functions or to repeat the
definition of predefined functions.

@lhs2TeX@ can be instructed to typeset identifiers or symbols (in
fact: every Haskell token) in a special way using format directives.
Here are some examples:
%
{\small\verbatiminput{directives1.snip}}
%include directives1.snip
\noindent
Subsequently, @|f . g|@ results in |f . g|, @|0 `elem` ns|@ results in
|0 `elem` ns|, @|sqrt 2|@ is displayed as |sqrt 2|, and @|pair 1 2|@
produces |pair 1 2|. Note that @lhs2TeX@ distinguishes between infix
and prefix uses of identifiers:  @|elem 0 ns|@ is typeset as |elem 0
ns|.

Format directives for Haskell variables and constructors are allowed to
have arguments while directives for Haskell operators must not have
any. This is because @lhs2TeX@ uses a very simple Haskell Parser which
knows nothing of operator precedences or associativity.

You sometimes need parenthesis in your code but not in the printed
output. For instance, the expression @|sqrt (a + b)|@ should
result in $\sqrt{\Varid{a} + \Varid{b}}$ rather than |sqrt (a + b)|.
Putting an argument in parenthesis means that it is safe to drop
parenthesis around the expansion of this argument. The desired
behaviour is obtained via
%
{\small\verbatiminput{directives2.snip}}
%include directives2.snip
\noindent
However, @|f (sqrt a)|@ still results in |f (sqrt a)|. To instruct
@lhs2TeX@ that it is safe to drop parenthesis around the expanded
macro you must put the left hand side in parenthesis.
%
{\small\verbatiminput{directives3.snip}}
%include directives3.snip
\noindent
Now, @|f (sqrt (a + b))|@ results in |f (sqrt (a + b))| and @|f (pair
(a + b) c)|@ in |f (pair (a + b) c)|. The format directive for @power@
shows that it is not always safe to drop parenthesis. If @a@ were in
parenthesis as well, @power (a + b) 3@ would result in ${\Varid{a} +
\Varid{b}}^{3}$.

\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{spec.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:spec}Meta Haskell (input)}
\end{figure}
%
\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{spec.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:spec.math}Meta Haskell (output)}
\end{figure}

Block structure with @%{@ and @%}@ \ldots

@lhs2TeX@ automatically aligns code and specification sections. By
default the alignment column is $33$. You can adjust to your needs
using, for example, @%align 25@ (which again looks like a
\TeX\ comment).

@lhs2TeX@ recognizes a numer of C-preprocessor-like directives: @%if@,
@%then@, @%else@, @%elif@, @%fi@, @%let@, @%include@.

You can also call @Hugs@ \ldots

%-------------------------------=  --------------------------------------------
\section{Pitfalls}
%-------------------------------=  --------------------------------------------

\begin{itemize}
\item
Don't forget to use @||@ if you need @|@ in ordinary \LaTeX\ text
(especially in @array@ or @tabular@ environments). Ditto \verb|@@|
(especially in your e-mail address).

\item
Note that @`@ (backquote) is a meta character in @%format@ directives.

\item
(Haskell) expressions must be properly bracketed.

\item
Don't forget to @%include@ both @lhs2TeX.fmt@ and @lhs2TeX.sty@.
\end{itemize}

\end{document}
