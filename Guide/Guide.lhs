\documentclass{article}

\usepackage{moreverb}
\usepackage{alltt}
\usepackage{boxedminipage}

%include ../lhs2TeX.sty
%let meta                       =  True
%let array                      =  True
%let doc                        =  True
%include ../lhs2TeX.fmt
%include ../Version.lhs

\title{A Quick Guide to @lhs2TeX@}
\author{RALF HINZE
\\Institut f\"{u}r Informatik III, Universit\"{a}t Bonn
\\R\"{o}merstra\ss e 164, 53117 Bonn, Germany
\\@ralf@@informatik.uni-bonn.de@
\\[2ex]ANDRES L\"{O}H
\\Institute of Information and Computing Sciences
\\Utrecht University, P.O.~Box 80.089\\3508\,TB Utrecht, The Netherlands
\\@andres@@cs.uu.nl@}

%-------------------------------=  --------------------------------------------
%if False

Private remarks or uninteresting code segments go here.

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

These instructions apply to Unix-like environments:

Unpack the archive. Assume that it has been unpacked into directory
@/somewhere@. Then say
\begin{alltt}
    cd /somewhere/lhs2TeX-\ProgramVersion
    ./configure
    make
    make install
\end{alltt}
You might need administrator permissions to perform the @make install@
step. Alternatively, you can select your own installation location by
passing the @--prefix@ argument to @configure@:
\begin{verbatim}
    ./configure --prefix=/my/local/programs
\end{verbatim}

@lhs2TeX@ needs to find two configuration files,
@lhs2TeX.sty@ and @lhs2TeX.fmt@ after installation. By default,
@lhs2TeX@ looks in the following directories, in order:
\begin{verbatim}
    .
    $HOME/lhs2TeX
    $HOME/.lhs2TeX
    $LHS2TEX
    /usr/local/share/lhs2tex
    /usr/local/share/lhs2TeX
    /usr/local/lib/lhs2tex
    /usr/local/lib/lhs2TeX
    /usr/share/lhs2tex
    /usr/share/lhs2TeX
    /usr/lib/lhs2tex
    /usr/lib/lhs2TeX
\end{verbatim}
If you want to install @lhs2TeX@ to a location where the
configuration files cannot be found automatically, it is
thus easies to set the environment variable @LHS2TEX@
to point to the correct directory.

If you desire to use the (not yet documented) @poly@ mode
of @lhs2TeX@ (\emph{and only in this case!}), 
you must manually install the files @polytable.sty@
and @lazylist.sty@ (they are included in the distribution)
in such a way that they will be found by your \TeX\ installation.
Assuming that you have a @texmf@ tree at @/usr/local/share/texmf@,
this can usually be achieved by placing the files in the
directory @/usr/local/share/texmf/tex/latex/polytable@,
and then running 
\begin{verbatim}
    mktexlsr
\end{verbatim} 
to update the \TeX\ filename database.

%-------------------------------=  --------------------------------------------
\section{A quick guide to @lhs2TeX@}
%-------------------------------=  --------------------------------------------

\subsection{Processing a file}

Input is a \emph{legal} Haskell literate script; Figure~\ref{fig:sorts}
contains a small example (the file @Sort.lhs@). A \LaTeX-file is obtained
via
%
\begin{verbatim}
    lhs2TeX --math Sort.lhs > Sort.tex
\end{verbatim}
%
@lhs2TeX@ converts among other things code sections (identified by Bird
tracks) into \LaTeX-code. Figure~\ref{fig:sorts.math} displays the
result.
%
\begin{figure}[htbp]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{sorts.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:sorts}Sample input}
%\end{figure}
\bigskip
%\begin{figure}[p]
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

Typewriter (verbatim) text is written as 
follows: \verb|@typewriter@|. The same
remarks apply here: to get one \verb|@| you must type two.

@lhs2TeX@ also allows for inverse bird tracks (@<@) which mark
specification code. Specification code is treated by @lhs2TeX@ in
exactly the same way as program code. It only makes a difference to the
Haskell compiler which recognizes |mergeSort| but not |splitAt|
(Figure~\ref{fig:sorts}).  Specification code is normally used to give
(non-) executable specifications of functions or to repeat the
definition of predefined functions. 

@lhs2TeX@ can also process literate scripts where the code is
enclosed by \LaTeX-style @\begin{code}@ and @\end{code}@ commands.

\subsection{Including files}

With @lhs2TeX@, one can include other source files
by means of an
@%include@ directive. All @lhs2TeX@ directives begin on
a new line and start with a @%@ character, to make them
appear as a \TeX\ comment. Directives may occur anywhere
in the file except in code blocks. Include directives
cause the specified file to be included literally at the
position of the directive. The contents of the file will
also be processed by @lhs2TeX@.

To successfully use @lhs2TeX@, it is, in fact, \emph{necessary}
to include at least two files, @lhs2TeX.fmt@ and @lhs2TeX.sty@,
as with
%
{\small\verbatiminput{directives0.snip}}
\noindent
%
These commands should be placed in the preamble of the document,
i.e.~between the @\documentclass@ and the @\begin{document}@ commands.
These files contain basic directives and \TeX\ commands that
are required for the preprocessor to work correctly.

Alternatively, one can use the @-i@ command line option of @lhs2TeX@
to include these files.

\subsection{Formatting directives}

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
any. This is because @lhs2TeX@ uses a very simple Haskell parser which
knows nothing of operator precedences or associativity.

You sometimes need parentheses in your code but not in the printed
output. For instance, the expression @|sqrt (a + b)|@ should
result in $\sqrt{\Varid{a} + \Varid{b}}$ rather than |sqrt (a + b)|.
Putting an argument in parentheses means that it is safe to drop
parentheses around the expansion of this argument. The desired
behaviour is obtained via
%
{\small\verbatiminput{directives2.snip}}
%include directives2.snip
\noindent
However, @|f (sqrt a)|@ still results in |f (sqrt a)|. To instruct
@lhs2TeX@ that it is safe to drop parentheses around the expanded
macro you must put the left hand side in parentheses.
%
{\small\verbatiminput{directives3.snip}}
%include directives3.snip
\noindent
Now, @|f (sqrt (a + b))|@ results in |f (sqrt (a + b))| and @|f (pair
(a + b) c)|@ in |f (pair (a + b) c)|. The format directive for @power@
shows that it is not always safe to drop parenthesis. If @a@ were in
parenthesis as well, @power (a + b) 3@ would result in ${\Varid{a} +
\Varid{b}}^{3}$.

\begin{figure}[htbp]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{spec.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:spec}Meta Haskell (input)}
%\end{figure}
\bigskip
%\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{spec.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:spec.math}Meta Haskell (output)}
\end{figure}

Formatting directives usually affect everything that comes after
them. However, a set of directives can be made local by enclosing
it in a block with @%{@ and @%}@. A local directive has no effect
after the current block ends. The blocks of @lhs2TeX@ do \emph{only}
group formatting directives, nothing else: no \LaTeX\ commands,
no other directives (@%align@, @%if@, @%subst@ or whatever).
Figures~\ref{fig:block} and \ref{fig:block.math} show an example.
Blocks can be nested.

\begin{figure}[htbp]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{block.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:block}Block structure (input)}
\bigskip
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{block.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:block.math}Block structure (output)}
\end{figure}

As of now, the right hand sides of formatting directives consist
of a space separated list that may contain snippets of \TeX\ code
that are enclosed between double quotes, arguments, and other
tokens. These other tokens may refer to formatting directives again,
which will be applied, but \emph{only if they have no arguments}.
%
{\small\verbatiminput{directives4.snip}}
%include directives4.snip
\noindent
After these directives, @|vecx|@ results in |vecx|, whereas
@|vecy|@ results in |vecy| instead of |vec(y)|.

\subsection{Alignment}

@lhs2TeX@ automatically aligns code and specification sections. By
default the alignment column is $33$. You can adjust to your needs
using, for example, @%align 25@ (which again looks like a
\TeX\ comment). This behaviour can be observed in 
Figures~\ref{fig:sorts.math} and \ref{fig:spec.math}. In the input
to the latter example, shown in Figure~\ref{fig:spec}, there
is also an example of the alignment column being changed.


\subsection{Conditional preprocessing}

@lhs2TeX@ recognizes a numer of C-preprocessor-like directives: @%if@,
@%then@, @%else@, @%elif@, @%fi@, @%let@.
With @%let@, one can set boolean or integer toggles that can be tested
in conditionals.
Internally, @lhs2TeX@ uses toggles to support different behaviours.
Also, some variables are predefined so that your documents can show
different behaviour under different circumstances. 

\subsection{Calling Hugs}

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
