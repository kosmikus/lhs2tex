\documentclass[fleqn]{article}

\usepackage{a4wide}
\usepackage[german,english]{babel}
\usepackage{latexsym}
\usepackage{moreverb}
\usepackage{boxedminipage}

%include lhs2TeX.sty

\title{\texttt{lhs2TeX} \\
       A Pretty-Printer for Haskell\\
       User's manual}
\author{Ralf Hinze}

%-------------------------------=  --------------------------------------------

\begin{document}
\maketitle
\begin{abstract}
Shuhbiduuh.
\end{abstract}

%-------------------------------=  --------------------------------------------
\section{Format of the input file}
%-------------------------------=  --------------------------------------------

Input is a \emph{legal} Haskell literate script; Figure~\ref{fig:sorts}
contains a small example (@Sort.lhs@). A \LaTeX-file is obtained
via
%
\begin{verbatim}
    expand Sort.lhs | unlit | lhs2TeX > Sort.tex
\end{verbatim}
%
The Unix command @expand@ converts tabs to spaces; the
preprocessor @unlit@ converts code sections using bird trails
(@>@) into code sections enclosed betweeen
@\begin{code}@\ldots@\end{code}@. The main work is done by
@lhs2TeX@ which (among other things) converts code sections into
\LaTeX-code. Figure~\ref{fig:sorts.tt} displays the result.
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
\small\input{sorts.tt}
\end{boxedminipage}
\end{center}
\caption{\label{fig:sorts.tt}Sample output (typewriter)}
\end{figure}
%
\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{sorts.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:sorts.math}Sample output (math)}
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
type a single bar here:  both @\verb|@@|@ and @\verb@@|@@@ will
work.

@lhs2TeX@ --- or rather, @unlit@, also allows for inverse
bird tracks (@<@) which mark specification code (alternatively
@\begin{spec}@\ldots@\end{spec}@ may be used). Specification
code is treated by @lhs2TeX@ in exactly the same way as program
code. It only makes a difference to the Haskell compiler which
recognizes |mergeSort| but not |splitAt| (Figure~\ref{fig:sorts}).
Specification code is normally used to give (non-) executable
specifications of functions or to repeat the definition of predefined
functions.

@lhs2TeX@ is able to format Haskell code in different styles:
Figure~\ref{fig:sorts.tt} contains output in \emph{typewriter style}.
The formatting of the input code (especially the spacing) is
essentially retained; keywords are set in bold face and upper-case
indentifiers in italics. Some multi-character symbols such as
@->@, @==@, @&&@ etc are replaced by one-character
symbols.  These are the defaults, Section~\ref{sec:typewriter} explains
how to adopt to your needs.

An alternative format, \emph{math style}, is shown in
Figure~\ref{fig:sorts.math} (obtained via @lhs2TeX -math@). For
this simple example the difference is not that striking:
variable-width fonts are used instead of fixed-width fonts and
`@=>@' is typeset as `|=>|'. The main difference is that the code
is set in math mode which means that \TeX's mathematical typesetting
capabilities are exploited. This implies that spaces in the code are
not preserved: both \verb*|f(a,b)| and \verb*|f (a, b)| are set as |f
(a, b)|. However, the example in Figure~\ref{fig:sorts.math} shows that
left indentatio and internal alignment is preserved (even across
several code sections).  Section~\ref{sec:math} explains math style in
more detail.

@lhs2TeX@ may also be used to extract the code portions of the
input (via @lhs2TeX -code@). This may be convenient if you would
like to distribute the source code only.

Here are the possible calls to @lhs2TeX@.
%
\begin{verbatim}
    lhs2TeX [-tt] [options] [<file>]
    lhs2TeX -math [options] [<file>]
    lhs2TeX -code [options] [<file>]
\end{verbatim}

%-------------------------------=  --------------------------------------------
\section{Format directives}
%-------------------------------=  --------------------------------------------

@lhs2TeX@ can be instructed to typeset identifiers or symbols (in
fact: every Haskell token) in a special way using format directives.
Here are some examples (we tacitly assume that math style is used):
%
{\small\verbatiminput{directives1.snip}}
%include directives1.snip
\noindent
Subsequently, @|f . g|@ results in |f . g|, @|0 `elem` ns|@
results in |0 `elem` ns|, @|sqrt 2|@ is displayed as |sqrt 2|, and
@|pair 1 2|@ produces |pair 1 2|. Note that @lhs2TeX@
distinguishes between infix and prefix uses of identifiers:
@|elem 0 ns|@ is typeset as |elem 0 ns|.

Format directives for Haskell variables and constructors are allowed to
have arguments while directives for Haskell operators must not have
any. This is because @lhs2TeX@ uses a very simple Haskell Parser
which knows nothing of operator precedences or associativity.

You sometimes need parenthesis in your code but not in the printed
output.  For instance, the expression @|sqrt (a + b)|@ should
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
Now, @|f (sqrt (a + b))|@ results in |f (sqrt (a + b))| and
@|f (pair (a + b) c)|@ in |f (pair (a + b) c)|. The format
directive for @power@ shows that it is not always safe to drop
parenthesis.  If @a@ were in parenthesis as well,
@power (a + b) 3@ would result in ${\Varid{a} + \Varid{b}}^{3}$.

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

%-------------------------------=  --------------------------------------------
\section{Conditional directives}
%-------------------------------=  --------------------------------------------

Options: @{-i<file> | -l<var>=<expr>}@.

Bedingte Formatierung: z.B.~zum Ausblenden von Modul-K"opfen.
%
\begin{verbatim}
%if codeOnly || showModuleHeader
\end{verbatim}

%-------------------------------=  --------------------------------------------
\section{Typewriter style}
\label{sec:typewriter}
%-------------------------------=  --------------------------------------------

%-------------------------------=  --------------------------------------------
\section{Math style}
\label{sec:math}
%-------------------------------=  --------------------------------------------

Symbolische Bezeichner wie @<==@, @:^:@ sollten stets mit
@%format@ definiert werden.

%-------------------------------=  --------------------------------------------
\section{Tips and tricks}
%-------------------------------=  --------------------------------------------

Erstreckt sich die linke Seite einer Gleichung "uber die
Alignment-Spalte, so sollte man darauf achten, da"s kein Zeichen in der
Spalte beginnt (ggf.~Leerzeichen einf"ugen).

F"ur ein Projekt eine Format-Datei @<project>.fmt@ anlegen
und alle Module mit @-i<project>.fmt@ "ubersetzen.

Varianten von Funktionen.
%
{\small\verbatiminput{fac.snip}}

%-------------------------------=  --------------------------------------------
\section{Pitfalls}
%-------------------------------=  --------------------------------------------

Der Parser ist sehr einfach gehalten; das kann manchmal zu merkw"urdigen
Effekten f"uhren. Betrachte:
%
{\small\verbatiminput{id.snip}}
{\small\input{id.math}}
%
Der Ausdruck @MkId a >>= f@ wird nicht richtig gesetzt. Dies liegt
daran, da"s der Parser die Instanzdeklaration wie folgt zerlegt
@return a | = | MkId a MkId a | >>= | f = f a@. Um die gew"unschte
Ersetzung zu erzielen, mu"s man das Ende der Definition mit
@{-""-}@ signalisieren.

\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\verbatiminput{cata.snip}
\end{boxedminipage}
\end{center}
\caption{\label{fig:cata}Sample input}
\end{figure}
%
\begin{figure}[p]
\begin{center}
\begin{boxedminipage}{\linewidth}
\small\input{cata.math}
\end{boxedminipage}
\end{center}
\caption{\label{fig:cata.math}Sample output (math)}
\end{figure}

\newpage
%-------------------------------=  --------------------------------------------
\appendix
\section{Agenda}
%-------------------------------=  --------------------------------------------

\begin{itemize}
\item
@\begin{array}{|c|}@, @\begin{tabular}{|c|}@ und @\multicolumn{|c|}@
abfangen (um nicht zwei doppelte Striche eingeben zu m"ussen).

\item
Bug or Feature? 
@\begin{code}@ darf nicht im laufenden Text und @\end{code}@ darf nicht
im formalen Programmtext auftreten: z.B.~@"\end{code}"@ mu"s als
@"\end\&{code}"@ eingegeben werden. M"ogliche L"osung:
Pseudo-Kommentare immer an den Zeilenanfang. Dies w"urde vielleicht
auch den Einleseproze"s beschleunigen?

\item
Feature: @--@ does not immediately start a comment.

\item
Verbatim: Space-Korrektur, bei Ersetzungen @->@ durch |->|.
Vielleicht: einfache Vereinbarung: Zwischenraum der mehr als zwei
Leerzeichen enth"alt ist dehnbar und kann ggf.~vergr"o"sert werden.
Also vor @=@ stets zwei Gleichheitszeichen belassen.
Hilft nicht, wenn |->| in der mittleren Spalte auftritt. Doch, beim
folgenden Eingabeformat:
\begin{verbatim*}
square    ::  (Num a) => a -> a
square a  =   a * a
\end{verbatim*}
Also: vorher und nachher zwei Leerzeichen.

\item
Module |lexer|: Funktionen, mit deren Hilfe der zu parsende String
gesetzt bzw.~bestimmt werden kann.

\item
Die det.~Parser f"ur den Haskell-Lexer verwenden.

\item
Endliche Abbildungen mit Tries!!

\item
Indexing

\item
Fit f"ur \LaTeX~2.09 machen @%if latex209@

\item Die Mathe-Formatierung auf Grundlage von |Doc| machen.
\end{itemize}

\newpage
%-------------------------------=  --------------------------------------------
\section{\texttt{lhs2TeX}}
%-------------------------------=  --------------------------------------------

\input{TeXCommands}
\input{TeXParser}

\input{Document}
\input{Verbatim}
\input{Typewriter}
\input{Math}
\input{Directives}
\input{HsLexer}

\input{Parser}
\input{StateT}
\input{Value}
\input{FiniteMap}
\input{Auxiliaries}

\input{Main}

\newpage
\input{lhs2TeXsty.tex}
\input{lhs2TeXfmt.tex}
\input{Makefile}

\newpage
\tableofcontents

\end{document}
