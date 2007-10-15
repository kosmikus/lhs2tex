\documentclass[10pt]{scrartcl}

% save linebreak; see below
\let\origlinebreak=\\

\renewcommand{\sectfont}{\bf}

%let framed = False

\usepackage[english]{babel}
%\usepackage[fleqn]{amsmath}
\usepackage{stmaryrd}

\usepackage{hyperref}

%\usepackage[T1]{fontenc}
\usepackage{mathpazo}
%\usepackage[scaled=0.9]{luximono}
\usepackage{colortbl}
\usepackage{calc}
%\usepackage{pifont}
\usepackage{paralist}
\usepackage{ifthen}
\usepackage{relsize}
\usepackage{xspace}
\usepackage{tabularx}
%if framed
\usepackage{framed}
\FrameSep=2\fboxsep
%endif

\newcommand*{\PDF}{{\smaller{PDF}}\xspace}
\newcommand*{\CTAN}{{\smaller{CTAN}}\xspace}
\setdefaultitem{\textbf{--}}{}{}{}

\let\defined\textbf

%let doc = True
%include lhs2TeX.fmt
%include Version.lhs

\newlength{\lwidth}
\newlength{\cwidth}
\setlength{\lwidth}{0pt}
\setlength{\cwidth}{0pt}

%separation 2
%latency 2

\let\origcolor=\color
\newcommand{\dep}[1]{{\origcolor{red}#1}}
\def\swgt#1{\switch[\value{step}>#1]}%
\def\ro#1{\ifthenelse{\value{step}=#1}{\origcolor{red}}{}}%

%\usepackage[display]{texpower}

%hyperref needs some setup, especially after pdfscreen
\hypersetup{%
  colorlinks=True,%
  pdfmenubar=True,%
  pdfcenterwindow=False,% 
  pdffitwindow=False}%

%fixed lengths are better ... 
% \AtBeginDocument{%
% \setlength{\abovedisplayskip}{6pt plus 0pt minus 0pt}% originally 10.0pt plus 2.0pt minus 5.0pt
% \setlength{\belowdisplayskip}{6pt plus 0pt minus 0pt}% originally 10.0pt plus 2.0pt minus 5.0pt
% }
% \setlength{\belowdisplayshortskip}{6pt plus 0pt minus 0pt}%
% \setlength{\abovedisplayshortskip}{6pt plus 0pt minus 0pt}%
% \setlength{\smallskipamount}{2pt}
% \setlength{\medskipamount}{5pt}
% \setlength{\bigskipamount}{10pt}
% 
% 
% \setlength\pltopsep{2pt}
% \setlength\plitemsep{1pt}
% \setlength\parskip{0pt}

\newcounter{pagesave}

% redefining the lhs2TeX code command is needed because
% TeXpower seems to tamper with \\ in some nasty way ...

% This one works:
%%subst code a = "\begingroup\parskip=\abovedisplayskip\par\advance\leftskip\mathindent\let\\=\origlinebreak\('n\begin{pboxed}\SaveRestoreHook'n" a "\ColumnHook'n\end{pboxed}'n\)\parskip=\belowdisplayskip\par\endgroup\resethooks'n"

% This one is with color:
%subst code a = "\begin{colorcode}'n" a "\end{colorcode}\resethooks'n" 

\definecolor{rlcolor}{gray}{.8}
\arrayrulecolor{rlcolor}
\definecolor{hcolor}{gray}{.7}

% \newenvironment{colorcode}{%
%   \parskip=\abovedisplayskip\par\noindent
%   \begingroup\small% small changes displayskips!
% %if color
%   \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
% %elif framed
%   \framed
% %else
%   \tabular{@@{}||p{\linewidth-2\arraycolsep-2\arrayrulewidth-2pt}||@@{}}%
%   \hline \\[-1.5ex]
%   \let\myendofline=\\
% %endif
%   \let\\=\origlinebreak
%   \(%
%   \pboxed\SaveRestoreHook}{%
%   \ColumnHook\endpboxed
%   \)%
% %if not color && not framed
%   \myendofline[.5ex]\hline
% %endif
% %if framed
%   \endframed
% %else
%   \endtabular
% %endif
%   \endgroup
%   \parskip=\belowdisplayskip\par\noindent
%   \ignorespacesafterend}

\newenvironment{colorcode}{%
  \colorsurround
  \(%
  \pboxed\SaveRestoreHook}{%
  \ColumnHook\endpboxed
  \)%
  \endcolorsurround}

% \newenvironment{colorsurround}{%
%   \parskip=\abovedisplayskip\par\noindent
%   \begingroup\small% small changes displayskips!
% %if color
%   \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
% %elif framed
%   \framed
% %else
%   \tabular{@@{}||p{\linewidth-2\arraycolsep-2\arrayrulewidth-2pt}||@@{}}%
%   \hline \\[-1.5ex]
%   \let\myendofline=\\
% %endif
%   \let\\=\origlinebreak}{%
% %if not color && not framed
%   \myendofline[.5ex]\hline
% %endif
% %if framed
%   \endframed
% %else
%   \endtabular
% %endif
%   \endgroup
%   \parskip=\belowdisplayskip\par\noindent
%   \ignorespacesafterend}

\newenvironment{colorsurround}{\colorverb}{\endcolorverb}

% \newenvironment{colorarray}{%
%   \parskip=\abovedisplayskip\par\noindent
%   \begingroup\small% small changes displayskips!
% %if color
%   \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
% %elif framed
%   \framed
% %else
%   \tabular{@@{}||p{\linewidth-2\arraycolsep-2\arrayrulewidth-2pt}||@@{}}%
%   \hline \\[-1.5ex]
%   \let\myendofline=\\
% %endif
%   \let\\=\origlinebreak
%   \(%
%   \array}{%
%   \endarray
%   \)%
% %if not color && not framed
%   \myendofline[.5ex]\hline
% %endif
% %if framed
%   \endframed
% %else
%   \endtabular
% %endif
%   \endgroup
%   \parskip=\belowdisplayskip\par\noindent
%   \ignorespacesafterend}

\newenvironment{colorarray}{%
  \colorsurround
  \(%
  \array}{%
  \endarray
  \)%
  \endcolorsurround}

\makeatletter
\newenvironment{colorverb}{%
  \parskip=\abovedisplayskip\par\noindent
  \begingroup\small% small changes displayskips!
%if color
  \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
%elif framed
  \framed
%else
  \tabular{@@{}||p{\linewidth-2\arraycolsep-2\arrayrulewidth-2pt}||@@{}}%
  \hline \\[-1.5ex]
  \let\myendofline=\\
%endif
  \let\\=\origlinebreak}{%
%if not color && not framed
  \myendofline[.5ex]\hline
%endif
%if framed
  \endframed
%else
  \endtabular
%endif
  \endgroup
  \parskip=\belowdisplayskip\par\noindent
  \ignorespacesafterend}
\makeatother

%%%
%%% "IMPORTANT" ENVIRONMENT
%%%

\newenvironment{important}[1][Important]%
  {\colorsurround
   \centering
   \bfseries\textsc{#1:}\ }%
  {\endcolorsurround}

%\definecolor{codecolor}{rgb}{.982, .902, .902}% original
%\definecolor{codecolor}{rgb}{1,.898,.667}% so'n orange
\definecolor{codecolor}{rgb}{1,1,.667}

%format forall(a) = "\forall " a "\relax"

%\usepackage{fonttabl}

\begin{document}

%\begingroup
%\texfamily
%\fonttable
%\endgroup

\title{@Guide2lhs2TeX@\\
  \smaller (for version \ProgramVersion)}
\author{{Ralf Hinze and Andres L\"oh}\\
  \smaller \tabular{c}
           Institut f\"ur Informatik III, Universit\"at Bonn\\
           R\"omerstra\ss e 164, 53117 Bonn, Germany\\
           \verb|{ralf,loeh}@informatik.uni-bonn.de|
           \endtabular}%
\date{\today}
\maketitle

\tableofcontents

%---------------------------------------------------------------------------
\section{About @lhs2TeX@}
%---------------------------------------------------------------------------

The program @lhs2TeX@ is a preprocessor
that takes as input a literate Haskell source file
(or something sufficiently alike) and produces as
output a formatted file that can be further processed
by \LaTeX.

For example, consider the following input file:
\input{HelloWorldInput}
If we run the following two commands on it
\input{HelloWorldDialogue}
then the resulting \PDF file will look similar to
\begin{colorsurround}
\input{HelloWorld}
%if color
\vspace*{-2\baselineskip}%
%endif
\end{colorsurround}
%if color
\vspace*{\belowdisplayskip}%
\par\noindent
%endif
The behaviour of @lhs2TeX@ is highly customizable.
For example, the argument @--poly@ in the call above
specifies one of several different \defined{style}s.
Depending on the selected style, @lhs2TeX@ can perform
quite different tasks. Here is a brief overview:
\begin{compactitem}
  \item \textbf{verb} (verbatim): format code completely verbatim
  \item \textbf{tt} (typewriter): format code verbatim, but allow special
    formatting of keywords, characters, some functions, \dots
  \item \textbf{math}: mathematical formatting with basic alignment,
    highly customizable
  \item \textbf{poly}: mathematical formatting with mutliple alignments,
    highly customizable, supersedes \textbf{math}
  \item \textbf{code}: delete all comments, extract sourcecode
  \item \textbf{newcode} (new code): delete all comments, extract sourcecode,
    but allow for formatting, supersedes \textbf{code}
\end{compactitem}

%%%
%%%

%---------------------------------------------------------------------------
\section{Installing @lhs2TeX@}
%---------------------------------------------------------------------------

There are two possibilities to install @lhs2TeX@:
\begin{compactitem}
\item Using Cabal.
\item Classic configure/make.
\end{compactitem}

\subsection{Using Cabal to install @lhs2TeX@}

This requires Cabal 1.1.6 or later. The process is then as usual:
\input{CabalInstallation}%
The third step requires write access to the installation location
and the \LaTeX\ filename database.

\subsection{configure/make}

The following instructions apply to Unix-like environments.  However,
@lhs2TeX@ does run on Windows systems, too. (If you would like to add
installation instructions or facilitate the installation procedure for
Windows systems, please contact the authors.)

Unpack the archive. Assume that it has been unpacked into directory
@/somewhere@. Then say
\input{InstallationInstructions}%
You might need administrator permissions to perform the @make install@
step. Alternatively, you can select your own installation location by
passing the @--prefix@ argument to @configure@:
\input{ConfigureCall}

With @lhs2TeX@ come a couple of library files (containing basic 
@lhs2TeX@ formatting directives) that need to be found by the
@lhs2TeX@ binary. The default search path is as follows:
\input{SearchPath}%
\label{defaultsearchpath}%
Here, @{HOME}@ and @{LHS2TEX}@ denote the current values of
the environment variables @HOME@ and @LHS2TEX@. The double slash
at the end of each dir means that subdirectories are also scanned.
If @lhs2TeX@ is installed
to a non-standard path, you might want to set the environment
variable @LHS2TEX@ to point to the directory where
@lhs2TeX.fmt@ and the other library files have been installed to.

\begin{important}
To be able to use ``poly'' style, the two \LaTeX\ 
packages\\ @polytable.sty@ and @lazylist.sty@ are required!
\end{important}
Both are included
in the @lhs2TeX@ distribution (they are not part of standard
\LaTeX\ distributions, although they are available from 
\CTAN~\cite{polytable,lazylist}),
and are usually installed during the normal procedure. The
@configure@ script will determine whether a suitably recent
version of @polytable@ is installed on your system, and if
necessary, install both @polytable.sty@ and @lazylist.sty@ to
your \TeX\ system. If this is not desired or fails (because the
script cannot detect your \TeX\ installation properly),
the installation of these files can be disabled by passing 
the option @--disable-polytable@ to @configure@. In this case, 
the two files must be manually installed to
a location where your \TeX\ distribution will find them.
Assuming that you have a local \TeX\ tree at @/usr/local/share/texmf@,
this can usually be achieved by placing the files in the directory
@/usr/local/share/texmf/tex/latex/polytable@ and subsequently running
\input{MkTeXLsrCall}%
to update the \TeX\ filename database.

%%%
%%%

%---------------------------------------------------------------------------
\section{@lhs2TeX@ operation}
%---------------------------------------------------------------------------

When run on an input file, @lhs2TeX@ classifies the source file
into different blocks:
\begin{compactitem}
\item Lines indicating a Bird-style literate program (i.e. lines beginning
      with either @>@ or @<@) are considered as \defined{code blocks}.
\item Lines that are surrounded by @\begin{code}@ and @\end{code}@
      statements, or also by @\begin{spec}@ and @\end{spec}@
      statements, are considered as \defined{code blocks}. (Note that
      @lhs2TeX@ does not care if both styles of a literate program are
      mixed in one file. In this sense, it is more liberal than
      Haskell.)
\item Text between two \verb+@+ characters that is not in a code block
      is considered \defined{inline verbatim}. If a \verb+@+ is desired
      to appear in text, it needs to be escaped: \verb+@@+. There is no
      need to escape \verb+@+'s in code blocks.
\item Text between two @|@ characters that is not in a code block is
      considered \defined{inline code}. Again, @|@ characters that should
      appear literally outside of code blocks need to be escaped: @||@.
\item A @%@ that is followed by the name of an @lhs2TeX@ directive is
      considered as a \defined{directive} and may cause @lhs2TeX@ to
      take special actions. Directives are described in detail later.
\item Some constructs are treated specially, such as occurrences of
      the \TeX\ commands @\eval@, @\perform@, @\verb@ or of the \LaTeX\
      environment @verbatim@.
\item All the rest is classified as \defined{plain text}.
\end{compactitem}
Depending on the style in which it is called, @lhs2TeX@ will treat
these blocks in different ways.

%---------------------------------------------------------------------------
\section{Overview over the different styles}
%---------------------------------------------------------------------------

In this section, we will demonstrate on a common example how the
different styles can be used. For each style, there will also be
a short summary. Some of the points listed in the summary are
defaults for the particular style and can actually be changed.

%%%
%%%

\subsection{Verbatim: ``verb'' style}

In \textbf{verb} style, the code shows up in the formatted
document exactly as it has been entered, i.e. verbatim.
All spaces are preserved, and a non-proportional font is
used.
\input{Zip}%
One does not need @lhs2TeX@ to achieve such a result. This style,
however, does not make use of an internal \TeX\ verbatim construct.
The implementation of verbatim environments in \TeX\ is somewhat
restricted, and the preprocessor approach may prove more flexible
in some situations. For example, it is easier to apply additional
formatting instructions to the output as a whole, such as placing
the code in a colored box.

\paragraph{Verbatim summary}
\begin{compactitem}
\item formatting directives are ignored
\item conditionals and includes are handled
\item inline code, inline verbatim, and code blocks are all
      typeset completely verbatim, using a typewriter font
\item all spaces in code blocks are preserved
\item plain text is copied unchanged
\end{compactitem}

%%%
%%%

\subsection{Space-preserving formatting with ``tt'' style}

The \textbf{tt} style is very similar to \textbf{verb} style,
but applies a tiny bit of formatting to the code and allows
for more customizabilty:
\input{ZipTT}%
By default, some of the Haskell symbols are expressed more
naturally. For instance, special symbols are being used
for the arrows or the lambda. In addition, the user can
specify additional formatting directives to affect the appearance
of certain identifiers. In this way, keywords can be highlighted,
user-defined Haskell infix operators can be replaced by more
appropriate symbols etc. In this style, the layout and all
spaces from the source file are still preserved, and a non-proportional
font is used, as in \textbf{verb} style.

\paragraph{Typewriter summary}
\begin{compactitem}
\item non-recursive formatting directives are obeyed
\item conditionals and includes are handled
\item inline verbatim is typeset as verbatim, whereas inline
      code and code blocks are typeset almost verbatim, after
      formatting directives are applied, in a typewriter font
      using some special symbols to ``beautify'' some
      Haskell operators.
\item all spaces in code blocks are preserved
\item plain text is copied unchanged
\end{compactitem}

%%%
%%%

\subsection{Proportional vs.~Monospaced}

Usually, there is a tradeoff between restricting oneself to
the use of a typewriter font and not using any formatting and
using a proportional font, at the same time replacing operators
with mathematical symbols, using different font shapes to highlight
keywords etc. While the latter offers far more flexibility, the
proportional font might destroy (at least part of) the layout
that the programmer has employed in order to make the source
code more readable.

Compare, for example, the previous two examples with the
following result (this is a negative example, @lhs2TeX@ can
do far better than that!!):
\input{ZipStupid}%
\noindent
While the indentation is kept (otherwise, for the layout sensitive
Haskell it would be even disastrous, because the code might no
longer be valid), alignment that has been present in the code
lines has been lost. For example, in the input the user had decided
to align all equality symbols of all three function definitions,
and also align them with the ``has-type'' operator |::|.

Without support from a tool like @lhs2TeX@, the horizontal positions
of the equality symbols in the formatted code are totally unrelated.
A solution to this problem is of course to put the Haskell code in
a \LaTeX\ table. Doing this manually, though, is very cumbersome and
in some case still quite hard. The task of the formatted styles of
@lhs2TeX@ is thus to spare the user the burden of cluttering up
the code with formatting annotations. Most of the time, completely
un-annotated code can be used to achieve good results, using the
fonts you like while maintaining alignment information in the code!

%%%
%%%

\subsection{Alignment and formatting with ``math'' style}

In prior versions of @lhs2TeX@, \textbf{math} style was the mode
to use for formatted Haskell code. There is one alignment column,
often used to align the equality symbols of several equations.
Additionally, indentation is handled automatically. User-defined
formatting directives can be used to alter the formatting of
identifiers, operators and symbols in many places.
\input{ZipMath}%
\noindent
The example shows that there is still a loss of alignment information
compared to the original verbatim example. The three arguments of the
|zipWith| function as well as the two guarded equations
in the definition of |select| are not aligned. At the moment,
\textbf{math} style exists mainly to maintain compatibility with
old documents. New features may be added to \textbf{poly}
style only.

\paragraph{``math'' summary}
\begin{compactitem}
\item all formatting directives are obeyed
\item conditionals and includes are handled
\item inline verbatim is typeset as verbatim, whereas inline
      code and code blocks are typeset using a proportional
      font, using mathematical symbols to represent many Haskell
      operators.
\item indentation in code blocks is preserved; furthermore, alignment
      on a single column is possible
\item plain text is copied unchanged
\end{compactitem}

%%%
%%%

\subsection{Complex layouts: ``poly'' style}

The \textbf{poly} style has been designed to lift the restrictions
that \textbf{math} style still has. Multiple alignments and thus
complex layouts are possible:
\input{ZipPoly}%
If run in \textbf{poly} style, @lhs2TeX@ produces \LaTeX\ code
that makes use of the @polytable@ package, a package that has
been specifically designed to fit the needs that arise while
formatting Haskell code. (If you are interested in the package
or think that it might be useful for other purposes, you are
welcome to look at the documentation for 
@polytable@~\cite[also distributed with @lhs2TeX@ as 
@polytable.pdf@ in the @polytable@ directory]{polytable}.)

Beyond the advanced alignment options, \textbf{poly} style has
all the functionality of \textbf{math} style. If \textbf{poly}
style works for you, you should use it.

\paragraph{``poly'' summary}
\begin{compactitem}
\item all formatting directives are obeyed
\item conditionals and includes are handled
\item inline verbatim is typeset as verbatim, whereas inline
      code and code blocks are typeset using a proportional
      font, using mathematical symbols to represent many Haskell
      operators.
\item alignment can be flexibly specified; complex layouts
      are possible
\item plain text is copied unchanged
\end{compactitem}

%%%
%%%

\subsection{``poly'' style is customizable}

The following example demonstrates that the visual appearance
of ``poly'' style is in no way dictated by @lhs2TeX@. There
are several possibilities to modify the output by means
of formatting directives. Here, we try to mimic \textbf{tt}
style by choosing a typewriter font again and using the same
symbols that are default in \textbf{tt} style.
\input{ZipPolyTT}%
In contrast to the \textbf{tt} style example, here the spaces
in the code are \emph{not} preserved -- the alignment is generated
by the @polytable@ package.

%%%
%%%

\subsection{The ``code'' and ``newcode'' styles}

These two styles are not for producing a \LaTeX\ source file,
but rather produce a Haskell file again. Everything that is
not code is thrown away. In addition, \textbf{newcode} does
a few things extra. It applies formatting directives which
can here be used as simple macros on the Haskell source level,
and it generates line pragmas for the Haskell compiler that will
result in error messages pointing to the original file (before
processing with @lhs2TeX@). The plain \textbf{code} style
does not have this extra functionality. Again, \textbf{code}
is mainly intended for compatibility with old documents. You
should use \textbf{newcode}.

\paragraph{``code'' summary}
\begin{compactitem}
\item formatting directives are ignored
\item conditionals and includes are handled (??)
\item code blocks that are not specifications are copied unchanged
\item plain text, inline code, specification code, 
      and inline verbatim are discarded
\end{compactitem}

\paragraph{``new code'' summary}
\begin{compactitem}
\item all formatting directives are obeyed
\item conditionals and includes are handled
\item code blocks that are not specifications are, after applying 
      formatting directives, 
      copied unchanged, prefixed by a line pragma indicating the
      original source location of the code block
\item plain text, inline code, specification code, 
      and inline verbatim are discarded
\end{compactitem}


%%%
%%%

%---------------------------------------------------------------------------
\section{Directives}
%---------------------------------------------------------------------------

A number of directives are understood by @lhs2TeX@. Some of the are
ignored in some of the styles, though.  Directives can occur on all
non-code lines and start with a @%@, the \TeX\ comment character,
immediately followed by the name of the directive, plus a list of
potential arguments.

While @lhs2TeX@ will remove directives that it has interpreted, it will 
simply ignore all normal \TeX comments that are no directives. 
Therefore, if a directive is accidentally misspelled, 
no error message will be raised in general.

Table~\ref{directives} is a complete list of the directives 
that @lhs2TeX@ knows about.
\begin{table}
\input{CompleteDirectives}%
\caption{All @lhs2TeX@ directives}\label{directives}
\end{table}
These directives will be explained in more or less detail in the
following sections.

%---------------------------------------------------------------------------
\section{Including files}
%---------------------------------------------------------------------------

Other files can be included by @lhs2TeX@. This is what the
@%include@ directive is for:
\input{IncludeSyntax}%
The specified file is searched for in the @lhs2TeX@ source path
which can be modified using environment variables or
the @-P@ command line option (see also page~\pageref{defaultsearchpath}).
Included files are inserted literally at the position of the
@%include@ directive. Because of that, the included files may not
only contain other sources, but also other directives (in particular,
an included file may contain an @%include@ directive again).
The @lhs2TeX@ is entirely independent of \TeX\ or Haskell includes/imports.

\begin{important}[Warning]
Although relative and absolute pathnames can be specified as part
of a filename in an @%include@ directive, the use of this feature
is strongly discouraged. Set the search path using the @-P@ command
line option to detect files to include.
\end{important}

If the @-v@ command line flag is set, @lhs2TeX@ will print the
paths of the files it is reading on screen while processing a file.

%%%
%%%

\subsection{The @lhs2TeX@ ``prelude''}

Several aspects of the behaviour of @lhs2TeX@ are not hardcoded,
but configurable via directives. As a consequence,
a minimal amount of functionality has to be defined for @lhs2TeX@
to be able to operate normally.

Essential definitions are collected in the file @lhs2TeX.fmt@.
\begin{important}[Note to users of previous versions]
There used to be a file @lhs2TeX.sty@ that also contained a part
of the prelude declarations. This file does still exist for
compatibility reasons, but is now deprecated. It should \emph{not}
be included directly in any of your documents anymore.
\end{important}
If you are using the \textbf{poly} or \textbf{newcode}
styles, some of the defaults in @lhs2TeX.fmt@ are sub-optimal.
In this case, there is a better prelude @polycode.fmt@ (which includes
@lhs2TeX.fmt@ in turn). One of the two files @lhs2TeX.fmt@ or @polycode.fmt@
should be included (using @%include@)
-- directly or indirectly -- in every file to be processed by
@lhs2TeX@!
\input{IncludePrelude}%
It is perfectly possible to design
own libraries that replace or extend these basic files and to include
those own libraries instead.  It is not recommended, though, to edit
these two files directly.  If you are not satisfied with some of the
default definitions, create your own file to redefine selected
parts. This way, if @lhs2TeX@ is updated, you will still be able to
benefit from improvements and changes in the ``prelude'' files.

It is possible to use @lhs2TeX@ in a setup where a \TeX\ document
is split into several files, and each of the files should be processed
separately by @lhs2TeX@. In this case, just include @lhs2TeX.fmt@
(or @polycode.fmt@) in every single file source file.

\begin{important}[Warning]
Note that both @lhs2TeX.fmt@ and @polycode.fmt@ contain @lhs2TeX@
directives, and therefore \emph{cannot} be included using \TeX\ or \LaTeX\
include mechanisms such as @\input@ or @\usepackage@.
\end{important}

%%%
%%%

% End of introduction part -- begin of reference

%%%
%%%

%%%
%%%

%%%
%%%

%---------------------------------------------------------------------------
\section{Formatting}
%---------------------------------------------------------------------------

Using the @%format@ directive, tokens can be given a different
appearance. The complete syntax that is supported by @lhs2TeX@ is
quite complex, but we will look at many different cases in detail.
\input{FormatSyntax}% There are three different forms of the
formatting statement.  The first one can be used to change the
appearance of most functions and operators and a few other
symbols. The second form is restricted to named identifiers (both
qualified and unqualified, but no symbolic operators); in turn,
such formatting directives can be parametrized. Finally, the third 
form provides a syntactically
lightweight way of formatting certain identifiers using some
heuristics. But let us look at some common examples first \dots

%%%
%%%

\subsection{Formatting single tokens}

The most important use for @%format@ is to assign a symbol to
an identifier or an operator.
The input
\input{FormatGreekIn}%
produces output similar to the following:
\input{FormatGreekOut}%
The occurrences of @alpha@ within the Haskell code portions of
the input file are replaced by the \TeX\ command @\alpha@ and
thus appear as ``$\alpha$'' in the output.

A lot of formatting directives for frequently used identifiers
or operators is already defined in the @lhs2TeX@ prelude.
For instance, @++@ is formatted as ``|++|'', @undefined@ is
formatted as ``|undefined|'', and @not@ is formatted
as ``|not|''. If you look at @lhs2TeX.fmt@, you will find the
following directives that do the job:
\input{FormatIdentifierExamples}%
Here, @\plus@ refers to a \LaTeX\ macro defined in the lhs2\TeX\ prelude:
\input{PlusDefinition}%
If you are not satisfied with any of the default definitions,
just redefine them. A @%format@ directive scopes over the rest
of the input, and if multiple directives for the same token
are defined, the last one is used. Thus, after
\input{FormatIdentifierRedefs}%
%{
%format ++        = "\mathbin{\mathbf{+}}"
%format undefined = "\Varid{undefined}"
%format not       = "!"
you get ``|++|'', ``|undefined|'', and ``|not|'', respectively.
Note that @\Varid@ is a macro defined in the lhs2\TeX\ prelude that
can be used to typeset identifier names. It is predefined to
be the same as @\mathit@, but can be changed. Do not use identifier
names in \TeX\ replacements directly. For instance,
\input{FormatIdentifierWrong}%
%{
%format undefined  = "undefined"
will cause @undefined@ to be typeset as ``|undefined|'', which looks
by far less nice than
%}
``|undefined|''.
%}
It is also possible to define a symbol for infix uses of a
function. The file @lhs2TeX.fmt@ contains:
\input{FormatElem}%
This causes @2 `elem` [1,2]@ to be typeset as ``|2 `elem` [1,2]|'',
whereas @elem 2 [1,2]@ will still be typeset as ``|elem 2 [1,2]|''.

%%%
%%%

\subsection{Nested formatting}

The right hand sides of formatting directives are not restricted
to (\TeX-)strings. They can in fact be sequences of such strings or
other tokens, separated by space. Such other tokens will be replaced
by their formatting again. For example, if you have already defined
a specific formatting
\input{FormatArrow}%
%format ~> = "\leadsto "
then you can later reuse that formatting while defining variants:
\input{FormatArrow2}%
%format ~>* = ~> "^{" * "}"
As you can see, in this definition we reuse both the current formatting
for @~>@ and for @*@. We now get ``|~>*|'' for @~>*@, but should we
decide to define
\input{FormatStar}%
%format * = "\star "
later, we then also get ``|~>*|''. Of course, you can use the same
mechanism for non-symbolic identifiers:
\input{FormatId}%
%{
%format new      = "\mathbf{new}"
%format text0    = text
%format text_new = text "_{" new "}"
will cause @text0@ to be typeset as ``|text0|'', and @text_new@ will
appear as ``|text_new|''.
%}
\begin{important}[Warning]
There is no check for recursion in the formatting directives.
Formatting directives are expanded on-demand, therefore a directive
such as
\input{FormatRecurse}%
will not produce ``$\mathsf{text}$'' for @text@, but rather 
cause an infinite loop in
@lhs2TeX@ once used.
\end{important}

%%%
%%%

\subsection{Parametrized formatting directives}

Formatting directives can be parametrized. The parameters may occur
once or more on the right hand side. This form of a formatting
directive is only available for alphanumeric identifiers. For
example, the input
\input{CardIn}%
produces output similar to
\begin{colorsurround}
\input{Card}
\end{colorsurround}
If the function is used with two few arguments as in the text,
a default symbol is substituted (usually a @\cdot@, but that is
customizable, cf. Section~\ref{subst}).

%%%
%%%

\subsection{(No) nesting with parametrized directives}

You cannot use a parametrized directive on the right hand side
of another directive. 
In summary,
the right-hand sides of formatting directives are processed as follows:
\begin{compactitem}
\item A string, enclosed in @"@, will be reproduced literally (without
      the quotes).
\item A name, if it is the name of a parameter, will be replaced by the
      actual (formatted) argument.
\item A name, if it is the name of a non-parametrized formatting directive,
      will be replaced by that directive's replacement.
\item Any other name will be replaced by its standard formatting.
\end{compactitem}
Note that the spaces between the tokens do not occur in the output.
If you want spaces, insert them explicitly.

%%%
%%%

\subsection{Parentheses}

Sometimes, due to formatting an identifier as a symbol, parentheses
around arguments or the entire function become unnecessary.
Therefore, @lhs2TeX@ can be instructed to drop parentheses around an
argument by enclosing the argument on the left hand side of the
directive in parentheses. Parentheses around the entire function are
dropped if the entire left hand side of the directive is enclosed in
parentheses. Let us look at another example:
\input{ParensExampleIn}%
The above input produces the following output:
\begin{colorsurround}
\input{ParensExample}
\end{colorsurround}
Note that in this example a special purpose operator, @^^@, is used
to facilitate the insertion of spaces on the right hand side of a
formatting directive. Read more about influencing spacing using formatting
directives in Section~\ref{spacing}.
Another example involving parentheses: the input
\input{ParensExample2In}%
results in       
\begin{colorsurround}
\input{ParensExample2}
\end{colorsurround}


%%%
%%%

\subsection{Local formatting directives}

Usually, formatting directives scope over the rest of the input.
If that is not desired, formatting directives can be placed into
\textbf{groups}. Groups look as follows:
\input{GroupSyntax}%
Formatting directives that are defined in a group scope only over
the rest of the current group. Groups can be nested. Groups in
@lhs2TeX@ do not interact with \TeX\ groups, so these different
kinds of groups do not have to occur properly nested.

The effect of groups is made visible by the example input
\input{GroupExampleIn}
which appears as follows:
\begin{colorsurround}
\input{GroupExample}
\end{colorsurround}

%%%
%%%

\subsection{Implicit formatting}

The third syntactic form of the formatting directive, lacking a
right hand side, can be used to easily format a frequently occurring
special case:
only a variable (or constructor) name that ends in a number or a prime @'@
can be used in an implicit formatting statement.
The prefix will then be formatted as determined by the formatting directives
in the input so far. The number will be added as an index, the prime 
character as itself.

The following input contains some example:
\input{ImplicitIn}
The corresponding output is:
\begin{colorsurround}
\input{Implicit}
\end{colorsurround}

Another form of implicit formatting only takes place only if the token to
be formatted does not end in primes, and only if digits at the end are 
immediately preceded by an underscore. The reason for these conditions is
compatibility. If the conditions are met, then the token is split at
underscores, and the part to the right of an underscore is typeset as
subscript to the part on the left, recursively. Again, let us look at an
example:
\input{ImplicitUnderscoreIn}
And its output:
\begin{colorsurround}
\input{ImplicitUnderscore}
\end{colorsurround}

%%%
%%%

\subsection{Formatting behaviour in different styles}

\begin{compactitem}
\item Formatting directives are applied in \textbf{math}, \textbf{poly}, and
      \textbf{newcode} styles.
\item In \textbf{tt} style, only non-parametrized directives apply.
\item In \textbf{verb} and \textbf{code} styles, formatting directives are ignored.
\end{compactitem}
A document can be prepared for processing in different styles 
using conditionals (cf.~Section~\ref{conditionals}).

%%%
%%%

%---------------------------------------------------------------------------
\section{Alignment in ``poly'' style}
%---------------------------------------------------------------------------

The second important feature of @lhs2TeX@ next to the ability to
change the appearance of tokens is the possibility to maintain
alignment in the code while using a proportional font.

Use of this feature is relatively simple:
\begin{compactitem}
\item Alignment is computed per code block.
\item All tokens that start on the same column and are preceded by at
      least \textbf{2} spaces are horizontally aligned in the output.
\end{compactitem}
Using these simple rules, (almost) everything is possible, but it
is very important to verify the results and watch out for accidental
alignments (i.e.~tokens that get aligned against intention).

%%%
%%%

\subsection{An example}

The following example shows some of the potential. This is the
input:
\input{RepAlgIn}%
Look at the highlighted (grey) tokens. The @lt@ will not appear
aligned with the two equality symbols, because it is preceded by
only one space. Similarly, the @m@ in the first line after the
@Leaf@ constructor will not be aligned with the declarations and
the body of the let-statement, because it is preceded by only
one space. Note furthermore that the equality symbols for the
main functions @rep_alg@ and @replace_min'@ are surrounded by two
spaces on both sides, also on the right. This causes the comma
and the closing parenthesis to be aligned correctly.

Indeed, the output looks as follows:
\input{RepAlg}%


%%%
%%%

\subsection{Accidental alignment}

The main danger of the alignment heuristic is that it results
in \emph{more} alignments than are intended. The following
example input contains such a case:
\input{AccidentalIn}%
The grey tokens will be unintentionally aligned because
they start on the same column, with two or more preceding spaces
each. The output looks as follows:
\input{Accidental}%
The ``|::|'' and the ``|=|'' have been aligned with the declarations
of the where-clause. This results in too much space between
the two |options| tokens and the symbols. Even worse, in this
case the \emph{centering} of the two symbols is destroyed by
the alignment (cf. Section~\ref{centering}), therefore ``|::|''
and ``|=|'' appear left-aligned, but not cleanly, because
\TeX\ inserts a different amount of whitespace around the two
symbols.

The solution to all this is surprisingly simple: just insert extra
spaces in the input to ensure that unrelated tokens start on different
columns:
\input{AccidentalCIn}%
This input produces the correct output:
\input{AccidentalC}%

%%%
%%%

\subsection{The full story}

If you further want to customize the alignment behaviour, you can.
Here is exactly what happens:
\begin{compactitem}
\item Alignment is computed per code block.
\item Per code block there are a number of \textbf{alignment columns}.
\item If a token starts in column |n| and is prefixed by at least 
      ``\emph{separation}''
      spaces, then |n| is an \textbf{alignment column} for the code block.
\item If a token starts in an alignment column |n| and is prefixed by at least 
      ``\emph{latency}''
      spaces, then the token is \textbf{aligned} at column |n|.
\item All tokens that are aligned at a specific column will appear aligned
      (i.e. at the same horizontal position) in the output.
\end{compactitem}
Both latency and separation can be modified by means of associated
directives:
\input{SepLatSyntax}%
It can occasionally be useful to increase the default settings of 2 and
2 for large code blocks where accidental alignments become very
likely! It does not really make sense to set latency to a value that
is strictly smaller than the separation, but you can do so -- there
are no checks that the specified settings are sensible.

%%%
%%%

\subsection{Indentation in ``poly'' style}

Sometimes, @lhs2TeX@ will insert additional space at the beginning
of a line to reflect indentation. The rule is described in the
following.

If a line is indented in column |n|, then
the \emph{previous} code line is taken into account:
\begin{compactitem} 
\item If there is an aligned token at column |n| in the previous
      line, then the indented line will be aligned normally.
\item Otherwise, the line will be indented with respect to the
      first aligned token in the previous line to the left of column |n|.
\end{compactitem}

The first example demonstrates the first case:
\input{Indent1In}%
In this example, there is an aligned token in the previous line
at the same column, so everything is normal.
The two highlighted parentheses are aligned, causing the
second line to be effectively indented:
\input{Indent1}%
The next example demonstrates the second case. It is the same
example, with one space before the two previously aligned parentheses
removed:
\input{Indent2In}%
Here, there is no aligned token in the previous line
at the same column. Therefore, the third line is indented with
respect to the first aligned token in the previous line to the
left of that column, which in this case happens to be the @xs@:
\input{Indent2}%
Sometimes, this behaviour might not match the intention of
the user, especially in cases as
above, where there really starts a token at the same position
in the previous line, but is not preceded by enough spaces.
Always verify the output if the result looks as desired.

The amount of space that is inserted can be modified. A call
to the \TeX\ control sequence @\hsindent@ is inserted at the
appropriate position in the output, which gets as argument the
column difference in the source between the token that is 
indented, and the base token. In the situation of the
above example, the call is @\hsindent{12}@. The default definition
in the lhs2\TeX\ prelude
ignores the argument and inserts a fixed amount of space:
\input{HsIndent}%

Here is another example that shows indentation in action, the
Haskell standard function |scanr1| written using only basic
pattern matching:
\input{Indent2aIn}%
And the associated output:
\input{Indent2a}%

%%%
%%%

\subsection{Interaction between alignment and indentation}

In rare cases, the indentation heuristic can lead to surprising
results. This is an example:
\input{Indent3In}%
And its output:
\input{Indent3}%
Here, the large amount of space between |test| and
|1| might be surprising. However, the |1| is aligned with the |2|, 
but |2| is also indented with respect to |bar|, so everything
is according to the rules. The ``solution'' is to verify if both
the alignment between |1| and |2| and the indentation of the |2|
are intended, and to remove or add spaces accordingly.

%%%
%%%

\subsection{Interaction between alignment and formatting}

If a token at a specific column is typeset according to a formatting
directive, then the first token of the replacement text inherits the
column position of the original token. The other tokens of the
replacement text will never be aligned. Actual arguments of
parametrized formatting directives keep the column positions they have
in the input.

%%%
%%%

\subsection{Centered and right-aligned columns}\label{centering}

Under certain circumstances @lhs2TeX@ decides to typeset a
column centered instead of left-aligned. This happens if the
following two conditions hold:
\begin{compactitem}
\item There is \emph{at most one} token per line that is associated
      with the column.
\item \emph{At least one} of the tokens associated with the column
      is a symbol.
\end{compactitem}
In most cases, this matches the intention. If it does not, there
still might be the possibility to trick @lhs2TeX@ to do the right
thing:
\begin{compactitem}
\item Change the alignment behaviour of the column using
      @\aligncolumn@ (see below).
\item If the column is centered but should not be, add extra
      tokens that are formatted as nothing that will be associated
      with the column (see also Section~\ref{spacing} about spacing).
\item If the column should be centered but is left-aligned, it is
      sometimes possible to use a symbol instead of an alphanumeric
      identifier, and add a formatting directive for that newly
      introduced symbol.
\end{compactitem}

The syntax of the @\aligncolumn@ command is:
\input{AlignColumnSyntax}%
% The above file also contains some additional documentation.

TODO: ADD EXAMPLE!!

%%%
%%%

\subsection{Saving and restoring column information}

It is possible to share alignment information between different
code blocks. This can be desirable, especially when one wants
to interleave the definition of a single function with longer
comments. This feature is implemented on the \TeX\ level 
(the commands are defined in the lhs2\TeX\ prelude).

Here is an example of its use:
\input{SaveRestoreIn}%
As output we get:
\begin{colorsurround}
\input{SaveRestore}
\end{colorsurround}
Compare this to the output that would be generated 
without the @\savecolumns@ and @\restorecolumns@ commands:
\begin{colorsurround}
\input{SaveRestoreNo}
\end{colorsurround}

\begin{important}
If this feature is used, it may require several runs of \LaTeX\ until
all code blocks are correctly aligned. Watch out for warnings
of the @polytable@ package that tell you to rerun \LaTeX!
\end{important}

%---------------------------------------------------------------------------
\section{Defining variables}\label{variables}
%---------------------------------------------------------------------------

One can define or define flags (or variables) by means of the
@%let@ directive.
\input{LetSyntax}%
Expressions are built from booleans (either @True@ or @False@),
numerals (integers, but also decimal numbers) and previously defined
variables using some fixed set of builtin operators. The expression
will be evaluated completely at the time the @%let@ directive
is processed. If an error occurs during evaluation, @lhs2TeX@ will
fail.

Variables can also be passed to @lhs2TeX@ from the operating
system level by using the @-l@ or @-s@ command line options.

The main use of variables is in conditionals 
(cf.~Section~\ref{conditionals}).
At the moment, there is no way to directly use the value of a
variable in a @%format@ directive.

%%%
%%%

\subsection{Predefined variables}

In every run of @lhs2TeX@, the version of @lhs2TeX@ is available
as a numerical value in the predefined variable @version@. Similarly,
the current style is available as an integer in the predefined 
variable @style@. There also are integer variables @verb@, @tt@, 
@math@, @poly@, @code@, and @newcode@ predefined that can be used
to test @style@.

It is thus possible to write documents in a way that they can be
processed beautifully in different styles, or to make safe use of
new @lhs2TeX@ features by checking its version first.

%%%
%%%

%---------------------------------------------------------------------------
\section{Conditionals}\label{conditionals}
%---------------------------------------------------------------------------

Boolean expressions can be used in conditionals. The syntax of an
@lhs2TeX@ conditional is
\input{IfSyntax}%
where the @%elif@ and @%else@ directives are optional. There may
be arbitrarily many @%elif@ directives. When an @%if@ directive
is encountered, the expression is evaluated, and depending on the
result of the evaluation of the expression, only the then or only
the else part of the conditional is processed by @lhs2TeX@, the
other part is ignored.

%%%
%%%

\subsection{Uses of conditionals}

These are some of the most common uses of conditionals:
\begin{compactitem}
\item One can have different versions of one paper in one (set of)
      source file(s). Depending
      on a flag, @lhs2TeX@ can produce either the one or the other. 
      Because the flag can be defined via a command 
      line option (cf.~Section~\ref{variables}), 
      no modification of the source is necessary to switch versions.
\item Code that is needed to make the Haskell program work but that
      should not appear in the formatted article (module headers,
      auxiliary definitions), can be enclosed between @%if False@
      and @%endif@ directives.
\item Alternatively, if Haskell code has to be annotated for 
      @lhs2TeX@ to produce aesthetically pleasing output, one can 
      define different formatting directives for
      the annotation depending on style (\textbf{poly} or \textbf{newcode}).
      Both code and \TeX\ file can then still be produced from a
      common source! Section~\ref{generichaskell} contains an example
      that puts this technique to use.
\end{compactitem}

The lhs2\TeX\ library files use conditionals to
include different directives depending on the style selected, but
they also use conditionals to provide additional or modified behaviour
if some flags are set. These flags are @underlineKeywords@,
@spacePreserving@, @meta@ (activate a number of additional formatting
directives), @array@ (use @array@ environment instead of @tabular@
to format code blocks in \textbf{math} style; use @parray@ instead
of @pboxed@ in \textbf{poly} style), @latex209@ (adapt for use with
\LaTeX\ 2.09 (not supported anymore)), @euler@, and @standardsymbols@. 
%TODO: document the purpose of these flags better. 
It is likely that these flags
will be replaced by a selection of library files that can be selectively
included in documents in future versions of @lhs2TeX@.

%%%
%%%

%---------------------------------------------------------------------------
\section{Typesetting code beyond Haskell}
%---------------------------------------------------------------------------

\subsection{Spacing}\label{spacing}

There is no full Haskell parser in @lhs2TeX@. Instead, the input
code is only lexed and subsequently parsed by an extremely simplified
parser. The main purpose of the parser is to allow a simple heuristic
where to insert spaces into the output while in \textbf{math} or
\textbf{poly} style. 

The disadvantage is that in
rare cases, this default spacing produces unsatisfying results.
However, there is also a big advantage: dialects of Haskell can
be processed by @lhs2TeX@, too. In theory, even completely 
different languages can be handled. The more difference between
Haskell and the actual input language, the more tweaking is probably
necessary to get the desired result.

An easy trick to modify the behaviour of @lhs2TeX@ is to insert
``dummy'' operators that do not directly correspond to constructs
in the input language, but rather provide hints to @lhs2TeX@ on
how to format something. For instance, spacing can be
guided completely by the following two formatting directives:
\input{SpacingOps}%
Use @^@ everywhere where \emph{no} space is desired, but the
automatic spacing of @lhs2TeX@ would usually place one.
Conversely, use @^^@ everywhere where a space \emph{is} desired,
but @lhs2TeX@ does usually not place one.

As described in Section~\ref{conditionals}, one can use conditionals
to format such annotated input code in both \textbf{poly} 
(or \textbf{math}) and \text{newcode} style to generate both typeset
document and code with annotation remove from a single source file.
For this to work correctly, one would define
\input{SpacingOpsCond}%
as an extended version of the above. This instructs @lhs2TeX@ to
ignore @^@ and replace @^^@ by a single space while in \textbf{newcode}
style, and to adjust spacing in other styles, as before.

The examples in the following subsections show these directives
in use.

%%%
%%%

\subsection{Inline \TeX}

Another possibility that can help to trick @lhs2TeX@ into doing things
it normally doesn't want to is to insert inline \TeX\ code directly
into the code block by using a special form of Haskell comment:
\input{InlineTeXSyntax}%
% The above file also contains some additional documentation.
The advantage of this construct over a dummy operator
is that if the input language is indeed Haskell, one does not need
to sacrifice the syntactic validity of the source program for nice
formatting. On the other hand, inline \TeX\ tends to be more verbose
than an annotation using a formatting directive.

%%%
%%%

\subsection{{\smaller AG} code example}

Here is an example that shows how one can typeset code of the
Utrecht University Attribute Grammar ({\smaller UUAG}) 
(\cite{uuag}) system,
which is based on Haskell, but adds additional syntactic constructs.

The input
\input{AGExampleIn}%
produces the following output:
\input{AGExample}

%%%
%%%

\subsection{Generic Haskell example}\label{generichaskell}

Another example of a Haskell variant that can be typeset using
@lhs2TeX@ using some annotations is Generic Haskell~\cite{gh}.

This is a possible input file, including the directives
necessary to be able to process it in both \textbf{newcode}
and \textbf{poly} style.
\input{GHExampleIn}%
Processed in \textbf{poly} style, the output looks as follows:
\input{GHExample}%

%%%
%%%

\subsection{Calculation example}

The following example shows a calculational proof. The input
\input{CalcExampleIn}%
produces
\input{CalcExample}%


%%%
%%%

%---------------------------------------------------------------------------
\section{Calling @hugs@ or @ghci@}
%---------------------------------------------------------------------------

It is possible to call @ghci@ or @hugs@ using the @%options@
directive. In all but the two \textbf{code} styles, @lhs2TeX@
looks for calls to the \textbf{\TeX\ commands} @\eval@ and
@\perform@ and feeds their arguments to the Haskell interpreter
selected.

The current input file will be the active module. This has a
couple of consequences: on the positive side, values defined in
the current source file may be used in the expressions; on the
negative side, the feature will only work if the current file
is accepted as legal input by the selected interpreter.

If the command line in the @%options@ directive starts with
@ghci@, then @lhs2TeX@ assumes that @ghci@ is called; otherwise,
it assumes that @hugs@ is called. Depending on the interpreter,
@lhs2TeX@ will use some heuristics to extract the answer from
the output of the interpreter. After this extraction, the result
will either be printed as inline verbatim (for a @\perform@) or
as inline code (for @\eval@), to which formatting directives
apply.

\begin{important}[Warning]
This feature is somewhat fragile: different versions of @ghci@
and @hugs@ show different behaviour, and the extraction heuristics
can sometimes fail. Do not expect too much from this feature.
\end{important}

%%%
%%%

\subsection{Calling @ghci@ -- example}

The following input shows an example of how to call @ghci@:
\input{InteractiveGhciIn}%
The option @-fglasgow-exts@ is necessary to make @ghci@
accept the @forall@ keyword (it only serves as an example
here how to pass options to the interpreter). 
The output will look similar to this:
\begin{colorsurround}
\input{InteractiveGhci}
\end{colorsurround}
Note that it is possible to pass interpreter commands such
as @:t@ to the external program. 
%(ADAPT EXAMPLE TO SHOW THIS:)
%Note furthermore the difference
%in output between an @\eval@ and a @\perform@ command.

%%%
%%%

\subsection{Calling @hugs@ -- example}

The same could be achieved using @hugs@ instead of @ghci@.
For this simple example, the output is almost indistinguishable,
only that @hugs@ usually does not print type signatures using
explicit quantification and tends to use different variable
names.
\input{InteractiveHugsIn}%
The input is the same except for the changed @%options@
directive. The output now looks as follows:
\begin{colorsurround}
\input{InteractiveHugs}
\end{colorsurround}

%%%
%%%

\subsection{Using a preprocessor}

The situation is more difficult if the current @lhs2TeX@
source file is not valid input to the interpreter, because
annotations were needed to format some Haskell extensions
satisfactory. The following input file makes use of Template
Haskell, and uses the formatting directives for both
\textbf{newcode} and \textbf{poly} style. The @%options@
directive instructs @ghci@ to use @lhs2TeX@ itself as
the literate preprocessor, using the @-pgmL@ option of @ghci@.
The @lhs2TeX@ binary itself acts as a suitable literate 
preprocessor if the @--pre@ command line option is passed, which
is achieved using the @-optL--pre@ option:
\input{InteractivePreIn}%
This is the corresponding output:
\begin{colorsurround}
\input{InteractivePre}
\end{colorsurround}


%---------------------------------------------------------------------------
\section{Advanced customization}\label{subst}
%---------------------------------------------------------------------------

There is one directive that has not yet been described: @%subst@.
This directive is used by @lhs2TeX@ to customize almost every aspect
of its output. The average user will and should not need to use
a @%subst@ directive, but if one wants to influence the very nature
of the code generated by @lhs2TeX@, the @%subst@ directives provide
a way to do it.

If one would, for instance, want to generate output for another
\TeX\ format such as plain\TeX\ or Con\TeX t, or if one would want
to use a different package than @polytable@ to do the alignment
on the \TeX\ side, then the @%subst@ directives are a good place to
start. The default definitions can be found in @lhs2TeX.fmt@.

Table~\ref{substs} shows only a short description of the approximate
use of each of the categories.

\begin{table}
\centering
\begin{colorsurround}
\begin{tabularx}{\linewidth}{lX}
@thinspace@   & how to produce a small quantity of horizontal space \\
@space@       & how to produce a normal horizontal space \\
@newline@     & how to produce a new line inside a code block \\
@verbnl@      & how to produce a new line in @lhs2TeX@ generated verbatim \\
@blankline@   & how to translate a blank line in a code block \\
@dummy@       & how to display a missing argument in a formatted function \\
@spaces@ |a|  & how to format the whitespace contained in |a| \\
@special@ |a| & how to format the special character |a| \\
@verb@ |a|    & how to format the (already translated) inline 
                verbatim text~|a| \\
@verbatim@ |a|& how to format an (already translated) verbatim block |a| \\
@inline@ |a|  & how to format (already translated) inline code |a| \\
@code@ |a|    & how to format an (already translated) code block |a| \\
@conid@ |a|   & how to format an identifier starting with an upper-case
                character |a| \\
@varid@ |a|   & how to format an identifier starting with a lower-case
                character |a| \\
@consym@ |a|  & how to format a constructor symbol |a| \\
@varsym@ |a|  & how to format a variable symbol |a| \\
@backquoted@ |a| & how to format a backquoted operator |a| \\
@numeral@ |a| & how to format a numeral |a| \\
@char@ |a|    & how to format a character literal |a| \\
@string@ |a|  & how to format a literal string |a| \\
@comment@ |a| & how to format an (already translated) one-line comment |a| \\
@nested@ |a|  & how to format an (already translated) nested comment |a| \\
@pragma@ |a|  & how to format an (already translated) compiler pragma |a| \\
@keyword@ |a| & how to format the Haskell keyword |a| \\
@column1@ |a| & how to format an (already translated) line |a| 
                in one column in \textbf{math} style \\
@hskip@ |a|   & how to produce a horizontal skip of |a| units \\
@phantom@ |a| & how to produce horizontal space of the width of the
                (already translated) text |a| \\
@column3@ |a| & how to format an (already translated) line |a|
                in three columns in \textbf{math} style \\
@fromto@ |b e a| & how to format a column starting at label |b|,
                ending at label |e|, containing the (already translated)
                code |a| in \textbf{poly} style \\
@column@ |n a| & how to define a column of label |n| with (already
                processed) format string |a| in \textbf{poly} style \\
@centered@    & the format string to use for a centered column \\
@left@        & the format string to use for a left-aligned column \\
@dummycol@    & the format string to use for the dummy column
                (a column that does not contain any code; needed
                due to deficiencies of the @polytable@ implementation) \\
@indent@ |n|  & how to produce an indentation (horizontal space) 
                of |n| units \\
\end{tabularx}
\end{colorsurround}
\caption{A short description of the @%subst@ directives}\label{substs}
\end{table}


%%%
%%%

% %---------------------------------------------------------------------------
% \section{Implementation and distribution}
% %---------------------------------------------------------------------------
% 
% \begin{compactitem}
% \item @lhs2TeX@ is written in Haskell
% \item \textbf{poly} style makes use of a specifically written \LaTeX\ package
%       @polytable@, which is included in the distribution
% \item License is {\smaller GPL}.
% \item There has not been an official release for a long time, so get the
%       most recent version from {\smaller CVS} (or subversion soon).
% \item It does work on Unix-alikes. It should work on Windows/Cygwin, and
%       on native Windows with minor modifications -- help welcome.
% \item It has been used for several recent papers and seems to be quite stable.
% \end{compactitem}
% 
% %%%
% %%%
% 
% %---------------------------------------------------------------------------
% \section{Future work}
% %---------------------------------------------------------------------------
% 
% \begin{compactitem}
% \item More language independence (customizable lexer).
% \item Clean up (and extend) the formatting directives language.
% \item Allow directives during code blocks.
% \item Add more features to @polytable@ package.
% \item \dots
% \end{compactitem}
% Future development is relatively low priority, though.
% If you want it, do it yourself or try to convince me
% that it is urgent!

\newenvironment{problem}%
  {\medskip\par\noindent\bfseries\ignorespaces}{\ignorespacesafterend}

%%%
%%%

% \section{History of @lhs2TeX@}
% 
% \begin{compactitem}
% \item Ralf Hinze started development in 1997. Most of the hard work has
%   been done by him!
% \item The program is based on @smugweb@ and @pphs@, both of which are
%   no longer available and I do not know.
% \item I picked up development in 2002, and added
%   the \textbf{poly} and \textbf{newcode} styles.
% %\item Future: I consider the \textbf{tt} and \textbf{math} styles as deprecated,
% %  I want to add more language independence (customizable lexer) and 
% %  extend/improve the formatting language.
% \end{compactitem}

%---------------------------------------------------------------------------
\section{Pitfalls/FAQ}
%---------------------------------------------------------------------------

\begin{problem}
The document consists of multiple files. Can @lhs2TeX@ be used?
\end{problem}
One option is to use @%include@ rather than \LaTeX\ commands
to include all files in the master file. The other is to process
all files that contain code \emph{and} the master file with @lhs2TeX@.
All files to be processed with @lhs2TeX@ must contain an
@%include lhs2TeX.fmt@ (or @%include polycode.fmt@) statement. 
From version 1.11 on, including @lhs2TeX.sty@ is no longer necessary.

\begin{problem}
Yes, but the master file should be pure \LaTeX.
\end{problem}
Create a file @mylhs2tex.lhs@ with just one line, namely
@%include lhs2TeX.fmt@. Process that file with @lhs2TeX@, using the
options you also use for the other included files. Call the resulting
file @mylhs2tex.sty@ and say @\usepackage{mylhs2tex}@ at the beginning
of your master file.

\begin{problem}
The spacing around my code blocks is bad (nonexistent) in ``\textbf{poly}''
style.
\end{problem}
Add the line @%include polycode.fmt@ to the preamble of your document.

\begin{problem}
\LaTeX\ complains when using @lhs2TeX@ in ``\textbf{poly}'' style
with the @beamer@ package.
\end{problem}
Add the line @%include polycode.fmt@ to the preamble of your document.

\begin{problem}
\LaTeX\ complains when using @lhs2TeX@ in ``\textbf{poly}'' style
with the @jfp@ class.
\end{problem}
Add the line @%include jfpcompat.fmt@ to the preamble of your document.

\begin{problem}
\LaTeX\ claims that the package @polytable@ (or @lazylist@) 
cannot be found, or that the version installed on your system
is too old.
\end{problem}
Did you install @polytable.sty@ (or @lazylist.sty@) 
in your \TeX\ system manually?
If you have absolutely no idea how to do this, you may try to
copy both @polytable.sty@ and @lazylist.sty@ from the
@lhs2TeX@ distribution into your working directory.

\begin{problem}
Haskell strings are displayed without double quotes. 
\end{problem}
This is
a result from using an old @lhs2TeX.fmt@ file together with
a new version of @lhs2TeX@. Usually, this stems from the fact
that there is an old version in the working directory. Now,
@lhs2TeX@ maintains a search path for included files, thus
usually a local old copy of @lhs2TeX.fmt@ can be removed.

\begin{problem}
In ``math'' style, I have aligned several symbols on one
column, but @lhs2TeX@ still won't align the code block.
\end{problem}
Did you set the alignment column correctly using the @%align@
directive? Note also that @lhs2TeX@ starts counting columns
beginning with |1|, whereas some editors might start counting
with |0|.

\begin{problem}
Large parts of the formatted file look completely garbled.
Passages are formatted as code or verbatim, although they are 
plain text. Conversely, things supposed to be code or verbatim
are typeset as text.
\end{problem}
You probably forgot multiple @|@ or \verb+@+ characters.
Because @lhs2TeX@ identifies both the beginning and end of
inline code or inline verbatim via the same character, one
missing delimiter can confuse @lhs2TeX@ and cause large
passages to be typeset in the wrong way. You should locate
the first position in the document where something goes wrong
and look for a missing delimiter at the corresponding position 
in the source file.

\begin{problem}
\LaTeX\ complains about a ``nested @\fromto@'' in ``poly'' style.
\end{problem}
This usually is a problem with one of your formatting directives.
If you start a \TeX\ group in one of your directives but do not
close it, then this error arises. You should not write such unbalanced
formatting directives unless you make sure that they do never span
an aligned column.
%TODO: Write example.

\begin{thebibliography}{99}

\bibitem{polytable}
  Andres L\"oh. \emph{The @polytable@ package.}
  \url{http://ctan.org/tex-archive/macros/latex/contrib/polytable/}

\bibitem{lazylist}
  Alan Jeffrey. \emph{The @lazylist@ package.}
  \url{http://ctan.org/tex-archive/macros/latex/contrib/lazylist/}

\bibitem{uuag}
  Arthur Baars, S.~Doaitse Swierstra, Andres L\"oh.
  \emph{The UU AG System User Manual.}
  \url{http://www.cs.uu.nl/~arthurb/data/AG/AGman.pdf}

\bibitem{array}
  Frank Mittelbach and David Carlisle.
  \emph{The @array@ package.}
  \url{http://www.ctan.org/tex-archive/macros/latex/required/tools/array.dtx}

\bibitem{gh}
  Andres L\"oh.
  \emph{Exploring Generic Haskell.}
  PhD Thesis, Utrecht University, 2004.

\end{thebibliography}

\end{document}

%%%
%%%

\section{Test}

\input{Variable}


%%%
%%%





\end{document}

