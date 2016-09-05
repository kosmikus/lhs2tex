
\documentclass[10pt]{scrartcl}

% save linebreak; see below
\let\origlinebreak=\\

\renewcommand{\sectfont}{\bfseries}

\usepackage[english]{babel}
%\usepackage[inference]{semantic}
%\usepackage[fleqn]{amsmath}
\usepackage{stmaryrd}

\usepackage[code,screen,nopanel,sectionbreak]{pdfscreen}
\DeleteShortVerb{\||}

\usepackage{mathpazo}
\usepackage{colortbl}
\usepackage{calc}
\usepackage{pifont}
\usepackage{paralist}
%\usepackage{soul}
\usepackage{ifthen}
\usepackage{relsize}

%include lhs2TeX.fmt
%include lhs2TeX.sty

\newlength{\lwidth}
\newlength{\cwidth}
\setlength{\lwidth}{0pt}
\setlength{\cwidth}{0pt}

%separation 2
%latency 2

\let\origcolor=\color
\definecolor{hcolor}{rgb}{1,0,0}
\newcommand{\dep}[1]{{\origcolor{red}#1}}
\def\swgt#1{\switch[\value{step}>#1]}%
\def\ro#1{\ifthenelse{\value{step}=#1}{\origcolor{red}}{}}%

\setdefaultitem{\ding{217}}{}{}{}

\usepackage[display]{texpower}

%hyperref needs some setup, especially after pdfscreen
\hypersetup{%
  pdfmenubar=True,%
  pdfcenterwindow=False,% 
  pdffitwindow=False}%

%fixed lengths are better ... 
\AtBeginDocument{%
\setlength{\abovedisplayskip}{6pt plus 0pt minus 0pt}% originally 10.0pt plus 2.0pt minus 5.0pt
\setlength{\belowdisplayskip}{6pt plus 0pt minus 0pt}% originally 10.0pt plus 2.0pt minus 5.0pt
}
\setlength{\belowdisplayshortskip}{6pt plus 0pt minus 0pt}%
\setlength{\abovedisplayshortskip}{6pt plus 0pt minus 0pt}%
\setlength{\smallskipamount}{2pt}
\setlength{\medskipamount}{5pt}
\setlength{\bigskipamount}{10pt}

\begin{screen}
  \margins{.3in}{.3in}{.3in}{.3in}  
  \screensize{3.75in}{4.8in}
  %\overlay{overlay1.pdf}
  \overlay{myov4.pdf}
  \setlength{\parindent}{0pt}
  \raggedright
\end{screen}

\setlength\pltopsep{2pt}
\setlength\plitemsep{1pt}
\setlength\parskip{0pt}

\newcounter{pagesave}

\def\BottomText{}
\def\slide{\ifthenelse{\value{pagesave}=\value{page}}{}{\def\BottomText{}}\section*}
\def\SkipToConclusionOption{%
  \setcounter{pagesave}{\value{page}}%
  \expandafter\def\expandafter\BottomText\expandafter{\BottomText
     \quad\hyperlink{SkipC}{(Skip to Conclusions)}}}
\def\SkipToSummaryOption{%
  \setcounter{pagesave}{\value{page}}%
  \expandafter\def\expandafter\BottomText\expandafter{\BottomText
     \quad\hyperlink{SkipS}{(Skip to Summary)}}}
\def\SkipTo#1#2{%
  \setcounter{pagesave}{\value{page}}%
  \expandafter\def\expandafter\BottomText\expandafter{\BottomText
     \quad\hyperlink{#1}{(#2)}\quad}}
\def\PrintDefault{{\tiny\color{section2}\raisebox{2ex}{\BottomText}}}
\makeatletter
\def\@@battrib{\color{section1}\footnotesize}
\def\@@@@buttons{\hfill\PrintDefault\hfill\vspace*{.15in}}
\def\TargetOnce#1#2{%
  \@@ifundefined{targetonce.#1}{%
    \message{targetting #1}%
    \hypertarget{#1}{#2}%
    \global\@@namedef{targetonce.#1}}{}{}}
\makeatother

\bottombuttons

\usepackage{polytable}
% redefining the lhs2TeX code command is needed because
% TeXpower seems to tamper with \\ in some nasty way ...

% This one works:
%%subst code a = "\begingroup\parskip=\abovedisplayskip\par\advance\leftskip\mathindent\let\\=\origlinebreak\('n\begin{pboxed}\SaveRestoreHook'n" a "\ColumnHook'n\end{pboxed}'n\)\parskip=\belowdisplayskip\par\endgroup\resethooks'n"

% This one is with color:
%subst code a = "\begin{colorcode}'n" a "\end{colorcode}\resethooks'n" 

\newenvironment{colorcode}{%
  \parskip=\abovedisplayskip\par\noindent
  \begingroup\small% small changes displayskips!
  \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
  \let\\=\origlinebreak
  \(%
  \pboxed\SaveRestoreHook}{%
  \ColumnHook\endpboxed
  \)%
  \endtabular
  \endgroup
  \parskip=\belowdisplayskip\par\noindent
  \ignorespacesafterend}

\newenvironment{colorsurround}{%
  \parskip=\abovedisplayskip\par\noindent
  \begingroup\small% small changes displayskips!
  \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
  \let\\=\origlinebreak}{%
  \endtabular
  \endgroup
  \parskip=\belowdisplayskip\par\noindent
  \ignorespacesafterend}

\newenvironment{colorarray}{%
  \parskip=\abovedisplayskip\par\noindent
  \begingroup\small% small changes displayskips!
  \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
  \let\\=\origlinebreak
  \(%
  \array}{%
  \endarray
  \)%
  \endtabular
  \endgroup
  \parskip=\belowdisplayskip\par\noindent
  \ignorespacesafterend}

\makeatletter
\newenvironment{colorverb}{%
  \parskip=\abovedisplayskip\par\noindent
  \begingroup\small% small changes displayskips!
  \tabular{@@{}>{\columncolor{codecolor}}p{\linewidth}@@{}}%
  \let\\=\origlinebreak}{%
  \endtabular
  \endgroup
  \parskip=\belowdisplayskip\par\noindent
  \ignorespacesafterend}
\makeatother

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

\title{\color{section0}Typesetting Haskell and more with @lhs2TeX@}
\author{{\color{section2}Andres L\"oh}\\
  Universiteit Utrecht\\
  \color{section2}\texttt{andres@@cs.uu.nl}}%
\date{September 8, 2004}
\maketitle

%%%
%%%

\slide{About @lhs2TeX@}

\begin{compactitem}
\item @lhs2TeX@ is a preprocessor
  \begin{compactitem}
  \item Input: a literate Haskell source file
  \item Output: a formatted file, depending on style of operation
  \end{compactitem}
\item Possible input:
\input{HelloWorldInput}
\end{compactitem}

%%%
%%%

\slide{Hello, world!}

\begin{compactitem}
\item @lhs2TeX@ is a preprocessor
  \begin{compactitem}
  \item Input: a literate Haskell source file
  \item Output: a formatted file, depending on selected style
  \end{compactitem}
\item Possible output:
\begin{colorsurround}
\input{HelloWorld}
\end{colorsurround}
\item From input to output:
\input{HelloWorldDialogue}
\end{compactitem}

%%%
%%%

\slide{Styles}

\begin{compactitem}
\item @lhs2TeX@ has several styles with different behaviour:
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
\end{compactitem}

%%%
%%%

\slide{Example of ``verb'' style}

\input{Zip}

%%%
%%%

\slide{Example of ``tt'' style}

\input{ZipTT}

Differences from \textbf{verb} style:
\begin{compactitem}
\item Some of Haskells symbols can be expressed more naturally.
\item Keywords can be highlighted.
\end{compactitem}

%%%
%%%

\slide{Drawback of formatting}

\input{ZipStupid}

\begin{compactitem}
\item Alignment information is lost.
\end{compactitem}

%%%
%%%

\slide{Example of ``math'' style}

\input{ZipMath}

\begin{compactitem}
\item Only one alignment column, plus indentation.
\end{compactitem}

%%%
%%%

\slide{Example of ``poly'' style}

\input{ZipPoly}
\begin{compactitem}
\item Complex layouts are possible.
\end{compactitem}

%%%
%%%

\slide{History of @lhs2TeX@}

\begin{compactitem}
\item Ralf Hinze started development in 1997. Most of the hard work has
  been done by him!
\item The program is based on @smugweb@ and @pphs@, both of which are
  no longer available and I do not know.
\item I picked up development in 2002, and added
  the \textbf{poly} and \textbf{newcode} styles.
%\item Future: I consider the \textbf{tt} and \textbf{math} styles as deprecated,
%  I want to add more language independence (customizable lexer) and 
%  extend/improve the formatting language.
\end{compactitem}

%%%
%%%

% End of introduction part -- begin of reference

%%%
%%%

\slide{@lhs2TeX@ operation}

\begin{compactitem}
\item When given an input file, @lhs2TeX@ does only look at the following
      constructs:
\begin{compactitem}
\item Directives.
\item Text between two \verb+@+ characters. Such text is considered inline
      verbatim. Any \verb+@+ in the source file needs to be escaped: \verb+@@+.
\item Text between two @|@ characters. Such text is considered inline code.
\item Lines indicating a Bird-style literate program (i.e. lines beginning
      with either @>@ or @<@) are considered as code blocks.
\item Lines surrounded by @\begin{code}@ and @\end{code}@ statements, or
      by @\begin{spec}@ and @\end{spec}@ statements, are considered as
      code blocks.
\end{compactitem}
\item Everything else is considered plain text and either ignored 
      (for \textbf{verb}, \textbf{tt}, \textbf{math}, and \textbf{poly})
      or discarded (for \textbf{code} and \textbf{newcode}).
\end{compactitem}

%%%
%%%

\slide{Directives}

\begin{compactitem}
\item
@lhs2TeX@ interprets a number of directives. 

\item
Directives can occur
on all non-code lines and start with a @%@, the \TeX\ comment character,
immediately followed by the name of the directive, plus a list of potential
arguments.

\item
These are the directives we will learn about in this talk:
\input{Directives}
\end{compactitem}

%%%
%%%

\slide{Including files}

\begin{compactitem}
\item Other files can be included by @lhs2TeX@. 
      \input{IncludeSyntax}
\item Using @%include@, not only other sources, but also other directives
      can be included. 

\item The specified file is searched for in the @lhs2TeX@
      source path which can be modified using environment variables or
      the @-P@ command line option.

\item Included files are inserted literally at the position of the
      @%include@ directive. The @lhs2TeX@ inclusion is therefore
      entirely independent of \TeX\ or Haskell includes/imports.
\end{compactitem}

%%%
%%%

\slide{The @lhs2TeX@ ``prelude''}

\begin{compactitem}
\item Several aspects of the behaviour of @lhs2TeX@ are not hardcoded,
      but configurable via directives.
\item A minimal amount of functionality has to be defined so that @lhs2TeX@
      can operate usefully.
\item Essential definitions are collected in two files, @lhs2TeX.fmt@
      (containing basic directives) and @lhs2TeX.sty@ (containing basic
      \LaTeX\ setup). These two files should be included -- directly or 
      indirectly -- in every file to be processed by @lhs2TeX@!
      \input{IncludePrelude}
\item It is perfectly possible to design own libraries that replace or extend
      these basic files and to include those own libraries instead.
\end{compactitem}

%%%
%%%

\slide{Formatting}

\begin{compactitem}
\item Using the @%format@ directive, tokens can be given a different
      appearance.
      \input{FormatSyntax}
\item Let us look at a couple of examples.
\end{compactitem}

%%%
%%%

\slide{Formatting identifiers}

\begin{compactitem}
\item Input:
      \input{FormatGreekIn}
\item Output:
      \input{FormatGreekOut}
\end{compactitem}

%%%
%%%

\slide{Parametrized formatting directives}

\begin{compactitem}
\item Formatting directives can be parametrized. The parameters may occur
      once or more on the right hand side.
\item Input:
      \input{CardIn}
\item Output:
      \begin{colorsurround}
      \input{Card}
      \vspace*{-2\baselineskip}%
      \end{colorsurround}
\end{compactitem}

%%%
%%%

\slide{Parentheses}

\begin{compactitem}
\item Sometimes, due to formatting, parentheses around arguments or the entire
      function become unnecessary.
\item Therefore, @lhs2TeX@ can be instructed to drop parentheses around an argument
      by enclosing the argument on the left hand side of the directive in parentheses.
\item Parentheses around the entire function are dropped if the entire left hand side
      of the directive is enclosed in parentheses.
\end{compactitem}

%%%
%%%

\slide{Parentheses -- example}

\begin{compactitem}
\item Input:
      \input{ParensExampleIn}
\item Output:
      \begin{colorsurround}
      \input{ParensExample}
      \vspace*{-2\baselineskip}%
      \end{colorsurround}
\end{compactitem}

%%%
%%%

\slide{Parentheses -- example}

\begin{compactitem}
\item Input:
      \input{ParensExample2In}
      
\item Output:
      \begin{colorsurround}
      \input{ParensExample2}
      \vspace*{-2\baselineskip}%
      \end{colorsurround}
\end{compactitem}


%%%
%%%

\slide{Local formatting directives}

\begin{compactitem}
\item Usually, formatting directives scope over the rest of the input.
\item Formatting directives can be placed into \textbf{groups}.
      \input{GroupSyntax}
\item Formatting directives that are defined in a group scope only over
      the rest of the current group. 
\end{compactitem}

%%%
%%%

\slide{Local formatting directives -- example}

\begin{compactitem}
\item Input:
      \input{GroupExampleIn}
\item Output:
      \begin{colorsurround}
      \input{GroupExample}
      \end{colorsurround}
\end{compactitem}

%%%
%%%

\slide{Nested applications of formatting directives}

The right-hand sides of formatting directives are processed as follows:
\begin{compactitem}
\item A string, enclosed in @"@, will be reproduced literally (without
      the quotes).
\item A name, if it is the name of a parameter, will be replaced by the
      actual (formatted) argument.
\item A name, if it is the name of a non-parametrized formatting directive,
      will be replaced by that directive's replacement.
\item Any other name will be replaced by its standard formatting.
\end{compactitem}

%%%
%%%

\slide{Implicit formatting}

\begin{compactitem}
\item A variable (or constructor) name that ends in a number or a prime @'@
      can be used in an implicit formatting statement.
\item The prefix will be formatted as determined by the formatting directives
      in the input so far. The number will be added as an index, the prime 
      character as itself.
\end{compactitem}

%%%
%%%

\slide{Implicit formatting -- example}

\begin{compactitem}
\item Input:
      \input{ImplicitIn}
\item Output:
      \begin{colorsurround}
      \input{Implicit}
      \end{colorsurround}
\end{compactitem}

%%%
%%%

\slide{Formatting in the various styles}

\begin{compactitem}
\item Formatting directives are applied in \textbf{math}, \textbf{poly}, and
      \textbf{newcode} styles. 
\item In \textbf{tt} style, only non-parametrized apply.
\item In \textbf{verb} and \textbf{code} styles, formatting directives are ignored.
\end{compactitem}

%%%
%%%

\slide{Alignment in ``poly'' style}

\begin{compactitem}
\item Alignment is computed per code block.
\item All tokens that start on the same column and are preceded by at
      least \textbf{2} spaces are horizontally aligned in the output.
\item (Almost) everything is possible, but watch out for 
      accidental alignments!
\end{compactitem}
      
%%%
%%%

\slide{Alignment example}

\begin{compactitem}
\item Input:
      \input{RepAlgIn}
\item The red {\origcolor{red} @lt@} is not aligned
      (only one preceding space).
\item Output:
      \input{RepAlg}
\end{compactitem}

%%%
%%%

\slide{Accidental alignment example -- input}

\input{AccidentalIn}

\begin{compactitem}
\item
  The red items will be unintentionally aligned because
  they start on the same column, with two or more preceding spaces
  each.
\item
  To correct, insert extra spaces to ensure that unrelated
  tokens start on different columns.
\end{compactitem}

%%%
%%%

\slide{Accidental alignment example -- continued}

\begin{compactitem}
\item Output:
      \input{Accidental}

\item Corrected version:
      \input{AccidentalC}
\end{compactitem}

%%%
%%%

%if False
\slide{Tweaking the alignment behaviour}

\begin{compactitem}
\item Alignment is computed per code block.
\item Per code block there are a number of \textbf{alignment columns}.
\item If a token starts in column |n| and is prefixed by at least @separation@
      spaces, then |n| is an \textbf{alignment column} for the code block.
\item If a token starts in an alignment column |n| and is prefixed by at least 
      @latency@ spaces, then the token is \textbf{aligned} at column |n|.
\item All tokens that are aligned at a specific column will appear aligned
      (i.e. at the same horizontal position) in the output.
\item It can be useful to increase the default settings of 2 and 2 for
      large code blocks where accidental alignments become very likely!
\end{compactitem}

\input{SepLatSyntax}
%endif

%%%
%%%

\slide{Indentation in ``poly'' style}

\begin{compactitem} 
\item If a line is indented in column |n|, then
the \textbf{previous} code line is taken into account:
  \begin{compactitem} 
  \item If there is an aligned token at column |n| in the previous
        line, then the indented line will be aligned normally.
  \item Otherwise, the line will be indendet with respect to the
        first aligned token in the previous line to the left of column |n|.
  \end{compactitem}
\end{compactitem}

%%%
%%%

\slide{Indentation in ``poly'' style -- example}

\begin{compactitem}
\item Input:
      \input{Indent1In}
\item Output:
      \input{Indent1}
\item In this example, there is an aligned token in the previous line
      at the same column, so everything is normal.
\end{compactitem}

%%%
%%%

\slide{Indentation in ``poly'' style -- example}

\begin{compactitem}
\item Input:
      \input{Indent2In}
\item Output:
      \input{Indent2}
\item In this example, there is no aligned token in the previous line
      at the same column. Therefore, the third line is indented with
      respect to the first aligned token in the previous line to the
      left of that column.
\end{compactitem}

%%%
%%%

\slide{Indentation in ``poly'' style -- example}

\begin{compactitem}
\item Input:
      \input{Indent3In}
\item Output:
      \input{Indent3}
\item In rare cases, the indentation heuristic can lead to surprising
      results. Here, the |1| is aligned with the |2|, but |2| is also
      indented with respect to |bar|.
\end{compactitem}


%%%
%%%

\slide{Advanced alignment topics}

\begin{compactitem}
\item Some columns (containing symbols) are centered by @lhs2TeX@
      (all other columns are left-aligned).
\item It is possible redefine the alignment of a specific column.
\item It is possible to customize the output environment (using
      @%subst@ directives). Using this, one can produce effects
      such as putting all code blocks into yellow boxes.
\item It is possible to save (and restore) column information.
\end{compactitem}

%%%
%%%

\slide{Saving and restoring column information\\ example -- input}

\input{SaveRestoreIn}

%%%
%%%

\slide{Saving and restoring column information\\ example -- output}

\input{SaveRestore}

%%%
%%%

\slide{Spacing}

\begin{compactitem}
\item @lhs2TeX@ does not really have a Haskell parser.
\item Because of this, it can be used for dialects of Haskell, too!
\item Spacing is handled automatically so that it works for correctly
      for pure Haskell most of the time.
\item A good trick is to define the following two pseudo-operators
      to correct wrong automatic spacing:
      \input{SpacingOps}
      \begin{compactitem}
      \item Use @^@ where you do \textbf{not} want a space, but @lhs2TeX@
            would place one.
      \item Use @^^@ where you \textbf{do} want a space, but @lhs2TeX@
            does not place one.
      \end{compactitem}
\end{compactitem}

%%%
%%%

\slide{{\smaller AG} code example -- input}

\input{AGExampleIn}

%%%
%%%

\slide{{\smaller AG} code example -- output}

\input{AGExample}

%%%
%%%

\slide{Calculation example -- input}

\vspace*{-\baselineskip}

\input{CalcExampleIn}

%%%

\slide{Calculation example -- output}

\input{CalcExample}

%%%
%%%

\slide{Defining variables}

\begin{compactitem}
\item @lhs2TeX@ allows flags (or variables) to be set by means of the
  @%let@ directive.
  \input{LetSyntax}
\item Expressions are built from booleans (either @True@ or @False@),
  integers, strings and previously define variables using some predefined, 
  Haskell-like operators.
\item Variables can also be defined by using the @-l@ or @-s@
  command line options.
\item @lhs2TeX@'s version is available as predefined @version@ variable,
  and the current style is available as predefined @style@ variable.
\end{compactitem}

%%%
%%%

\slide{Conditionals}

\begin{compactitem}
\item (Boolean) expressions can also be used in conditionals:
  \input{IfSyntax}
  The @%elif@ and @%else@ directives are optional.
\item Depending on the result of the evaluation of the expression,
  only the then or the else part are processed by @lhs2TeX@.
\end{compactitem}

%%%
%%%

\slide{Uses of conditionals}

\begin{compactitem}
\item Have different versions of one paper in one source. Depending
      on a flag, produce either the one or the other. Because the
      flag can be defined via a command line option, no modification
      of the source is necessary to switch versions.
\item Code that is needed to make the Haskell program work but that
      should not appear in the formatted article (module headers,
      auxiliary definitions), can be enclosed between @%if False@
      and @%endif@ directives, \textbf{or:}
\item If Haskell code has to be annotated for @lhs2TeX@ to produce
      the right output, define different formatting directives for
      the annotation depending on style (\textbf{poly} or \textbf{newcode}).
      Both code and \TeX\ file can then still be produced from a
      common source!
\end{compactitem}

%%%
%%%

\slide{Calling @ghci@}

\begin{compactitem}
\item It is possible to call @ghci@ (or @hugs@) using the @%options@
      directive. 
\item @lhs2TeX@ looks for calls to the \textbf{\TeX\ commands}
      @\eval@ and @\perform@ and feeds their arguments to the
      interpreter.
\item The current input file will be the active module. Therefore,
      this feature works only if the current file really is legal
      Haskell.
\end{compactitem}

%%%
%%%

\slide{Calling @ghci@ -- example}

\begin{compactitem}
\item Input:
      \input{InteractiveIn}
\item Output:
      \begin{colorsurround}
      \input{InteractiveGhci}
      \end{colorsurround}
\end{compactitem}

%%%
%%%

\slide{Implementation and distribution}

\begin{compactitem}
\item @lhs2TeX@ is written in Haskell
\item \textbf{poly} style makes use of a specifically written \LaTeX\ package
      @polytable@, which is included in the distribution
\item License is {\smaller GPL}.
\item There has not been an official release for a long time, so get the
      most recent version from the Subversion repository.
\item It is reported to work on Linux, Mac OS X, and Windows.
\item It has been used for several papers and seems to be quite stable.
\end{compactitem}

%%%
%%%

\slide{Future work}

\begin{compactitem}
\item More language independence (customizable lexer).
\item Clean up (and extend) the formatting directives language.
\item Allow directives during code blocks.
\item Add more features to @polytable@ package.
\item \dots
\end{compactitem}
Future development is relatively low priority, though.
If you want it, do it yourself or try to convince me
that it is urgent!

\end{document}

%%%
%%%

\slide{Test}

\input{Variable}


%%%
%%%

\slide{@poly@-style is customizable}

\input{ZipPolyTT}




\end{document}

