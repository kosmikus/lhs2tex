% lhs2TeX --poly UnlitP.lhs > UnlitP.tex

\documentclass{article}
\usepackage[german]{babel}

%-------------------------------=  --------------------------------------------

%include lhs2TeX.sty
%include lhs2TeX.fmt

%-------------------------------=  --------------------------------------------

\begin{document}

%-------------------------------=  --------------------------------------------
\section{\texttt{unlit}}
%-------------------------------=  --------------------------------------------

Literate-Skripte, die Bird-Tracks verwenden, in Skripte "uberf"uhren,
die die Pseudo-\TeX-Kommandos \verb|\begin{code}|, \verb|\end{code}|
verwenden. Das Programm realisiert einen einfachen UNIX-Filter.

Synopsis:
%
\begin{verbatim}
unlit
unlit <file>
\end{verbatim}

%-------------------------------=  --------------------------------------------

%if code || showModuleHeader

> module Main			(  main  )
> where
>
> import Char			(  isSpace  )
> import System			(  getArgs  )
> import Auxiliaries		(  (.>)  )

%endif

> data Class			=  Program LineNo Line
>				|  Spec    LineNo Line
>				|  Blank   LineNo Line
>				|  Comment LineNo Line
>
> type LineNo			=  Int
> type Line			=  String

> beginCode, endCode, 
>     beginSpec, endSpec	:: Line
> beginCode			=  "\\begin{code}"
> endCode			=  "\\end\&{code}"
> beginSpec			=  "\\begin{spec}"
> endSpec			=  "\\end{spec}"

\NB Damit \verb|"\\end{code}"| nicht das Codesegment beendet, wird ein
Nullstring eingef"ugt: \verb|"\\end\&{code}"|.

> main				:: IO ()
> main				=  do args <- getArgs
>				      unlit args
>
> unlit []			=  getContents       >>= (putStr . convert)
> unlit (filePath : _)		=  readFile filePath >>= (putStr . convert)

Fehlt: Fehlerbehandlung, wenn die Datei nicht vorhanden oder nicht
lesbar ist.

> convert			:: String -> String
> convert			=  lines
>				.> zip [1 ..]		-- number
>				.> map classify
>				.> format
>				.> unlines

> classify			:: (LineNo, Line) -> Class
> classify (n, '>' : s)		=  Program n (' ' : s)
> classify (n, '<' : s)		=  Spec    n (' ' : s)
> classify (n, s)
>     | all isSpace s		=  Blank   n s
> classify (n, s)		=  Comment n s

Die Formatierung wird mit einem einfachen endlichen Automaten mit f"unf
Zust"anden vorgenommen. Es wird darauf geachtet, da"s die Anzahl der
Zeilen nicht ver"andert wird.
\rightcolumn{52}%

> format			:: [Class] -> [Line]
> format []			=  []
> format (Program _ a : x)	=  (beginCode  ++ a) : inProgram x
> format (Spec    _ a : x)	=  (beginSpec  ++ a) : inSpec    x
> format (Blank   _ a : x)	=                      inBlank a x
> format (Comment _ a : x)	=                  a : inComment x

\rightcolumn{45}%
\rightcolumn{37}%

> inBlank			:: Line -> [Class] -> [Line]
> inBlank a []			=  [a]
> inBlank a (Program _ b : x)	=   beginCode  : b : inProgram x
> inBlank a (Spec    _ b : x)	=   beginSpec  : b : inSpec    x
> inBlank a (Blank   _ b : x)	=           a  :     inBlank b x
> inBlank a (Comment _ b : x)	=           a  : b : inComment x

\rightcolumn{45}%
\rightcolumn{37}%

> inProgram			:: [Class] ->  [Line]
> inProgram []			=  [endCode]
> inProgram (Program _  a : x)	=           a : inProgram x
> inProgram (Spec    n  a : x)	=  message n "program line" "specification line"
> inProgram (Blank   _  a : x)	=   endCode   : format    x
> inProgram (Comment n  a : x)	=  message n "program line" "comment"

\rightcolumn{45}%
\rightcolumn{37}%
	
> inSpec			:: [Class] -> [Line]
> inSpec []			=  [endSpec]
> inSpec (Program n  a : x)	=  message n "specification line" "program line"
> inSpec (Spec    _  a : x)	=           a : inSpec x
> inSpec (Blank   _  a : x)	=   endSpec   : format x
> inSpec (Comment n  a : x)	=  message n "specification line" "comment"

> inComment			:: [Class] -> [Line]
> inComment []			=  []
> inComment (Program n  a : x)	=  message n "comment" "program line"
> inComment (Spec    n  a : x)	=  message n "comment" "specification line"
> inComment (Blank   _  a : x)	=      inBlank   a x
> inComment (Comment _  a : x)	=  a : inComment x

> message			:: LineNo -> String -> String -> a
> message n x y			=  error ("line " ++ show n ++ ": "
>				++ x ++ " next to " ++ y ++ "\n")

\end{document}
