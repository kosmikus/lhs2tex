%include tex.fmt
\newcommand*{\InlineTeX}{@{-"@}%
\newcommand*{\EndInlineTeX}{@"-}@}%
%format inline(a) = "\InlineTeX " a "\EndInlineTeX "

\begin{code}
inline(ent(tex))
\end{code}
If this construct appears in a code block, then
|ent(tex)| is inserted literally into the output file.
