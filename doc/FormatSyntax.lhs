%include tex.fmt
%format symorname = sym_or_name

\invisiblecomments
\begin{code}
dir(format) ^^ ent(token) = many(ent(fmttoken))     ^^^  -- (format single tokens)
dir(format) ^^ ent(lhs) = many(ent(fmttoken))            -- (parametrized formatting)
dir(format) ^^ ent(name)                                 -- (implicit formatting)

ent(lhs)        ::=  ent(name) ^^ many(ent(arg)) | (ent(name)) ^^ many(ent(arg))
ent(name)       ::=  ent(varname) | ent(conname)
ent(arg)        ::=  ent(varname) | (ent(varname))
ent(fmttoken)   ::=  string(ent(text)) | ent(token)
\end{code}
%old stuff:
%ent(lhs)        ::=  ent(symorname) ^^ many(ent(arg)) | (ent(symorname) ^^ many(ent(arg)))
%ent(symorname)  ::=  ent(name) | back(ent(name)) | (ent(operator))
