%include poly.fmt

%if False

> module Interactive where

%endif
%format . = "."
%format forall a = "\forall" a

> fix f = f (fix f)

%options ghci -fglasgow-exts
\textbf{ghci:}
This function is of type \eval{:t fix},
and |take 10 (fix ('x':))| 
evaluates to \eval{take 10 (fix ('x':))}.

%options hugs -98
\textbf{hugs:}
This function is of type \eval{:t fix},
and |take 10 (fix ('x':))| 
evaluates to \eval{take 10 (fix ('x':))}.

%options ghci-5.04.3 -fglasgow-exts
\textbf{ghci-5.04.3:}
This function is of type \eval{:t fix},
and |take 10 (fix ('x':))| 
evaluates to \eval{take 10 (fix ('x':))}.
