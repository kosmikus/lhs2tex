%include poly.fmt
%subst code a = "\begin{colorcode}'n" a "\end{colorcode}\resethooks'n" 

%{
%format rho = "\rho"
%format pi  = "\pi"
%format tau = "\tau"
%format sigma = "\sigma"
%format tau1
%format tau2
%format tau1'
%format tau2'
%format .==. = "\mathrel{\mathopen{.}\equiv\mathclose{.}}"
%format .->. = "\mathrel{\mathopen{.}\rightarrow\mathclose{.}}"
%format :->: = "\mathrel{\mathopen{:}\rightarrow\mathclose{:}}"

\begin{code}
functionMatch :: IsRho rho => rho -> Solve (Sigma, Rho)
functionMatch rho =
   case toRho rho of
      Exists [] (Pi tau) ->
         do  tau1   <-  newTau
             tau2   <-  newTau
             tau .==. (tau1 .->. tau2)
             tau1'  <-  subst tau1
             tau2'  <-  subst tau2
             return (toSigma tau1', toRho tau2')
      Exists [] (sigma :->: rho) ->
         do  return (sigma, rho)
      Exists is pi ->
         do  addError
               $ "*** Existential in functionMatch: " ++ show (toRho rho)
             functionMatch (toRho pi)
\end{code}

%}
