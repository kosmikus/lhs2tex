%include poly.fmt

In the beginning: |one|.\par
%format one = "\mathsf{1}"
Before the group: |one|.\par
%{
%format one = "\mathsf{one}"
Inside the group: |one|.\par
%}
After the group: |one|.
