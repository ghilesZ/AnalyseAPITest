# AnalyseAPITest

Useful links:

OCaml's parsetree
https://v2.ocaml.org/api/compilerlibref/Parsetree.html

How to compile with dune:

````sh
dune build
````

How to execute with dune: if you have a *main.ml* file

````sh
dune exec ./main.exe
````

Note the *.exe* !

How to get the parsetree of an OCaml program
````sh
ocamlc -dparsetree a.ml
````
