# Concurrent to Sequential Program Transformations #

---

## Purpose ##

The purpose of this `c2s` tool is to provide code-to-code translations from
concurrent programs to sequential programs.  We currently parse, print, and
translate Boogie code (with annotations denoting concurrent semantics), and
have partial (perhaps deprecated) support for Boolean programs as well.

## Dependencies ##

* [OCaml][ocaml] -- currently compiling with 4.00.1
* [Findlib][findlib]

## Getting Started ##

To get going, simply build the sources..

	make
	   
and run a demo, which parses  simple Boogie program and prints it back out

	./bin/c2s src/test/bpl/simple.bpl --print -

then look at the usage options 

	./bin/c2s

There are also `ocmaldoc`-generated API documentation if you like

	make doc

which gets put into `doc/index.html`.

## Further questions ##

Contact the author, [Michael Emmi][email].


[ocaml]:http://caml.inria.fr/ocaml/index.en.html
[findlib]:http://projects.camlcity.org/projects/findlib.html
[ounit]:http://ounit.forge.ocamlcore.org/
[email]:mailto:michael.emmi@gmail.com
