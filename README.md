# Concurrent to Sequential Program Transformations #

---

## Purpose ##

The purpose of this `c2s` tool is to provide parsers, printers, and
translators for/between Boolean programs, Boogie programs, and our own flavor
of "concurrent programs" -- a variation on the aformentioned with concurrency
constructs such as `yield` and `post`.

## Dependencies ##

* [OCaml][ocaml] -- developed with 3.12
* [Findlib][findlib]

## Getting Started ##

To get going, simply build the sources..

	make
	   
and run a demo, which parses the sample program and prints it back out

	./bin/c2s src/test/cp/syntax-tutorial.cp --print -

then look at the usage options 

	./bin/c2s

There are also `ocmaldoc`-generated API documentation if you like

	make doc

which gets put into `doc/index.html`.

A simple suite of regression tests can also be run

	make test

which attempts to test the parsers and end-to-end verification.

## Further questions ##

Contact the author, [Michael Emmi][email].


[ocaml]:http://caml.inria.fr/ocaml/index.en.html
[findlib]:http://projects.camlcity.org/projects/findlib.html
[ounit]:http://ounit.forge.ocamlcore.org/
[email]:mailto:michael.emmi@gmail.com
