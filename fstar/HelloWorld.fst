module HelloWorld

// Example of extracting an OCaml source from an F* development and building a binary from it

let _ = FStar.IO.print_string "Hello world!"

// fstar.exe --use_hints --codegen OCaml --extract HelloWorld HelloWorld.fst
// ocamlfind ocamlc -warn-error -a -package fstarlib -linkpkg -g HelloWorld.ml -o helloworld.exe
