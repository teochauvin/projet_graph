#!/bin/bash

ocamlc -c graphStructure.ml && 
ocamlfind ocamlc -package yojson -linkpkg -g -c instanceConverter.ml graphStructure.cmo &&
ocamlfind ocamlc -package yojson -linkpkg -g -o convert graphStructure.cmo instanceConverter.cmo convert.ml && 
ocamlfind ocamlc -package yojson -linkpkg -g -o color graphStructure.cmo instanceConverter.cmo color.ml