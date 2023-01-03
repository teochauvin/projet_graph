#!/bin/bash

ocamlc -c color_heap.ml &&
ocamlc -c color_heap.cmo graphStructure.ml && 
ocamlfind ocamlc -package yojson -linkpkg -g -c graphStructure.cmo instanceConverter.ml &&
ocamlfind ocamlc -package yojson -linkpkg -g -o convert color_heap.cmo graphStructure.cmo instanceConverter.cmo convert.ml && 
ocamlfind ocamlc -package yojson -linkpkg -g -o color color_heap.cmo graphStructure.cmo instanceConverter.cmo color.ml &&