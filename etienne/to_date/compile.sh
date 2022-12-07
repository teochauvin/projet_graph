#!/bin/bash

ocamlc -c graph_structure.ml && 
ocamlfind ocamlc -package yojson -linkpkg -g -c instances_utils.ml graph_structure.cmo &&
ocamlfind ocamlc -package yojson -linkpkg -g -o main graph_structure.cmo instances_utils.cmo main.ml