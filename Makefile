
all:
	ocamlc -c -annot ast.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c -annot parser.mli
	ocamlc -c -annot lexer.ml
	ocamlc -c -annot parser.ml
	ocamlc -c -annot main.ml
	ocamlc -o test lexer.cmo parser.cmo ast.cmo main.cmo

clean:
	\rm -f *.mli *.cmi *.cmo *.annot test lexer.ml parser.ml
