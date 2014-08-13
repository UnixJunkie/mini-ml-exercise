
all:
	ocamlc -c ast.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o test lexer.cmo parser.cmo ast.cmo main.cmo

clean:
	\rm -f *.mli *.cmi *.cmo test lexer.ml parser.ml
