
all:
	ocamlyacc parser.mly
	# ocamlc -c ast.ml
	# ocamlc -c parser.mli
	# ocamlc -c parser.ml


clean:
	\rm *.mli *.cmi *.cmo ast parser
