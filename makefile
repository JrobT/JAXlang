all: main

main: lexer.cmo parser.cmo reader.cmo functions.cmo
	ocamlc -o mysplinterpreter str.cma lexer.cmo parser.cmo reader.cmo functions.cmo

lexer.cmo: lexer.ml parser.ml functions.cmo
	ocamlc -c lexer.ml

parser.cmo: parser.ml functions.cmo
	ocamlc -c parser.ml

reader.cmo: reader.ml functions.cmo
	ocamlc -w -10 -c reader.ml

lexer.ml: functions.cmo
	ocamllex lexer.mll

parser.ml: parser.mli functions.cmo
	ocamlc -c parser.mli

parser.mli: functions.cmo
	ocamlyacc parser.mly

functions.cmo: functions.ml
	ocamlc -c functions.ml
clean:
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f lexer.cmo
	rm -f parser.cmo
	rm -f reader.cmo
	rm -f *.cmi
	rm -f functions.cmo
