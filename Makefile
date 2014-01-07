OCAMLC=ocamlfind ocamlc -package js_of_ocaml -I ../corejs -I ../ocamlfrp -c

all: jq

jq:
	$(OCAMLC) jq.mli jq.ml
