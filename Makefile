OCAMLC=ocamlfind ocamlc -package js_of_ocaml -I ../corejs -I ../ocamlfrp -c

all: jq widgets

jq:
	$(OCAMLC) jq.mli jq.ml

widgets: jq
	$(OCAMLC) jq.cmo widgets.mli widgets.ml

clean:
	rm *.cmo
	rm *.cmi
