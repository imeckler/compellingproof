OCAMLC=ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -I ../corejs -I ../ocamlfrp


OBJS=../corejs/time.cmo ../corejs/either.cmo ../corejs/option.cmo ../corejs/core_list.cmo ../corejs/core_array.cmo ../corejs/arrow.cmo ../corejs/inttbl.cmo ../corejs/core_string.cmo ../corejs/core_queue.cmo ../corejs/core.cmo ../ocamlfrp/frp.cmo

all: jq widgets main

test: jq.cmo draw.cmo
	$(OCAMLC) -c test.ml
	$(OCAMLC) -linkpkg $(OBJS) jq.cmo draw.cmo -o test.byte test.cmo
	js_of_ocaml test.byte

draw.cmi: draw.mli
	$(OCAMLC) -c draw.mli 

draw.cmo: draw.ml jq.cmo draw.cmi
	$(OCAMLC) -c draw.ml

jq.cmi:
	$(OCAMLC) -c jq.mli

jq.cmo: jq.cmi
	$(OCAMLC) -c jq.ml

animate.cmi: animate.mli
	$(OCAMLC) -c animate.mli

animate.cmo: animate.ml animate.cmi
	$(OCAMLC) -c animate.ml

jq: jq.cmo jq.cmi

draw: draw.cmo draw.cmi

animate: animate.cmo animate.cmi

widgets: jq
	$(OCAMLC) jq.cmo -c widgets.mli widgets.ml

clean:
	rm *.cmo
	rm *.cmi

main: jq widgets
	ocamlfind ocamlc -package js_of_ocaml -I ../corejs -I ../ocamlfrp -c jq.cmo main.ml
	ocamlfind ocamlc -package js_of_ocaml -I ../corejs -I ../ocamlfrp \
		-linkpkg -o main.byte main.cmo ../corejs/core.cma ../ocamlfrp/frp.cma # jq.cmo widgets.cmo 
	js_of_ocaml -debuginfo -pretty main.byte

