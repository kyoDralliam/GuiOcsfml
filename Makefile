


all: byte

byte:
	ocamlbuild -use-ocamlfind widget.cmo

native:
	ocamlbuild -use-ocamlfind widget.cmxa

clean:
	ocamlbuild -clean
