BYTE:=guiOcsfml.cma
NATIVE:=guiOcsfml.cmxa



all: build

build:
	ocamlbuild -use-ocamlfind $(BYTE) $(NATIVE)

byte:
	ocamlbuild -use-ocamlfind $(BYTE) 

native:
	ocamlbuild -use-ocamlfind $(NATIVE)

clean:
	ocamlbuild -clean

install: build
	ocamlfind install guiocsfml META $(BYTE:%=_build/%) $(NATIVE:%=_build/%)

uninstall:
	ocamlfind remove guiocsfml

test:
	ocamlbuild -use-ocamlfind Test/test_label.native Test/test_panel.native


.PHONY: clean
