MAIN=main.ml
EXEC=yaoc
BUILD=_build

NATIVE=$(MAIN:.ml=.native)

all:
	ocamlbuild -use-ocamlfind -yaccflags -v,--table $(NATIVE)
	mv $(NATIVE) $(EXEC)

test:
	ocamlbuild -use-ocamlfind -yaccflags -v,--table test.native

algow:
	ocamlbuild -use-ocamlfind -yaccflags -v,--table algow.native

clean:
	rm -rf $(BUILD)
	rm -f $(NATIVE)
	rm -f $(EXEC)
