MKDIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
BASEDIR := $(PWD)

.phony: 

all:

	 ocaml compiler.ml $(BASEDIR)/infile > test/test.s && nasm -f elf64 -o test/test.o test/test.s && gcc -static -m64 -o test/test test/test.o #&& mv $@ $(BASEDIR)
	 ./test/test
ass:
	nasm -f elf64 -o test/test.o test/test.s && gcc -static -m64 -o test/test test/test.o; ./test/test

one:
	ocaml compiler.ml $(BASEDIR)/file.scm > test/test.s

onee:
	ocaml compiler.ml $(BASEDIR)/file.scm > test/test1.s
rone:
	rm test1.s
ttt:
	ocaml compiler.ml $(BASEDIR)/infile > test/test.s && nasm -f elf64 -o test/test.o test/test.s && gcc -static -m64 -o test/test test/test.o