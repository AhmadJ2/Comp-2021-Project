MKDIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
BASEDIR := $(PWD)

.phony: 

all:

	 ocaml compiler.ml $(BASEDIR)/file.scm > test.s && nasm -f elf64 -o test.o test.s && gcc -static -m64 -o test test.o #&& mv $@ $(BASEDIR)
	 ./test
ass:
	nasm -f elf64 -o test.o test.s && gcc -static -m64 -o test test.o; ./test