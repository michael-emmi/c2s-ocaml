SHELL = /bin/bash
DEBUG = 1

C_C = gcc
CPP_C = g++

CAML_C = ocamlc
CAML_OPT_C = ocamlopt
CAML_LD = $(CAML_C)
CAML_OPT_LD = $(CAML_OPT_C)

CAML_C_FLAGS = -dtypes -warn-error A -I $(OBJDIR)
CAML_OPT_C_FLAGS = -unsafe -noassert -I $(OBJDIR)
CAML_LD_FLAGS = 
CAML_OPT_LD_FLAGS =

ifdef DEBUG
CAML_C_FLAGS += -g 
CAML_OPT_C_FLAGS += -g 
CAML_LD_FLAGS += -g 
CAML_OPT_LD_FLAGS += -g 
endif

CAML_DEP = ocamldep
CAML_LEX = ocamllex
CAML_YACC = ocamlyacc -v
