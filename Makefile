include Makefile.config

CAML_C_FLAGS = -dtypes -warn-error A -I $(OBJDIR)
CAML_OPT_C_FLAGS = -unsafe -noassert -I $(OBJDIR)
CAML_LD_FLAGS = 
CAML_OPT_LD_FLAGS =

MOVE_AFTER_CAML_C = cmi cmo cmx o

NARRATIVE := echo
COMMAND := true
AT := @
ECHO := @

CAMLFLAGS = $(OPT_FLAGS) -I $(OBJDIR)

ifdef DEBUG
CAMLFLAGS += -g
endif

ifdef DEBUG
CAML_C_FLAGS += -g
CAML_OPT_C_FLAGS += -g
CAML_LD_FLAGS += -g
CAML_OPT_LD_FLAGS += -g
endif

SRC := src
TARBALL := $(NAME)-$$(date "+%m-%d-%Y").tar.gz

OBJDIR := obj
TESTDIR := $(SRC)/test
TESTLOGS := test-log
DEPENDDIR := obj/.depend
SOURCEDIRS := $(SRC) $(SRC)/bp $(SRC)/bpl $(SRC)/pn $(SRC)/cfg $(TESTDIR)
TOOLSDIR := tools

BINDIR := bin
DOCDIR := doc
LIBDIR := lib
INCDIR := include

CAML_LIB = lib$(NAME)
CAML_LIB_OBJ = \
	Prelude PrettyPrinting ParsingUtils \
	BplAst BplParser BplLexer BplUtils BplMarkers \
  BplInitAxioms \
	BplIdentifyEntryPoints \
  BplWrapEntrypoints BplYieldElimination BplAsyncToSeq BplAsyncWithWait \
	BplCompleteReturnAssignments \
	BplAsserts BplBackend \
	BplViolin
    
DEPRICATED_LIB_OBJ = \
	Cfg CfgParser CfgLexer Parikh \
	PnAst PnParser PnLexer PnToBpl \
	BpAst BpParser BpLexer BpUtils \
	BplEscAsync BplFifoSeq BplMultiToSingle

SOURCES = $(shell find $(SRC) $(OBJDIR) -name "*.ml" -or -name "*.mli")
CMOFILES = $(addprefix $(OBJDIR)/, $(addsuffix .cmo, $(CAML_LIB_OBJ)))
CMIFILES = $(addprefix $(OBJDIR)/, $(addsuffix .cmi, $(CAML_LIB_OBJ)))


CAML_TEST_OBJ = # TestPrelude test_prover test_suite


CAML_TPL_EXE = $(NAME)top


CAML_COMMON_EXE_OBJ = 

OS=$(shell uname -a)

ifeq (1, $(words $(filter $(OS), Cygwin Mingw)))
	CAML_EXE = $(NAME).exe
else
	CAML_EXE = $(NAME)
endif

CAML_EXE_OBJ = $(CAML_LIB_OBJ) Main
CAML_INCLUDES = $(INCDIR)
CAML_LD_FLAGS = -I $(LIBDIR)
CAML_OPT_LD_FLAGS = -I $(LIBDIR)
CAML_LD_LIBS = unix str nums

MODULES = $(CAML_LIB_OBJ) Main $(CAML_TEST_OBJ)
MLYS = $(notdir $(shell find $(SRC) -name "*.mly"))
MLLS = $(notdir $(shell find $(SRC) -name "*.mll"))

TOOLS = 

TODO = $(TOOLS) exe

include Makefile.rules

distclean:: clean
	rm -f Makefile.config
