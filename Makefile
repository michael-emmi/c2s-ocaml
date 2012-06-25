
include config.make

NAME := c2s
SRC := src
TARBALL := $(NAME)-$$(date "+%m-%d-%Y").tar.gz

OBJDIR := obj
TESTDIR := $(SRC)/test
TESTLOGS := test-log
DEPENDDIR := obj/.depend
SOURCEDIRS := $(SRC) $(SRC)/cp $(SRC)/bp $(SRC)/bpl $(SRC)/pn $(SRC)/cfg $(TESTDIR)
TOOLSDIR := tools

BINDIR := bin
DOCDIR := doc
LIBDIR := lib
INCDIR := include

CAML_LIB = lib$(NAME)
CAML_LIB_OBJ = \
	Prelude PrettyPrinting \
	Options ParsingUtils \
	Cfg CfgParser CfgLexer \
	PnAst PnParser PnLexer \
	BpAst BpParser BpLexer BpUtils \
	BplAst BplParser BplLexer BplUtils BplTranslateUtils \
	Parikh \
	BplViolin \
	PnToBpl \
	BplEscAsync \
	BplAsyncToSeq \
	CpAst CpParser CpLexer CpTyping CpUtils \
	CpToBp CpToBpl \
	CpTranslateUtils \
	CpAsserts \
	CpMultiToSingle \
	CpAsyncToSeq \
	CpEliminateYield \

SOURCES = $(shell find $(SRC) $(OBJDIR) -name "*.ml" -or -name "*.mli")
CMOFILES = $(addprefix $(OBJDIR)/, $(addsuffix .cmo, $(CAML_LIB_OBJ)))
CMIFILES = $(addprefix $(OBJDIR)/, $(addsuffix .cmi, $(CAML_LIB_OBJ)))


CAML_TEST_OBJ = # TestPrelude test_prover test_suite

CAML_TPL_EXE = $(NAME)top

CAML_COMMON_EXE_OBJ = 

CAML_EXE = $(NAME)
CAML_EXE_OBJ = $(CAML_LIB_OBJ) Main
CAML_INCLUDES = $(INCDIR)
CAML_LD_FLAGS = -I $(LIBDIR)
CAML_OPT_LD_FLAGS = -I $(LIBDIR)
CAML_LD_LIBS = unix str nums

MODULES = $(CAML_LIB_OBJ) Main $(CAML_TEST_OBJ)
MLYS	= CpParser.mly BpParser.mly BplParser.mly PnParser.mly CfgParser.mly
MLLS	= CpLexer.mll BpLexer.mll BplLexer.mll PnLexer.mll CfgLexer.mll

TOOLS = 

TODO = $(TOOLS) exe

all:: $(TODO)

test: exe
	@$(NARRATIVE) Running regression tests.
	@$(BINDIR)/regression-test.sh

dist:
	@$(NARRATIVE) Making distributible archive $(TARBALL)
	@ln -s -f -h . c2s
	@tar zcvf $(TARBALL) -X .ignores -H	c2s 2> /dev/null
	@rm c2s

doc: $(CMOFILES)
	@$(NARRATIVE) Making API documentation
	@mkdir -p $(DOCDIR)
	@ocamlfind ocamldoc -html -sort -d $(DOCDIR) \
		-I $(OBJDIR) -I $(INCDIR) \
		$$(find $(SRC) $(OBJDIR) -name "*.ml" -or -name "*.mli")

clean: default-clean
distclean: clean
	@rm -rf *.cmi *.annot $(TODO)
	@rm -rf $(OBJDIR) $(DOCDIR) $(TESTLOGS)
	@rm -f $(BINDIR)/$(CAML_TPL_EXE) $(BINDIR)/$(CAML_EXE) $(TARBALL)

exe :: $(BINDIR)/$(CAML_EXE)

top :: $(BINDIR)/$(CAML_TPL_EXE)

$(BINDIR)/$(CAML_TPL_EXE): $(CAML_LIB_OBJ:%=$(OBJDIR)/%.cmo)
	@$(NARRATIVE) Making top-level $@ from $(CAML_LIB_OBJ:%=%.cmo) \
		$(CAML_LD_LIBS:%=%.cma)

	$(AT)ocamlfind ocamlmktop $(CAML_FLAGS) $(CAML_LD_FLAGS) \
		-o $@ -linkpkg \
		-I $(OBJDIR) -I $(TESTDIR) \
		$(CAML_LD_LIBS:%=%.cma) $(CAML_LIB_OBJ:%=%.cmo)

lib:: $(LIBDIR)/$(CAML_LIB).cma $(LIBDIR)/$(CAML_LIB).cmxa

.PHONY : all clean distclean doc test dist

include rules.make
