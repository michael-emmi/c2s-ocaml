NARRATIVE := echo
COMMAND := true
AT := @
ECHO := @

ifdef NATIVECAML
  CAMLC = ocamlopt -warn-error
  CAMLLINK = ocamlopt
  MOVEAFTERCAMLC = cmi cmx $(OBJ)
else
  CAMLC = ocamlc -dtypes -warn-error A
  CAMLLINK = ocamlc
  MOVEAFTERCAMLC = cmi cmo cmx o
endif


CAMLFLAGS = $(OPT_FLAGS) -I $(OBJDIR)

ifdef DEBUG
CAMLFLAGS += -g
endif

vpath %.mll $(SOURCEDIRS)
vpath %.mly $(SOURCEDIRS)
vpath %.ml $(SOURCEDIRS) $(OBJDIR)
vpath %.mli $(SOURCEDIRS) $(OBJDIR)
vpath %.c $(SOURCEDIRS)


.SECONDARY : $(MLLS:%.mll=$(OBJDIR)/%.ml) $(MLYS:%.mly=$(OBJDIR)/%.ml) \
	     $(MLYS:%.mly=$(OBJDIR)/%.mli)

ifdef NATIVECAML
$(BINDIR)/$(CAML_EXE): $(CAML_EXE_OBJ:%=$(OBJDIR)/%.cmx)
	@$(NARRATIVE) Linking $@ from $(CAML_EXE_OBJ:%=%.cmx) \
		$(CAML_LD_LIBS:%=%.cmxa)
	$(ECHO)mkdir -p $(@D)

	ocamlfind $(CAMLLINK) \
		$(CAMLFLAGS) $(CAML_OPT_LD_FLAGS) -o $@ -linkpkg \
		$(CAML_LD_LIBS:%=%.cmxa) $(CAML_EXE_OBJ:%=%.cmx)
else
$(BINDIR)/$(CAML_EXE): $(CAML_EXE_OBJ:%=$(OBJDIR)/%.cmo)
	@$(NARRATIVE) Linking $@ from $(CAML_EXE_OBJ:%=%.cmo) \
		$(CAML_LD_LIBS:%=%.cma)
	$(ECHO)mkdir -p $(@D)

	$(AT)ocamlfind $(CAMLLINK) \
		$(CAMLFLAGS) $(CAML_LD_FLAGS) -o $@ -linkpkg \
		$(CAML_LD_LIBS:%=%.cma) $(CAML_EXE_OBJ:%=%.cmo)
endif

$(OBJDIR)/%.ml: %.mll
	$(ECHO)mkdir -p $(@D)
	$(CAML_LEX) $<
	$(AT)mv -f $(basename $<).ml $(OBJDIR)
	$(ECHO)if test -f $(basename $<).mli; then \
	  $(COMMAND) cp -f $(basename $<).mli $(OBJDIR); \
	  cp -f $(basename $<).mli $(OBJDIR); \
	fi

$(OBJDIR)/%.ml $(OBJDIR)/%.mli: %.mly
	$(ECHO)mkdir -p $(@D)
	$(CAML_YACC) $<
	$(AT)mv -f $(basename $<).ml $(basename $<).mli $(OBJDIR)

$(OBJDIR)/%.cmi: %.mli
	@$(NARRATIVE) Compiling interface $<
	$(ECHO)mkdir -p $(@D)
	$(AT)$(CAML_C) $(CAML_C_FLAGS) $(CAML_INCLUDES:%=-I %) -c $<
	$(ECHO)if test $(OBJDIR) != $(<D); then \
	    $(COMMAND) mv -f $(basename $<).cmi $(OBJDIR); \
	    mv -f $(basename $<).cmi $(OBJDIR); \
	fi

$(OBJDIR)/%.cmo: %.ml
	@$(NARRATIVE) Compiling $<
	$(ECHO)mkdir -p $(@D)
	$(AT)ocamlfind $(CAML_C) -linkpkg \
		$(CAML_C_FLAGS) $(CAML_INCLUDES:%=-I %) -c $<
	$(ECHO)if test $(OBJDIR) != $(<D); then \
	    for ext in $(MOVEAFTERCAMLC); do \
	        if test -f $(basename $<).$$ext; then \
		    $(COMMAND) mv -f $(basename $<).$$ext $(OBJDIR); \
		    mv -f $(basename $<).$$ext $(OBJDIR); \
		fi; \
	    done; \
	fi

$(OBJDIR)/%.cmx: %.ml
	@$(NARRATIVE) Compiling $< \(to native code\)
	$(ECHO)mkdir -p $(@D)
	$(AT)ocamlfind $(CAML_OPT_C) -linkpkg \
		$(CAML_OPT_C_FLAGS) $(CAML_INCLUDES:%=-I %) -c $< -o $@
	$(ECHO)if test $(OBJDIR) != $(<D); then \
	    for ext in $(MOVEAFTERCAMLC); do \
	        if test -f $(basename $<).$$ext; then \
		    $(COMMAND) mv -f $(basename $<).$$ext $(OBJDIR); \
		    mv -f $(basename $<).$$ext $(OBJDIR); \
		fi; \
	    done; \
	fi

$(BINDIR)/test_suite: $(CAML_TEST_OBJ:%=$(OBJDIR)/%.cmo) \
		$(CAML_LIB_OBJ:%=$(OBJDIR)/%.cmo)
	@$(NARRATIVE) Building $@

	$(AT)ocamlfind $(CAML_C) \
		$(CAMLFLAGS) $(CAML_LD_FLAGS) -o $@ -linkpkg \
		-I $(OBJDIR) -I $(TESTDIR) \
		$(CAML_LD_LIBS:%=%.cma) \
		$(CAML_LIB_OBJ:%=%.cmo) \
		$(CAML_TEST_OBJ:%=%.cmo)

ifdef CAML_LIB
$(LIBDIR)/$(CAML_LIB).cma: $(CAML_LIB_OBJ:%=$(OBJDIR)/%.cmo)
	@echo Creating OCaml \(byte code\) library $@
	$(ECHO)mkdir -p $(@D)
	$(CAML_LD) $(CAML_LD_FLAGS) -a -o $@ $(CAML_LIB_OBJ:%=$(<D)/%.cmo)
$(LIBDIR)/$(CAML_LIB).cmxa $(LIBDIR)/$(CAML_LIB).a: $(CAML_LIB_OBJ:%=$(OBJDIR)/%.cmx)
	@echo Creating OCaml \(native code\) library $@
	$(ECHO)mkdir -p $(@D)
	$(CAML_OPT_LD) $(CAML_OPT_LD_FLAGS) -a -o $@ $(CAML_LIB_OBJ:%=$(<D)/%.cmx)
endif

.PHONY: default-clean
default-clean:
	@$(NARRATIVE) Cleaning
	$(ECHO)-find obj \( \
	    -name '*.cmi' -o \
	    -name '*.cmo' -o \
	    -name '*.cmx' -o \
	    -name '*.cma' -o \
	    -name '*.cmxa' -o \
	    -name '*.exe' -o \
	    -name '*.o' -o \
	    -name '*.a' -o \
	    -name '*.ml' -o \
	    -name '*.mli' -o \
	    -name '*.annot' -o \
	    -name '*.obj' \
	\) -exec rm {} \;
	$(ECHO)-find src \( \
	    -name '*.annot' \) -exec rm {} \;
	$(ECHO)-find src \( \
	    -name '*.output' \) -exec rm {} \;
	$(ECHO)-find $(DEPENDDIR) \( \
	    -name '*.ml*.depend' \) -exec rm {} \;


FIXDEPEND:=perl -e 'while(<>) { s%[^/\\ :]+[/\\]% %g; \
s%([-a-zA-Z0-9+-.:/\/_]+)%\$$(OBJDIR)/$$1%g; print $$_;}'

DEPINCLUDES= -I $(OBJDIR) $(SOURCEDIRS:%=-I %)

$(DEPENDDIR)/%.mli.depend: %.mli
	@$(NARRATIVE) Generating dependency information for $<
	$(ECHO)mkdir -p $(@D)
	$(AT)$(CAML_DEP) $(DEPINCLUDES) $< | $(FIXDEPEND) > $@

$(DEPENDDIR)/%.ml.depend: %.ml
	@$(NARRATIVE) Generating dependency information for $<
	$(ECHO)mkdir -p $(@D)
	$(AT)$(CAML_DEP) $(DEPINCLUDES) $< | $(FIXDEPEND) > $@

ifeq ($(MAKECMDGOALS),clean)
  CLEANING := 1
endif

ifeq ($(MAKECMDGOALS),distclean)
  CLEANING := 1
endif

ifndef CLEANING
-include $(MODULES:%=$(DEPENDDIR)/%.ml.depend)
-include $(MODULES:%=$(DEPENDDIR)/%.mli.depend)
endif

