#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT(c2s, 0.7, michael.emmi@gmail.com)

# Checks for programs.

AC_PROG_CC
AC_PROG_CXX


AC_PROG_AWK
AC_PROG_GREP
AC_PROG_SED

AC_PATH_PROGS(OCAMLC, [ocamlc])
AC_PATH_PROGS(OCAMLCDOTOPT, [ocamlc.opt])
AC_PATH_PROGS(OCAMLOPT, [ocamlopt])
AC_PATH_PROGS(OCAMLDEP, [ocamldep])
AC_PATH_PROGS(OCAMLLEX, [ocamllex])
AC_PATH_PROGS(OCAMLYACC, [ocamlyacc])
AC_PATH_PROGS(OCAMLDOC, [ocamldoc])
AC_PATH_PROGS(OCAMLFIND, [ocamlfind])


if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_PROG_INSTALL
# AC_PROG_MKDIR_P

# Checks for libraries.

# AC_CHECK_OCAML_PKG(unix)
# AC_CHECK_OCAML_PKG(str)
# AC_CHECK_OCAML_PKG(nums)

# FIXME: Replace `main' with a function in `-linkpkg':
AC_CHECK_LIB([inkpkg], [main])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
