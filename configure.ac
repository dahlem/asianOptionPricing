# Copyright (C) 2008 Dominik Dahlem <Dominik.Dahlem@gmail.com>
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT([asianOptionPricing],
        [1.0],
        [Dominik Dahlem Dominik.Dahlem@gmail.com])

AC_CONFIG_AUX_DIR(config)

# extra autoconf macros
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign])

# Check for MPI
AC_ARG_ENABLE(mpi, AS_HELP_STRING(
		   [--enable-mpi], 
		   [enable MPI @<:@default=no@:>@]), 
		   [USE_MPI=$enableval], [USE_MPI=no])
if test "x$USE_MPI" = xyes; then
   HAVE_MPI=TRUE
   SNOW=", snow"
   AC_SUBST(HAVE_MPI)
   AC_SUBST(SNOW)
else
   HAVE_MPI=FALSE
   SNOW=""
   AC_SUBST(HAVE_MPI)
   AC_SUBST(SNOW)
fi


# enable report generation
AC_ARG_ENABLE(report, AS_HELP_STRING(
                     [--enable-report],
                     [enable report generation @<:@default=no@:>@]), 
                     [REPORT=$enableval], [REPORT=no])

# Check for latex stuff
if test "x$REPORT" = xyes; then
  AC_PROG_LATEX
  AC_PROG_DVIPS
  AC_PROG_PS2PDF
fi
AM_CONDITIONAL(GEN_REPORT, test "x$REPORT" = xyes)


AC_CONFIG_FILES([
   Makefile
   doc/Makefile
   src/R/asian_pricing.R
   src/package/asianOptionPricing/DESCRIPTION
])

AC_OUTPUT


for top_builddir in . .. ../.. $ac_auxdir $ac_auxdir/..; do
  test -f $top_builddir/configure && break
done

AC_MSG_RESULT([

$PACKAGE_NAME $VERSION: Automatic configuration OK.

Type 'make' to compile this project.

])
