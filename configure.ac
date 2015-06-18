dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/CCDoubles],[0.4d0],[marco.maggi-ipsu@poste.it],
  [vicare-ccdoubles],[http://marcomaggi.github.io/vicare.html])
AC_CONFIG_SRCDIR([lib/vicare/math/ccdoubles.vicare.sls])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign subdir-objects dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CC_C_O
AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
LT_PREREQ([2.4])
LT_INIT([shared disable-static])
AC_LANG([C])
AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

dnl --------------------------------------------------------------------

VICARE_SCHEME
VICARE_CHECK_LIBRARY([VICARE],
  [(vicare (or (0 4 2015 6 (>= 18))
               (0 4 2015 (>= 7))
               (0 4 (>= 2016))))])
AS_IF([test "$vicare_cv_schemelib_VICARE" = no],
  [AC_MSG_ERROR([wrong Vicare version],1)])

dnl

VICARE_CHECK_LIBRARY([VICARE_FFI],
  [(vicare ffi (or (0 4 2015 5 (>= 27))
                   (0 4 2015 (>= 6))
                   (0 4 (>= 2016))))])

AS_IF([test "$vicare_cv_schemelib_VICARE_FFI" = no],
  [AC_MSG_ERROR([wrong Vicare FFI library version],1)])

dnl

VICARE_CHECK_LIBRARY([VICARE_PLATFORM_CONSTANTS],
  [(vicare platform constants (or (0 4 2015 6 (>= 18))
                              (0 4 2015 (>= 7))
                              (0 4 (>= 2016))))])

AS_IF([test "$vicare_cv_schemelib_VICARE_PLATFORM_CONSTANTS" = no],
  [AC_MSG_ERROR([wrong (vicare platform constants) library version],1)])

dnl --------------------------------------------------------------------

# Command  line  options  for  the executable  "vicare".   The  variable
# AX_VFLAGS is  for options preselected by  the building infrastructure.
# The variable VFLAGS  is for user options selected on  the command line
# of "configure" and "make", for example:
#
#    $ make VFLAGS="-g --option print-loaded-libraries"
#
AS_VAR_SET(AX_VFLAGS,["-O2 --verbose"])
AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS)])

VICARE_OPTION_DEBUGGING_MODE
dnl VICARE_OPTION_NAUSICAA([check])
dnl AM_CONDITIONAL([WANT_NAUSICAA],[test x$vicare_with_nausicaa = xyes])

AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])
AM_COND_IF([WANT_VICARE_DEBUG],[AS_VAR_SET(VFLAGS,"$VFLAGS -g")])

VICARE_ENABLE_OPTION([INSTALL_SOURCES],[sources-installation],[no],
  [whether to install source Scheme libraries],
  [enable installation of source Scheme libraries])
AM_CONDITIONAL([WANT_INSTALL_SOURCES],
  [test "x$vicare_enable_INSTALL_SOURCES" = xyes])

VICARE_ENABLE_OPTION([TIME_TESTS],[time-tests],[no],
  [whether to enable timing of tests],
  [enable timing of tests])
AM_CONDITIONAL([WANT_TIME_TESTS],[test "x$vicare_enable_TIME_TESTS" = xyes])

AM_COND_IF([WANT_TIME_TESTS],
  [AC_PATH_PROG([TIME_PROGRAM],[time])])

#page
#### CCDoubles configuration

PKG_CHECK_MODULES([CCDOUBLES],[ccdoubles >= 0.1],[ccdoubles=yes],[ccdoubles=no])

dnl CCDOUBLES_CPPFLAGS=$(pkg-config libccdoubles --cflags)
dnl CCDOUBLES_LDFLAGS=$(pkg-config libccdoubles --libs)

AS_IF([test "x$ccdoubles" = "xyes"],,
 [AC_MSG_FAILURE([unable to find required version of CCDOUBLES],[1])])

#page
#### finish

VICARE_OUTPUT
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
