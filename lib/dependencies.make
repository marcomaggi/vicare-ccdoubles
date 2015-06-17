## dependencies.make --
#
# Automatically built.

lib/vicare/math/ccdoubles.fasl: \
		lib/vicare/math/ccdoubles.vicare.sls \
		lib/vicare/math/ccdoubles/functions.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_math_ccdoubles_fasldir = $(bundledlibsdir)/vicare/math
lib_vicare_math_ccdoubles_vicare_slsdir  = $(bundledlibsdir)/vicare/math
nodist_lib_vicare_math_ccdoubles_fasl_DATA = lib/vicare/math/ccdoubles.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_math_ccdoubles_vicare_sls_DATA = lib/vicare/math/ccdoubles.vicare.sls
endif
EXTRA_DIST += lib/vicare/math/ccdoubles.vicare.sls
CLEANFILES += lib/vicare/math/ccdoubles.fasl

lib/vicare/math/ccdoubles/functions.fasl: \
		lib/vicare/math/ccdoubles/functions.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_math_ccdoubles_functions_fasldir = $(bundledlibsdir)/vicare/math/ccdoubles
lib_vicare_math_ccdoubles_functions_vicare_slsdir  = $(bundledlibsdir)/vicare/math/ccdoubles
nodist_lib_vicare_math_ccdoubles_functions_fasl_DATA = lib/vicare/math/ccdoubles/functions.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_math_ccdoubles_functions_vicare_sls_DATA = lib/vicare/math/ccdoubles/functions.vicare.sls
endif
EXTRA_DIST += lib/vicare/math/ccdoubles/functions.vicare.sls
CLEANFILES += lib/vicare/math/ccdoubles/functions.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
