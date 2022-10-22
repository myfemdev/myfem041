
####################################################################
#  BLAS make include file.                                         #
#  March 2007                                                      #
#                                                                  #
#  modificado por rodrigo.valina en 2010-02                        #
####################################################################
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT = _ANY
#  
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
FORTRAN  = gfortran
OPTS     = -O3 ${OPTIONS}
DRVOPTS  = $(OPTS)
NOOPT    =
LOADER   = ${COMPILER}
LOADOPTS =
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#
#  The location and name of the Reference BLAS library.
#
BLASLIB      = blas$(PLAT).a
