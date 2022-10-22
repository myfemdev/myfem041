

#
#  This file is part of MUMPS 4.9.2, built on Thu Nov  5 07:05:08 UTC 2009
#
topdir = .
libdir = $(topdir)/lib

default:	z

.PHONY: default alllib all s d c z \
	sexamples dexamples cexamples zexamples \
	mumps_lib requiredobj libseqneeded clean

alllib:		s d c z
all:		sexamples dexamples cexamples zexamples

s:
	$(MAKE) ARITH=s mumps_lib
d:
	$(MAKE) ARITH=d mumps_lib
c:
	$(MAKE) ARITH=c mumps_lib
z:
	$(MAKE) ARITH=z mumps_lib


# Is Makefile.inc available ?
Makefile.inc:
	@echo "######################################################################"
	@echo "# BEFORE COMPILING MUMPS, YOU SHOULD HAVE AN APPROPRIATE FILE"
	@echo "# Makefile.inc AVALAIBLE. PLEASE LOOK IN THE DIRECTORY ./Make.inc FOR"
	@echo "# EXAMPLES OF Makefile.inc FILES, AT Make.inc/Makefile.inc.generic"
	@echo "# IN CASE YOU NEED TO BUILD A NEW ONE AND READ THE MAIN README FILE"
	@echo "######################################################################"
	@exit 1

include Makefile.inc

mumps_lib: requiredobj
	$(MAKE) $(ARITH) -C src

sexamples:	s
	$(MAKE) s -C examples

dexamples:	d
	$(MAKE) d -C examples

cexamples:	c
	$(MAKE) c -C examples

zexamples:	z
	$(MAKE) z -C examples


requiredobj: Makefile.inc $(LIBSEQNEEDED) $(libdir)/libpord$(PLAT).a 
#$(libdir)/libpord$(PLAT).so

# dummy MPI library (sequential version)

libseqneeded:
	$(MAKE) -C libseq


# Build the libpord.a library and copy it into $(topdir)/lib
$(libdir)/libpord$(PLAT).a:
	$(MAKE) CC="$(CC)" CFLAGS="$(OPTC)" AR="$(AR)" ARFUNCT= RANLIB="$(RANLIB)" -C $(LPORDDIR)

#$(libdir)/libpord$(PLAT).so:
#	make CC="$(CC)" CFLAGS="$(OPTC)" AR="$(AR)" ARFUNCT= RANLIB="$(RANLIB)" libpord$(PLAT).so; fi -C $(LPORDDIR)
	
clean:
	$(MAKE) clean -C src
	(cd $(libdir); $(RM) *$(PLAT).a)
	(cd $(libdir); $(RM) *$(PLAT).so)
	$(MAKE) clean -C libseq
	$(MAKE) realclean -C $(LPORDDIR)

