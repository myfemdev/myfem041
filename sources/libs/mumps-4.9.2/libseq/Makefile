#
#  This file is part of MUMPS 4.9.2, built on Thu Nov  5 07:05:08 UTC 2009
#
all: libmpiseq

.PHONY: all libmpiseq clean

include ../Makefile.inc

libmpiseq: libmpiseq$(PLAT).a 
#libmpiseq$(PLAT).so

libmpiseq$(PLAT).a: mpi.o mpic.o elapse.o
	$(AR) $@ mpi.o mpic.o elapse.o
	$(RANLIB) $@
#libmpiseq$(PLAT).so: mpi.o mpic.o elapse.o
#	$(FC) -shared $^ -Wl,-soname,libmpiseq$(PLAT)-4.9.2.so -o libmpiseq$(PLAT)-4.9.2.so
#	ln -s libmpiseq$(PLAT)-4.9.2.so $@
.f.o:
	$(FC) $(OPTF) -fPIC -c $*.f
.c.o:
	$(CC) $(OPTC) $(CDEFS) -I. -fPIC -c $*.c

clean:
	$(RM) *.o *.a *.so
