# To compile directly, uncomment the line below.
# include ../Make.in
#
# Otherwise, adequate variables for CC, CFLAGS, AR and
# RANLIB must be passed to make.
#

INCLUDES = -I../include

COPTIONS = $(INCLUDES) $(CFLAGS) $(OPTFLAGS) -fPIC

OBJS = graph.o gbipart.o gbisect.o ddcreate.o ddbisect.o nestdiss.o \
       multisector.o gelim.o bucket.o tree.o \
       symbfac.o interface.o sort.o minpriority.o 

# Note: numfac.c read.c mapping.c triangular.c matrix.c kernel.c
# were not direcly used by MUMPS and have been removed from the
# original SPACE package.

.c.o:
	$(CC) $(COPTIONS) -c $*.c

libpord$(PLAT).a:	$(OBJS)
	$(AR) $(ARFUNCT) $@ $(OBJS)
	$(RANLIB) $@

libpord$(PLAT).so: $(OBJS)
	$(CC) -shared $(OBJS) -Wl,-soname,libpord$(PLAT)-4.9.2.so -o libpord$(PLAT)-4.9.2.so
	ln -s libpord$(PLAT)-4.9.2.so $@

clean:
	$(RM) *.o

realclean:
	$(RM) *.o libpord*.a *.so
