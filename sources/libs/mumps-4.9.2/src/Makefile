
#
#  This file is part of MUMPS 4.9.2, built on Thu Nov  5 07:05:08 UTC 2009
#
topdir = ..
libdir = $(topdir)/lib

default:	z

.PHONY: default s d c z mumps_lib clean

s:
	$(MAKE) ARITH=s mumps_lib
d:
	$(MAKE) ARITH=d mumps_lib
c:
	$(MAKE) ARITH=c mumps_lib
z:
	$(MAKE) ARITH=z mumps_lib

include $(topdir)/Makefile.inc

mumps_lib:    $(libdir)/libmumps_common$(PLAT).a \
              $(libdir)/lib$(ARITH)mumps$(PLAT).a 
#              $(libdir)/libmumps_common$(PLAT).so \
#              $(libdir)/lib$(ARITH)mumps$(PLAT).so

OBJS_COMMON = \
        mumps_part9.o\
        mumps_common.o\
        mumps_ooc_common.o\
        mumps_orderings.o\
        mumps_size.o\
        mumps_io.o\
        mumps_io_basic.o\
        mumps_io_thread.o\
        mumps_io_err.o\
        mumps_static_mapping.o \
	tools_common_mod.o

OBJS = 	$(ARITH)mumps_part1.o\
        $(ARITH)mumps_part2.o\
        $(ARITH)mumps_part3.o\
        $(ARITH)mumps_part4.o\
        $(ARITH)mumps_part5.o\
        $(ARITH)mumps_part6.o\
        $(ARITH)mumps_part7.o\
        $(ARITH)mumps_part8.o\
        $(ARITH)mumps_comm_buffer.o\
        $(ARITH)mumps_load.o\
        $(ARITH)mumps_c.o\
        $(ARITH)mumps_ooc_buffer.o\
        $(ARITH)mumps_ooc.o\
        $(ARITH)mumps_struc_def.o


$(libdir)/libmumps_common$(PLAT).a:	$(OBJS_COMMON)
	$(AR) $@ $?
	$(RANLIB) $@

#$(libdir)/libmumps_common$(PLAT).so:	$(OBJS_COMMON)
#	$(FC) -shared $^ -Wl,-soname,libmumps_common$(PLAT)-4.9.2.so -L$(libdir) $(LORDERINGS) -lpthread $(MUMPS_LIBF77) -o $(libdir)/libmumps_common$(PLAT)-4.9.2.so
#	ln -s libmumps_common$(PLAT)-4.9.2.so $@

$(libdir)/lib$(ARITH)mumps$(PLAT).a:    $(OBJS)
	$(AR) $@ $?
	$(RANLIB) $@

#$(libdir)/lib$(ARITH)mumps$(PLAT).so:    $(OBJS)
#	$(FC) -shared $^ -Wl,-soname,lib$(ARITH)mumps$(PLAT)-4.9.2.so -L$(libdir) -lmumps_common$(PLAT) $(LORDERINGS) $(MUMPS_LIBF77) -lblas $(SCALAP) -o $(libdir)/lib$(ARITH)mumps$(PLAT)-4.9.2.so
#	ln -s lib$(ARITH)mumps$(PLAT)-4.9.2.so $@

$(ARITH)mumps_load.o:		$(ARITH)mumps_comm_buffer.o \
				$(ARITH)mumps_struc_def.o

$(ARITH)mumps_ooc.o: 		$(ARITH)mumps_struc_def.o \
				$(ARITH)mumps_ooc_buffer.o \
				mumps_ooc_common.o

$(ARITH)mumps_ooc_buffer.o: 	mumps_ooc_common.o


$(ARITH)mumps_part1.o \
$(ARITH)mumps_part2.o \
$(ARITH)mumps_part3.o \
$(ARITH)mumps_part4.o \
$(ARITH)mumps_part5.o \
$(ARITH)mumps_part6.o \
$(ARITH)mumps_part7.o \
$(ARITH)mumps_part8.o:	$(ARITH)mumps_comm_buffer.o \
			$(ARITH)mumps_load.o \
			$(ARITH)mumps_ooc.o

$(ARITH)mumps_part5.o:	mumps_static_mapping.o
$(ARITH)mumps_part5.o:	$(ARITH)mumps_part2.o

$(ARITH)mumps_part2.o : tools_common_mod.o

.SUFFIXES: .c .F .o
.F.o:
	$(FC) $(OPTF) $(INC) $(IORDERINGSF) $(ORDERINGSF) -I. -I../include -fPIC -c $*.F
.c.o:
	$(CC) $(OPTC) $(INC) -I../include $(CDEFS) $(IORDERINGSC) $(ORDERINGSC) -fPIC -c $*.c

$(ARITH)mumps_c.o:	mumps_c.c
	$(CC) $(OPTC) $(INC) $(CDEFS) -DMUMPS_ARITH=MUMPS_ARITH_$(ARITH) \
	      $(IORDERINGSC) $(ORDERINGSC) -I../include -fPIC -c $? -o $@


clean:
	$(RM) *.o *.mod
