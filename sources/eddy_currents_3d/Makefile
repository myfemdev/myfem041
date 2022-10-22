#################################################################
# Makefile created using the tool 'Creamake'
# 
# Creamake is distributed under the GNU GPL license
# Author: Francisco Pena, fran.pena@usc.es
# Download page: http://sourceforge.net/projects/creamake/
#################################################################
 
#################################################################
# User-modifiable options
#################################################################

MPI_INCLUDE = ../libs/mumps-4.9.2/libseq
MUMPS_INCLUDE = ../libs/mumps-4.9.2/include
MPI_LIB = ../libs/mumps-4.9.2/libseq
MUMPS_LIB = ../libs/mumps-4.9.2/lib
PORD_LIB = ../libs/mumps-4.9.2/PORD/lib
XTRALIBS = -lzmumps -lmumps_common -lpthread -lpord -lmpiseq

# SOURCE AND COMMONS FOLDERS (separated by spaces)
dir_fuentes = . ../other/basicmod ../other/common ../other/fem ../other/xml \
../other/vtu ../other/blas
 
# OBJECT AND .MOD FOLDER
dir_objetos = object
 
# MAIN SOURCE FILE (include relative path from folder where Makefile is)
condir_principal = ./ppal_mixto_mumps.f90
 
# EXECUTABLE NAME 
ejecutable = ../../solvers/eddy_currents_3d
 
# NEEDED TO convert ejecutable THE DEFAULT RULE: 
$(ejecutable): $(condir_principal) 
 
# MODULES
modulos = comprobaciones.f90 condicion_intensidad_general.f90 \
dielectrico_caras.f90 ensamblar.f90 globales.f90 inversa.f90 leedata.f90 \
matriz_Crouzeix_Raviart.f90 matriz_Lagrange.f90 matriz_Nedelec.f90 mlocalt.f90 \
module_MATH.f90 module_SET.f90 morse_CRD.f90 morse_id_row.f90 morse_ND.f90 \
morse_NN.f90 norma_axb.f90 postproceso.f90 readxml.f90 signo.f90 solve_mumps.f90 \
sparse_aux.f90 sparse_class.f90 sparse_complex.f90 sparse_real.f90 times.f90 \
module_ALLOC_char_r1.f90 module_ALLOC.f90 module_ALLOC_int_alloc_r2.f90 \
module_ALLOC_int_r1.f90 module_ALLOC_int_r2.f90 module_ALLOC_log_r2.f90 \
module_ALLOC_real_alloc_r2.f90 module_ALLOC_real_DOUBLE_r1.f90 \
module_ALLOC_real_DOUBLE_r2.f90 module_ALLOC_real_r1.f90 \
module_ALLOC_real_r2.f90 module_COMPILER_DEPENDANT.f90 module_CONVERS.f90 \
module_FILES.f90 module_REPORT.f90 module_SO_DEPENDANT.f90 defines.f90 mesh.f90 \
subdomains.f90 vector.f90 module_fem_extract_complex.f90 module_fem_extract.f90 \
module_fem_extract_real.f90 module_xml_parser.f90 LIB_VTK_IO.f90 \
LIB_VTK_IO_READ.f90 module_writeVTU.f90
 
# DEPENDENCIAS ENTRE MODULOS
# si pru1 depende de pru2... pru1.o: pru2.o
comprobaciones.o: globales.o
condicion_intensidad_general.o: globales.o module_MATH.o morse_id_row.o
dielectrico_caras.o: globales.o module_MATH.o module_SET.o
ensamblar.o: sparse_class.o globales.o
globales.o: mesh.o sparse_class.o
inversa.o: defines.o
leedata.o: globales.o vector.o
matriz_Crouzeix_Raviart.o: globales.o sparse_class.o module_MATH.o morse_CRD.o
matriz_Lagrange.o: globales.o sparse_class.o module_MATH.o morse_ND.o
matriz_Nedelec.o: globales.o sparse_class.o signo.o mlocalt.o morse_NN.o
mlocalt.o: globales.o module_MATH.o
module_MATH.o: defines.o vector.o inversa.o
morse_CRD.o: sparse_class.o
morse_id_row.o: defines.o sparse_class.o
morse_ND.o: sparse_class.o
morse_NN.o: sparse_class.o
norma_axb.o: sparse_class.o
postproceso.o: globales.o sparse_class.o module_MATH.o module_writeVTU.o \
subdomains.o module_fem_extract.o
readxml.o: module_SO_DEPENDANT.o module_REPORT.o module_xml_parser.o globales.o
signo.o: globales.o
solve_mumps.o: sparse_class.o
sparse_class.o: sparse_real.o sparse_complex.o
sparse_complex.o: sparse_aux.o vector.o
sparse_real.o: sparse_aux.o vector.o
module_ALLOC_char_r1.o: module_REPORT.o
module_ALLOC.o: module_ALLOC_int_r1.o module_ALLOC_int_r2.o \
module_ALLOC_int_alloc_r2.o module_ALLOC_real_r1.o module_ALLOC_real_r2.o \
module_ALLOC_real_alloc_r2.o module_ALLOC_real_DOUBLE_r1.o \
module_ALLOC_real_DOUBLE_r2.o module_ALLOC_char_r1.o module_ALLOC_log_r2.o
module_ALLOC_int_alloc_r2.o: module_ALLOC_int_r1.o
module_ALLOC_int_r1.o: module_REPORT.o
module_ALLOC_int_r2.o: module_REPORT.o
module_ALLOC_log_r2.o: module_REPORT.o
module_ALLOC_real_alloc_r2.o: module_ALLOC_real_r1.o
module_ALLOC_real_DOUBLE_r1.o: module_COMPILER_DEPENDANT.o module_REPORT.o
module_ALLOC_real_DOUBLE_r2.o: module_COMPILER_DEPENDANT.o module_REPORT.o
module_ALLOC_real_r1.o: module_REPORT.o
module_ALLOC_real_r2.o: module_REPORT.o
module_CONVERS.o: module_COMPILER_DEPENDANT.o module_REPORT.o
module_FILES.o: module_REPORT.o module_CONVERS.o
module_REPORT.o: module_COMPILER_DEPENDANT.o module_SO_DEPENDANT.o
module_SO_DEPENDANT.o: module_COMPILER_DEPENDANT.o
defines.o: module_COMPILER_DEPENDANT.o
mesh.o: defines.o
subdomains.o: defines.o
vector.o: defines.o
module_fem_extract.o: module_fem_extract_real.o module_fem_extract_complex.o
module_xml_parser.o: module_ALLOC.o module_CONVERS.o module_REPORT.o \
module_FILES.o
LIB_VTK_IO_READ.o: LIB_VTK_IO.o
module_writeVTU.o: LIB_VTK_IO.o
 
# INCLUDES
includes = 
 
# COMPILER
FC = gfortran
 
# COMPILER OPTIONS
FFLAGS = -J$(dir_objetos) -I$(MPI_INCLUDE) -I$(MUMPS_INCLUDE) -ffree-line-length-0 -ffixed-line-length-0
 
# LINKER OPTIONS
LDFLAGS = -L$(MPI_LIB) -L$(MUMPS_LIB) -L$(PORD_LIB) $(XTRALIBS)
 
#################################################################
# Non-modifiable part
#################################################################
 
# CARPETAS DE BUSQUEDA
VPATH =   $(subst ,:,$(strip $(dir_fuentes)))
vpath %.o $(dir_objetos)
 
# FUENTES
fuentes_       = $(filter %.f %.F %.for %.FOR %.f90 %.F90 %.f95 %.F95 %.f03 %.F03,$(shell ls $(dir_fuentes)))
fuentes        = $(filter-out $(notdir $(condir_principal)) $(modulos),$(fuentes_))
 
# OBJETOS
modulos_obj    = $(addsuffix .o,$(basename $(modulos)))
fuentes_obj    = $(addsuffix .o,$(basename $(fuentes)))
 
# OBJETOS EN SU CARPETA
condir_modulos_obj    = $(addprefix $(dir_objetos)/,$(modulos_obj))
condir_fuentes_obj    = $(addprefix $(dir_objetos)/,$(fuentes_obj))
 
# OPCIONES DE COMPILACION
FFLAGS += $(patsubst %,-I%,$(dir_fuentes))
FFLAGS += -I$(dir_objetos)
 
# REGLA PRINCIPAL
all: $(ejecutable)
 
$(ejecutable): $(includes) $(modulos_obj) $(fuentes_obj)
	$(FC) -o $(ejecutable) $(FFLAGS) $(condir_principal) $(condir_modulos_obj) $(condir_fuentes_obj) $(LDFLAGS)
 
# REGLA PARA LAS FUENTES
$(fuentes_obj): $(includes) $(modulos_obj)
 
# PATRONES DE REGLAS
%.o:%.f
	$(FC) -c -o $@ $(FFLAGS) $<
	@mv $@ $(dir_objetos) 
%.o:%.F
	$(FC) -c -o $@ $(FFLAGS) $< 
	@mv $@ $(dir_objetos) 
%.o:%.for
	$(FC) -c -o $@ $(FFLAGS) $< 
	@mv $@ $(dir_objetos) 
%.o:%.FOR
	$(FC) -c -o $@ $(FFLAGS) $< 
	@mv $@ $(dir_objetos) 
%.o:%.f90
	$(FC) -c -o $@ $(FFLAGS) $< 
	@mv $@ $(dir_objetos) 
%.o:%.F90
	$(FC) -c -o $@ $(FFLAGS) $< 
	@mv $@ $(dir_objetos) 
 
.PHONY: clean
clean:
	rm $(dir_objetos)/*.o      
	rm $(dir_objetos)/*.mod    
	rm $(ejecutable)
