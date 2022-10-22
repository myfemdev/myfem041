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
 
# SOURCE AND COMMONS FOLDERS (separated by spaces)
dir_fuentes = . ../other/blas ../other/basicmod ../other/common ../other/fem \
../other/unv ../other/xml ../other/vtu ../other/conver3d
 
# OBJECT AND .MOD FOLDER
dir_objetos = object
 
# MAIN SOURCE FILE (include relative path from folder where Makefile is)
condir_principal = ./principal.f90
 
# EXECUTABLE NAME 
ejecutable = ../../solvers/direct_current_3d
 
# NEEDED TO convert ejecutable THE DEFAULT RULE: 
$(ejecutable): $(condir_principal) 
 
# MODULES
modulos = comprobaciones.f90 mod_mcambio.f90 modulos_electros3D.f90 \
module_ALLOC_char_r1.f90 module_ALLOC.f90 module_ALLOC_int_alloc_r2.f90 \
module_ALLOC_int_r1.f90 module_ALLOC_int_r2.f90 module_ALLOC_log_r2.f90 \
module_ALLOC_real_alloc_r2.f90 module_ALLOC_real_DOUBLE_r1.f90 \
module_ALLOC_real_DOUBLE_r2.f90 module_ALLOC_real_r1.f90 \
module_ALLOC_real_r2.f90 module_COMPILER_DEPENDANT.f90 module_CONVERS.f90 \
module_FILES.f90 module_REPORT.f90 module_SO_DEPENDANT.f90 defines.f90 mesh.f90 \
subdomains.f90 vector.f90 module_fem_extract_complex.f90 module_fem_extract.f90 \
module_fem_extract_real.f90 module_bsearch.f90 module_cells.f90 \
module_dataset_2411.f90 module_dataset_2412.f90 module_dataset_2467.f90 \
module_dataset.f90 module_FE_DB.f90 module_groups.f90 module_mesh.f90 \
module_readUNV.f90 module_unv.f90 module_xml_parser.f90 LIB_VTK_IO.f90 \
LIB_VTK_IO_READ.f90 module_writeVTU.f90 module_conver3d.f90 \
module_conver3d_source.f
 
# DEPENDENCIAS ENTRE MODULOS
# si pru1 depende de pru2... pru1.o: pru2.o
comprobaciones.o: mesh.o modulos_electros3D.o modulos_electros3D.o \
modulos_electros3D.o modulos_electros3D.o modulos_electros3D.o \
modulos_electros3D.o modulos_electros3D.o modulos_electros3D.o \
modulos_electros3D.o
mod_mcambio.o: modulos_electros3D.o
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
module_dataset_2411.o: module_COMPILER_DEPENDANT.o module_ALLOC.o \
module_dataset.o module_mesh.o
module_dataset_2412.o: module_ALLOC.o module_dataset.o module_mesh.o \
module_FE_DB.o module_cells.o
module_dataset_2467.o: module_dataset.o module_mesh.o module_cells.o \
module_groups.o
module_dataset.o: module_REPORT.o module_CONVERS.o
module_FE_DB.o: module_COMPILER_DEPENDANT.o
module_groups.o: module_ALLOC.o
module_mesh.o: module_COMPILER_DEPENDANT.o module_ALLOC.o module_FILES.o
module_readUNV.o: module_COMPILER_DEPENDANT.o module_SO_DEPENDANT.o \
module_CONVERS.o module_unv.o module_mesh.o
module_unv.o: module_ALLOC.o module_FILES.o module_mesh.o module_dataset_2411.o \
module_dataset_2412.o module_dataset_2467.o module_bsearch.o
module_xml_parser.o: module_ALLOC.o module_CONVERS.o module_REPORT.o \
module_FILES.o
LIB_VTK_IO_READ.o: LIB_VTK_IO.o
module_writeVTU.o: LIB_VTK_IO.o
module_conver3d.o: module_COMPILER_DEPENDANT.o module_conver3d_source.o
 
# INCLUDES
includes = 
 
# COMPILER
FC = gfortran
 
# COMPILER OPTIONS
FFLAGS = -J$(dir_objetos) -ffree-line-length-0 -ffixed-line-length-0
 
# LINKER OPTIONS
LDFLAGS = 
 
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
