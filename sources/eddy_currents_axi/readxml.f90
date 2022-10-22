subroutine readxml()
!-----------------------------------------------------------------------
! procedure for reading the solver variables
!-----------------------------------------------------------------------
use module_SO_DEPENDANT
use module_REPORT
use module_xml_parser
!Solver modules
      use fich_electros
      use electros_2D
      use sourcevolumic
      use neumann
      use dirichlet
      use derivados
      use voltage_drop
      use intensity_input



implicit none

character(len = MAXPATH) :: infile
integer :: idme, idma
integer :: i, j, k, l, pos, n1, n2
character(len=1000), dimension(:), allocatable :: list, list1, list2, list3, refs
character(len=1000) :: sval,sval1,sval2
character(len=MAXPATH) :: materialsfile
real(DOUBLE) :: real1
real(DOUBLE), dimension(:), allocatable :: aux1, aux2, aux3
character(len=100) :: rmp, hb , prefix

! para xml parser
call set_SO()
call set_report_level(REPORT_STDOUT)

prefix = '/Materials database/Open/Materials/'
rmp = 'Relative magnetic permeability'
hb = 'H-B Table'

!infile = 'local.dat.xml'

!idme = fopen(infile)
 idme = fopen()

 print * , 'idme', idme

! fichero de malla
 call fread(idme, '/Mesh/Open/Mesh file', fichma)

! ficheros de salida
 fichsolr = 'out_solution_r'
 fichsoli = 'out_solution_i'
 fichsolm = 'out_solution_m'
 fichdenscr = 'out_density_r'
 fichdensci = 'out_density_i'
 fichdenscm = 'out_density_m'
 fichcampmr = 'out_magfield_r'
 fichcampmi = 'out_magfield_i'
 fichcampmm = 'out_magfield_m'
 fichindumr = 'out_induc_r'
 fichindumi = 'out_induc_i'
 fichindumm = 'out_induc_m'
 fichjoule = 'out_joule'

 fichintens = 'intensity_current'
 fichpot = 'power_active'
 fichvolt = 'solid_conductor'
 fichflo = 'out_lorentz'

 dirichlet_bc%numero = 0
 neumann_bc%numero = 0
 sourcevol%numero = 0
 permagrel%numero = 0
 conduc%numero = 0
 num_inputsv = 0
 num_inputsi = 0
 iopteta = 0
 iop = 1
 iopf = 1


! condicions Dirichlet

call flist(idme, '/Boundary conditions/Dirichlet/Condition', list)
do i = 1, size(list,1) !loop for defined Dirichlet BC's
  call flist(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
      &'/A function/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = dirichlet_bc%numero + 1
        dirichlet_bc%referencias(pos:pos+size(refs,1)-1) = int(refs)
        dirichlet_bc%numero = dirichlet_bc%numero + size(refs,1)
	dirichlet_bc%modo(pos:pos+size(refs,1)-1) = 1
        call fread(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
        &'/A function/Function name', sval)
	dirichlet_bc%etiqueta(i) = trim(sval)
      else
        print * , 'Function Dirichlet B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
      &'/A constant/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = dirichlet_bc%numero + 1
        dirichlet_bc%modo(pos:pos+size(refs,1)-1) = 2
	dirichlet_bc%referencias(pos:pos+size(refs,1)-1) = int(refs)
        dirichlet_bc%numero = dirichlet_bc%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        dirichlet_bc%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant Dirichlet B.C. with 0 references: skipping'
      endif
    case default; call error('readxml: in Dirichlet: case not implemented.')
    end select
  enddo
enddo


! condicions Neumann

call flist(idme, '/Boundary conditions/Neumann/Condition', list)
do i = 1, size(list,1) !loop for defined Neumann BC's
  call flist(idme, '/Boundary conditions/Neumann/Condition/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Boundary conditions/Neumann/Condition/'//trim(list(i))//&
      &'/A function/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = neumann_bc%numero + 1
        neumann_bc%referencias(pos:pos+size(refs,1)-1) = int(refs)
	neumann_bc%modo(pos:pos+size(refs,1)-1) = 1
        neumann_bc%numero = neumann_bc%numero + size(refs,1)

        call fread(idme, '/Boundary conditions/Neumann/Condition/'//trim(list(i))//&
        &'/A function/Function name', sval)
	neumann_bc%etiqueta(i) = trim(sval)
      else
        print * , 'Function Neumann B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Boundary conditions/Neumann/Condition/'//trim(list(i))//&
      &'/A constant/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = neumann_bc%numero + 1
        neumann_bc%referencias(pos:pos+size(refs,1)-1) = int(refs)
	neumann_bc%modo(pos:pos+size(refs,1)-1) = 2
        neumann_bc%numero = neumann_bc%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Boundary conditions/Neumann/Condition/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        neumann_bc%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant Neumann B.C. with 0 references: skipping'
      endif
    case default; call error('readxml: in Neumann: case not implemented.')      
    end select
  enddo
enddo    


! current sources

! Volumetric sources: current density

call flist(idme, '/Volumetric sources/Stranded/Current density/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i))//&
      &'/A function/Domain references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 1
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 1
        sourcevol%numero = sourcevol%numero + size(refs,1)
        call fread(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i))//&
        &'/A function/Function name', sval)
	sourcevol%etiqueta = trim(sval)
      else
        print * , 'Function current source with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i))//&
      &'/A constant/Domain references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 2
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 1
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i))//&
        &'/A constant/RMS value', real1)
        sourcevol%vrms(pos:pos+size(refs,1)-1) = real1
        call fread(idme, '/Volumetric sources/Stranded/Current density/Sources/'//trim(list(i))//&
        &'/A constant/Phase angle', real1)
        sourcevol%vphase(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
    case default; call error('readxml: in volumetric current density: case not implemented.')
    end select
  enddo
enddo   

! Volumetric sources: current intensity 

call flist(idme, '/Volumetric sources/Stranded/Current intensity/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
!  call flist(idme, '/Volumetric sources/Stranded/Current intensity/Sources/'//trim(list(i)), list2)
!  do j = 1, size(list2,1) !loop for data type for each BC 
!    select case(trim(list2(j)))
!    case('A function')
!      !References
!      call fread_alloc(idme, '/Volumetric sources/Stranded/Current intensity/Sources/'//trim(list(i))//&
!      &'/A function/Domain references', refs, realloc=.true.)
!      if (size(refs,1)>0) then
!        pos = sourcevol%numero + 1
!        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
!        sourcevol%modo(pos:pos+size(refs,1)-1) = 1
!        sourcevol%itipo(pos:pos+size(refs,1)-1) = 2
!        sourcevol%numero = sourcevol%numero + size(refs,1)
!      else
!        print * , 'Function current source with 0 references: skipping'
!      endif
!    case('A constant')
      !References
      call fread_alloc(idme, '/Volumetric sources/Stranded/Current intensity/Sources/'//trim(list(i))//&
      &'/Domain references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 2
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 2
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Volumetric sources/Stranded/Current intensity/Sources/'//trim(list(i))//&
        &'/RMS value', real1)
        sourcevol%vrms(pos:pos+size(refs,1)-1) = real1
        call fread(idme, '/Volumetric sources/Stranded/Current intensity/Sources/'//trim(list(i))//&
        &'/Phase angle', real1)
        sourcevol%vphase(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
!    case default; call error('readxml: in volumetric current intensity: case not implemented.')
!    end select
!  enddo
enddo 


! Surface sources: voltage drop

call flist(idme, '/Volumetric sources/Solid/Potential drop/Sources', list)
allocate(inputsv(size(list,1)))
do i = 1, size(list,1) !loop for defined current sources
      !References
      call fread_alloc(idme, '/Volumetric sources/Solid/Potential drop/Sources/'//trim(list(i))//&
      &'/Domain references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        inputsv(i)%nsubdo = size(refs,1)
        inputsv(i)%subdo_references(1:inputsv(i)%nsubdo) = int(refs)
        num_inputsv = num_inputsv + 1
        !Constant value
        call fread(idme, '/Volumetric sources/Solid/Potential drop/Sources/'//trim(list(i))//&
        &'/RMS value', real1)
        inputsv(i)%vrms = real1
        call fread(idme, '/Volumetric sources/Solid/Potential drop/Sources/'//trim(list(i))//&
        &'/Phase angle', real1)
        inputsv(i)%vphase = real1
	inputsv(i)%etiqueta = trim(list(i))
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
enddo  


call flist(idme, '/Volumetric sources/Solid/Current intensity/Sources', list)
allocate(inputsi(size(list,1)))
do i = 1, size(list,1) !loop for defined current sources
      !References
      call fread_alloc(idme, '/Volumetric sources/Solid/Current intensity/Sources/'//trim(list(i))//&
      &'/Domain references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = num_inputsi + 1
        inputsi(i)%nsubdo = size(refs,1)
        inputsi(i)%subdo_references(1:inputsi(i)%nsubdo) = int(refs)
        num_inputsi = num_inputsi + 1
        !Constant value
        call fread(idme, '/Volumetric sources/Solid/Current intensity/Sources/'//trim(list(i))//&
        &'/RMS value', real1)
        inputsi(i)%vrms = real1
        call fread(idme, '/Volumetric sources/Solid/Current intensity/Sources/'//trim(list(i))//&
        &'/Phase angle', real1)
        inputsi(i)%vphase = real1
	inputsi(i)%etiqueta = trim(list(i))
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
enddo  


call flist(idme, '/Properties/Materials/Materials', list)
permagrel%numero = size(list,1)

call fread(idme, '/Materials file/Open/materialsDB', materialsfile)
idma = fopen(materialsfile)


do i = 1, size(list,1)

  permagrel%referencias(i) = int(list(i))

  call fread(idme, '/Properties/Materials/Materials/'//trim(list(i)), sval)
  call flist(idma, trim(prefix)//trim(sval)//'/Magnetic properties', list1)

  select case(trim(list1(1)))

    case('Relative magnetic permeability')
      call flist(idma, trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(rmp),list2)
      if (size(list2, 1)==0) call error('readxml: missing '//trim(rmp)// ' type for material')

      select case(trim(list2(1)))

        case('A constant')
          permagrel%iopermagr(i) = 2 !Definido por constante
          call flist(idma, trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(rmp)//'/A constant', list3)  
          if (size(list3,1)==0) call error('readxml: missing constant '//trim(rmp)//' type for material')

          select case(trim(list3(1)))

            case('Isotropic')
              call fread(idma, trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(rmp)//'/A constant/Isotropic',&
              &real1)
              permagrel%valor(i) = real1
            case default
              call error('readxml: ' //trim(list3(1))// ' : case not implemented.')
          end select
        case default
          call error('readxml: ' //trim(list2(1))// ' : case not implemented.')
      end select

    case default
      call error('readxml: ' //trim(list2(1))// ' : case not implemented.')
  end select

  conduc%numero = size(list,1)

  conduc%referencias(i) = int(list(i))
  call flist(idma,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity', list2)
  if (size(list2,1)==0) call error('readxml: missing Electrical conductivity type for material')
  select case(trim(list2(1)))
      case('A constant')
         call flist(idma,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant', list3)  
         if (size(list3,1)==0) call error('readxml: missing constant Electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(idma, '/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                 conduc%valor(i)) 
             case default; call error('readxml: '//trim(list3(1))//' Case not implemented.')
         end select
         conduc%iopcond(i) = 2  

  case default; call error('readxml: '//trim(list2(1))//' Case not implemented.')
  end select
enddo
 call fread(idme, '/Data/Frequency/Value', frec)

do i=1, dirichlet_bc%numero
   print*, 'dirichlet modo:',dirichlet_bc%modo(i), 'refs:',dirichlet_bc%referencias(i),'valor:',dirichlet_bc%valor(i)
enddo
do i=1, sourcevol%numero
   print*, 'sourcevol modo:',sourcevol%modo(i),'tipo:',sourcevol%itipo(i), 'refs:',&
   & sourcevol%referencias(i),'valor:',sourcevol%vrms(i), sourcevol%vphase(i)
enddo
do i=1, num_inputsi
   print*, 'numinputsi num refs:',inputsi(i)%subdo_references(:inputsi(i)%nsubdo),'valor:',inputsi(i)%vrms,inputsi(i)%vphase, 'tot:',size(inputsi)
enddo
do i=1, num_inputsv
   print*, 'numinputsv num refs:',inputsv(i)%subdo_references(:inputsv(i)%nsubdo),'valor:',inputsv(i)%vrms,inputsv(i)%vphase, 'tot:',size(inputsv)
enddo
do i=1, conduc%numero
   print*, 'conduc refs:',conduc%referencias(i) ,'valor:',conduc%valor(i)
enddo
do i=1, permagrel%numero
   print*, 'permagrel refs:',permagrel%referencias(i) ,'valor:',permagrel%valor(i)
enddo


call fclose(idme)
call fclose(idma)


if (allocated(list)) call dealloc(list)
if (allocated(list1)) call dealloc(list1)
if (allocated(list2)) call dealloc(list2)
if (allocated(list3)) call dealloc(list3)
if (allocated(refs)) call dealloc(refs)
if (allocated(aux1)) call dealloc(aux1)
if (allocated(aux2)) call dealloc(aux2)
if (allocated(aux3)) call dealloc(aux3)

end subroutine


