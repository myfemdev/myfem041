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
use sourcesurface
use neumann
use dirichlet
use derivados
use nolineal
use tiempo
use pwm
use sine
use cosine




implicit none

character(len = MAXPATH) :: infile
integer :: idme, idma, ios, numsigs
integer :: i, j, k, l, pos, couplepos, n1, n2
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
fichsol = 'out_solution_J'
fichdensc = 'out_density_J'
fichcampm = 'out_magfield_J'
fichindum = 'out_induc_J'
fichint = 'out_int_J'


dirichlet_bc%numero = 0
neumann_bc%numero = 0
sourcevol%numero = 0
sourcesur%numero = 0
potencial_dat_vol%numero = 0
potencial_dat_sur%numero = 0

!Leemos el fichero de materiales al principio: hace falta para potential drop
call fread(idme, '/Materials file/Open/materialsDB', materialsfile)
idma = fopen(materialsfile)


! condicions Dirichlet
pos = 0
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
	dirichlet_bc%modo(pos:pos+size(refs,1)-1) = 1
        dirichlet_bc%numero = dirichlet_bc%numero + size(refs,1)
        !Function name
        call fread(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
        &'/A function/Function name', sval)
        dirichlet_bc%etiqueta(pos:pos+size(refs,1)-1) = trim(sval)
      else
        print * , 'Function Dirichlet B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Boundary conditions/Dirichlet/Condition/'//trim(list(i))//&
      &'/A constant/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = dirichlet_bc%numero + 1
        dirichlet_bc%referencias(pos:pos+size(refs,1)-1) = int(refs)
	dirichlet_bc%modo(pos:pos+size(refs,1)-1) = 2
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
 pos = 0
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
        neumann_bc%etiqueta(pos:pos+size(refs,1)-1) = trim(sval)
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
pos = 0
call flist(idme, '/Sources/Volumetric/Current density/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Volumetric/Current density/Sources/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Sources/Volumetric/Current density/Sources/'//trim(list(i))//&
      &'/A function/Volumetric references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 1
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 1
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Function name
        call fread(idme, '/Sources/Volumetric/Current density/Sources/'//trim(list(i))//&
        &'/A function/Function name', sval)
        sourcevol%etiqueta(pos:pos+size(refs,1)-1) = trim(sval)
      else
        print * , 'Function current source with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Sources/Volumetric/Current density/Sources/'//trim(list(i))//&
      &'/A constant/Volumetric references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 2
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 1
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Sources/Volumetric/Current density/Sources/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        sourcevol%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
    case default; call error('readxml: in volumetric current density: case not implemented.')
    end select
  enddo
enddo   

! Volumetric sources: current intensity 
pos = 0
call flist(idme, '/Sources/Volumetric/Current intensity/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Volumetric/Current intensity/Sources/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References

      call fread_alloc(idme, '/Sources/Volumetric/Current intensity/Sources/'//trim(list(i))//&
      &'/A function/Volumetric references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 1
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 2
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Function name
        call fread(idme, '/Sources/Volumetric/Current intensity/Sources/'//trim(list(i))//&
        &'/A function/Function name', sval)
        sourcevol%etiqueta(pos:pos+size(refs,1)-1) = trim(sval)
      else
        print * , 'Function current source with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Sources/Volumetric/Current intensity/Sources/'//trim(list(i))//&
      &'/A constant/Volumetric references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcevol%numero + 1
        sourcevol%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcevol%modo(pos:pos+size(refs,1)-1) = 2
        sourcevol%itipo(pos:pos+size(refs,1)-1) = 2
        sourcevol%numero = sourcevol%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Sources/Volumetric/Current intensity/Sources/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        sourcevol%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
    case default; call error('readxml: in volumetric current intensity: case not implemented.')
    end select
  enddo
enddo 
! Volumetric sources: potential drop

pos = 0
couplepos = 0
call flist(idme, '/Sources/Volumetric/Potential drop/Sources', list)
potencial_dat_vol%numero = 0
potencial_dat_vol%ncouples = 0
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))&
  &, list1)
!Single conductor
    select case(trim(list1(1)))
      case('Single')
!!!References
        call fread_alloc(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Conductor/Domain references', refs,realloc=.true.)
        if (size(refs,1)>0) then
          pos = potencial_dat_vol%numero + 1
          potencial_dat_vol%referencias(pos:pos+size(refs,1)-1) = int(refs)
!!!Electrical conductivity
          call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
          &'/Single/Conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_vol%modo1(pos:pos+size(refs,1)-1) = 2
                  potencial_dat_vol%valor1(pos:pos+size(refs,1)-1) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_vol%modo1(pos:pos+size(refs,1)-1) = 1
              potencial_dat_vol%etiqueta1(pos:pos+size(refs,1)-1) = trim(sval)
            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        else
          call error('/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Conductor/Domain references with 0 references')
        endif
!!!Initial intensity
        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Conductor/Initial intensity', real1)
        potencial_dat_vol%valor3(pos:pos+size(refs,1)-1) =  real1
!!!Potential drop
        call flist(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Potential drop', list2)
        select case(trim(list2(1)))
          case('A precompiled function')      
            call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
            &'/Single/Potential drop/A precompiled function', sval)
            !if(size(list3,1)/=1) call error('readxml: in volumetric potential drop: One function must be selected')
            potencial_dat_vol%modo2(pos:pos+size(refs,1)-1) = 1
!            select case(sval)
!              case('Example 1')
                potencial_dat_vol%etiqueta2(pos:pos+size(refs,1)-1) = trim(sval)
!              case('User defined')
!                potencial_dat_vol%etiqueta2(pos:pos+size(refs,1)-1) = trim(sval)
!              case default; call error('readxml: in volumetric potential drop: function not implemented.')
!            end select
          case('A constant')
            call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//'/Single/Potential drop/A constant', real1)
            potencial_dat_vol%modo2(pos:pos+size(refs,1)-1) = 2
            potencial_dat_vol%valor2(pos:pos+size(refs,1)-1) = real1
          case default; call error('readxml: in volumetric potential drop: Case not implemented.')
        end select
        potencial_dat_vol%numero =  potencial_dat_vol%numero + size(refs,1)
      case('Coupled')
        couplepos = potencial_dat_vol%ncouples +1
        pos = potencial_dat_vol%numero + 1
        !Conductor1 References
        call fread_alloc(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Domain references', list2, realloc=.true.)
        potencial_dat_vol%referencias(pos) = int(list2(1))
        potencial_dat_vol%icouple(1,couplepos) = pos
        !Conductor1 Material
        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_vol%modo1(pos) = 2
                  potencial_dat_vol%valor1(pos) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_vol%modo1(pos) = 1
              potencial_dat_vol%etiqueta1(pos) = trim(sval)

            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        !Initial intensity
        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Initial intensity', real1)
        potencial_dat_vol%valor3(pos) =  real1
        !Conductor1 Thickness
!        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
!        &'/Coupled/First conductor/Thickness', real1)
        potencial_dat_vol%numero =  potencial_dat_vol%numero + 1
        pos = potencial_dat_vol%numero + 1
        !Conductor2 References
        call fread_alloc(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Second conductor/Domain references', list3, realloc=.true.)
        potencial_dat_vol%referencias(pos) = int(list3(1))
        potencial_dat_vol%icouple(2,couplepos) = pos
!       Initial intensity conductor 2
        potencial_dat_vol%valor3(pos) =  -potencial_dat_vol%valor3(pos-1)
        !Conductor2 Material
        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Second conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_vol%modo1(pos) = 2
                  potencial_dat_vol%valor1(pos) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_vol%modo1(pos) = 1
              potencial_dat_vol%etiqueta1(pos) = trim(sval)

            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        !Conductor2 Thickness
!        call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
!        &'/Coupled/Second conductor/Thickness', real1)
        !Potential drop
        call flist(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Potential drop', list2)
        select case(trim(list2(1)))
          case('A function')     
            potencial_dat_vol%modo4(couplepos) = 1
            call flist(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A function', list3)
            if(size(list3,1)/=1) call error('readxml: in volumetric potential drop: One function must be selected')
            select case(trim(list3(1)))
              case('Sine') 
                potencial_dat_vol%etiqueta4(couplepos) = 'sine'
                call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Sine/Amplitude', real1)
                amp_vol(couplepos) = real1
                call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Sine/Frequency', real1)
                freq_vol(couplepos) = real1
              case('Cosine') 
                potencial_dat_vol%etiqueta4(couplepos) = 'cosine'
                call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Cosine/Amplitude', real1)
                amp_vol_cos(couplepos) = real1 
                call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Cosine/Frequency', real1)
                freq_vol_cos(couplepos) = real1
              case default; call error('readxml: in volumetric potential drop: function not implemented.')
            end select
!            potencial_dat_vol%modo2(pos:pos+size(refs,1)-1) = 1
!            potencial_dat_vol%etiqueta2(pos:pos+size(refs,1)-1) = trim(list3(1))
          case('A precompiled function')     
            potencial_dat_vol%modo4(couplepos) = 1 
            call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A precompiled function', sval)
!            potencial_dat_vol%etiqueta4(couplepos) = trim(list3(1))
            if(size(list3,1)/=1) call error('readxml: in volumetric potential drop: One function must be selected')
!            potencial_dat_vol%modo2(pos) = 1
!            potencial_dat_vol%modo2(pos-1) = 1
!            potencial_dat_vol%etiqueta2(pos) = trim(list3(1))
!            potencial_dat_vol%etiqueta2(pos-1) = trim(list3(1))
!            potencial_dat_vol%etiqueta4(couplepos) = trim(list3(1))
!            select case(sval)
!              case('Example 1')
                potencial_dat_vol%etiqueta4(couplepos) = trim(sval)
!              case('User defined')
!                potencial_dat_vol%etiqueta4(couplepos) = trim(sval)
!              case default; call error('readxml: in volumetric potential drop: function not implemented.')
!            end select
          case('A constant')
            potencial_dat_vol%modo4(couplepos) = 2
            call fread(idme, '/Sources/Volumetric/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A constant', real1)
!            potencial_dat_vol%modo2(pos) = 2
!            potencial_dat_vol%modo2(pos-1) = 2
!            potencial_dat_vol%valor2(pos) = real1
!            potencial_dat_vol%valor2(pos-1) = real1
            potencial_dat_vol%valor4(couplepos) = real1
          case default; call error('readxml: in volumetric potential drop: Case not implemented.')
        end select

        potencial_dat_vol%ncouples = potencial_dat_vol%ncouples + 1
        potencial_dat_vol%numero =  potencial_dat_vol%numero + 1
    end select
enddo 

! Surface sources: potential drop

pos = 0
couplepos = 0
call flist(idme, '/Sources/Surface/Potential drop/Sources', list)
potencial_dat_sur%numero = 0
potencial_dat_sur%ncouples = 0
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))&
  &, list1)
!Single conductor
    select case(trim(list1(1)))
      case('Single')
!!!References
        call fread_alloc(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&

        &'/Single/Conductor/Surface references', refs,realloc=.true.)
        if (size(refs,1)>0) then
          pos = potencial_dat_sur%numero + 1
          potencial_dat_sur%referencias(pos:pos+size(refs,1)-1) = int(refs)
!!!Electrical conductivity
          call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
          &'/Single/Conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_sur%modo1(pos:pos+size(refs,1)-1) = 2
                  potencial_dat_sur%valor1(pos:pos+size(refs,1)-1) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_sur%modo1(pos:pos+size(refs,1)-1) = 1
              potencial_dat_sur%etiqueta1(pos:pos+size(refs,1)-1) = trim(sval)

            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        else
          call error('/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Conductor/Surface references with 0 references')
        endif
!!!Initial intensity
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Conductor/Initial intensity', real1)
        potencial_dat_sur%valor3(pos:pos+size(refs,1)-1) =  real1
!!!Potential drop
        call flist(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Single/Potential drop', list2)
        select case(trim(list2(1)))
          case('A precompiled function')      
            call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
            &'/Single/Potential drop/A precompiled function', sval)
            if(size(list3,1)/=1) call error('readxml: in surface potential drop: One function must be selected')
            potencial_dat_sur%modo2(pos:pos+size(refs,1)-1) = 1
!            select case(sval)
!              case('Example 1')
!                potencial_dat_sur%etiqueta2(pos:pos+size(refs,1)-1) = trim(sval)
!              case('User defined')
                potencial_dat_sur%etiqueta2(pos:pos+size(refs,1)-1) = trim(sval)
!              case default; call error('readxml: in surface potential drop: function not implemented.')
!            end select
          case('A constant')
            call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//'/Single/Potential drop/A constant', real1)
            potencial_dat_sur%modo2(pos:pos+size(refs,1)-1) = 2
            potencial_dat_sur%valor2(pos:pos+size(refs,1)-1) = real1
          case default; call error('readxml: in surface potential drop: Case not implemented.')
        end select
        potencial_dat_sur%numero =  potencial_dat_sur%numero + size(refs,1)
      case('Coupled')
        couplepos = potencial_dat_sur%ncouples +1
        pos = potencial_dat_sur%numero + 1
        !Conductor1 References
        call fread_alloc(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Surface references', list2, realloc=.true.)
        potencial_dat_sur%referencias(pos) = int(list2(1))
        potencial_dat_sur%icouple(1,couplepos) = pos
        !Conductor1 Material
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_sur%modo1(pos) = 2
                  potencial_dat_sur%valor1(pos) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_sur%modo1(pos) = 1
              potencial_dat_sur%etiqueta1(pos) = trim(sval)
            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        !Initial intensity
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Initial intensity', real1)
        potencial_dat_sur%valor3(pos) =  real1
        !Conductor1 Thickness
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/First conductor/Thickness', real1)
        potencial_dat_sur%valor0(pos) = real1
        potencial_dat_sur%numero =  potencial_dat_sur%numero + 1
        pos = potencial_dat_sur%numero + 1
        !Conductor2 References
        call fread_alloc(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Second conductor/Surface references', list3, realloc=.true.)
        potencial_dat_sur%referencias(pos) = int(list3(1))
        potencial_dat_sur%icouple(2,couplepos) = pos
!       Initial intensity conductor 2
        potencial_dat_sur%valor3(pos) =  -potencial_dat_sur%valor3(pos-1)
        !Conductor2 Material
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Second conductor/Material', sval)
          call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
          if(size(list2,1)<1) call error('Undefined electrical conductivity: '//trim(prefix)//trim(sval))   
          do k = 1, size(list2,1) 
            select case(trim(list2(k)))
            case('A constant')
              call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
              do l = 1, size(list3,1)
                select case(trim(list3(l)))
                case('Isotropic')
                  call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                  & real1)
                  potencial_dat_sur%modo1(pos) = 2
                  potencial_dat_sur%valor1(pos) = real1
                case default
                  call error(trim(prefix)//trim(sval)//'/Electrical conductivity/'//&
                  & trim(list3(l))//': not implemented')
                end select
              enddo
            case('A function')
              potencial_dat_sur%modo1(pos) = 1
              potencial_dat_sur%etiqueta1(pos) = trim(sval)
            case default
              call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(list2(k))//&
              & ': not implemented')      
            end select
          enddo
        !Conductor2 Thickness
        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Second conductor/Thickness', real1)
        potencial_dat_sur%valor0(pos) = real1
        !Potential drop
        call flist(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
        &'/Coupled/Potential drop', list2)
        select case(trim(list2(1)))
          case('A function')     
            potencial_dat_sur%modo4(couplepos) = 1
            call flist(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A function', list3)
            if(size(list3,1)/=1) call error('readxml: in surface potential drop: One function must be selected')
            select case(trim(list3(1)))
              case('Sine') 
                potencial_dat_sur%etiqueta4(couplepos) = 'sine'
                call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Sine/Amplitude', real1)
                amp_sur(couplepos) = real1
                call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Sine/Frequency', real1)
                freq_sur(couplepos) = real1
              case('Cosine') 
                potencial_dat_sur%etiqueta4(couplepos) = 'cosine'
                call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Cosine/Amplitude', real1)
                amp_sur_cos(couplepos) = real1 
                call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
                &'/Coupled/Potential drop/A function/Cosine/Frequency', real1)
                freq_sur_cos(couplepos) = real1
              case default; call error('readxml: in surface potential drop: function not implemented.')
            end select
!            potencial_dat_vol%modo2(pos:pos+size(refs,1)-1) = 1
!            potencial_dat_vol%etiqueta2(pos:pos+size(refs,1)-1) = trim(list3(1))
          case('A precompiled function')     
            potencial_dat_sur%modo4(couplepos) = 1 
            call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A precompiled function', sval)
!            potencial_dat_vol%etiqueta4(couplepos) = trim(list3(1))
            if(size(list3,1)/=1) call error('readxml: in surface potential drop: One function must be selected')
!            potencial_dat_vol%modo2(pos) = 1
!            potencial_dat_vol%modo2(pos-1) = 1
!            potencial_dat_vol%etiqueta2(pos) = trim(list3(1))
!            potencial_dat_vol%etiqueta2(pos-1) = trim(list3(1))
 !           select case(trim(sval))
!              case('Example 1')
                potencial_dat_sur%etiqueta4(couplepos) = trim(sval)
!              case('User defined')
!                potencial_dat_sur%etiqueta4(couplepos) = trim(sval)
!              case default; call error('readxml: in volumetric potential drop: function not implemented.')
!            end select
          case('A constant')
            potencial_dat_sur%modo4(couplepos) = 2
            call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
            &'/Coupled/Potential drop/A constant', real1)
!            potencial_dat_vol%modo2(pos) = 2
!            potencial_dat_vol%modo2(pos-1) = 2
!            potencial_dat_vol%valor2(pos) = real1
!            potencial_dat_vol%valor2(pos-1) = real1
            potencial_dat_sur%valor4(couplepos) = real1
          case default; call error('readxml: in surface potential drop: Case not implemented.')
        end select

        potencial_dat_sur%ncouples = potencial_dat_sur%ncouples + 1
        potencial_dat_sur%numero =  potencial_dat_sur%numero + 1
    end select
enddo 


! Surface sources: current density

call flist(idme, '/Sources/Surface/Current density/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Surface/Current density/Sources/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Sources/Surface/Current density/Sources/'//trim(list(i))//&
      &'/A function/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcesur%numero + 1
        sourcesur%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcesur%modo(pos:pos+size(refs,1)-1) = 1
        sourcesur%itipo(pos:pos+size(refs,1)-1) = 1
        sourcesur%numero = sourcesur%numero + size(refs,1)
      else
        print * , 'Function current source with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Sources/Surface/Current density/Sources/'//trim(list(i))//&
      &'/A constant/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcesur%numero + 1
        sourcesur%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcesur%modo(pos:pos+size(refs,1)-1) = 2
        sourcesur%itipo(pos:pos+size(refs,1)-1) = 1
        sourcesur%numero = sourcesur%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Sources/Surface/Current density/Sources/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        sourcesur%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
    case default; call error('readxml: in surface current density: case not implemented.')
    end select
  enddo
enddo  

call flist(idme, '/Sources/Surface/Current intensity/Sources', list)
do i = 1, size(list,1) !loop for defined current sources
  call flist(idme, '/Sources/Surface/Current intensity/Sources/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(idme, '/Sources/Surface/Current intensity/Sources/'//trim(list(i))//&
      &'/A function/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcesur%numero + 1
        sourcesur%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcesur%modo(pos:pos+size(refs,1)-1) = 1
        sourcesur%itipo(pos:pos+size(refs,1)-1) = 2
        sourcesur%numero = sourcesur%numero + size(refs,1)
        call fread(idme, '/Sources/Surface/Current intensity/Sources/'//trim(list(i))//&
        &'/A function/Function name', sval)
        sourcesur%etiqueta(pos:pos+size(refs,1)-1) = trim(sval)
      else
        print * , 'Function current source with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(idme, '/Sources/Surface/Current intensity/Sources/'//trim(list(i))//&
      &'/A constant/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        pos = sourcesur%numero + 1
        sourcesur%referencias(pos:pos+size(refs,1)-1) = int(refs)
        sourcesur%modo(pos:pos+size(refs,1)-1) = 2
        sourcesur%itipo(pos:pos+size(refs,1)-1) = 2
        sourcesur%numero = sourcesur%numero + size(refs,1)
        !Constant value
        call fread(idme, '/Sources/Surface/Current intensity/Sources/'//trim(list(i))//&
        &'/A constant/Constant value', real1)
        sourcesur%valor(pos:pos+size(refs,1)-1) = real1
      else
        print * , 'Constant current source with 0 references: skipping'
      endif
    case default; call error('readxml: in surface current intensity: case not implemented.')
    end select
  enddo
enddo 

! Surface sources: potential drop

!call flist(idme, '/Sources/Surface/Potential drop/Sources', list)
!
!do i = 1, size(list,1) !loop for defined current sources
!  call flist(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!  &'/Potential drop', list1)
!
!  do j = 1, size(list1,1) !loop for data type for each BC 
!    select case(trim(list1(j)))
!    case('A function')
!      !References
!      call fread_alloc(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!      &'/Surface references', refs, realloc=.true.)
!      if (size(refs,1)>0) then
!        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!            &'/Potential drop/A function/Function name', sval)
!        pos = potencial_dat_sur%numero + 1
!        potencial_dat_sur%referencias(pos:pos+size(refs,1)-1) = int(refs)
!        potencial_dat_sur%numero = potencial_dat_sur%numero + size(refs,1)
!        potencial_dat_sur%modo2(pos:pos+size(refs,1)-1) = 1
!        potencial_dat_sur%etiqueta2(i) = sval
!
!      else
!        print * , 'Function current source with 0 references: skipping'
!      endif
!    case('A constant')
!      !References
!      call fread_alloc(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!      &'/Surface references', refs, realloc=.true.)
!      if (size(refs,1)>0) then
!        pos = potencial_dat_sur%numero + 1
!        potencial_dat_sur%referencias(pos:pos+size(refs,1)-1) = int(refs)
!        potencial_dat_sur%numero = potencial_dat_sur%numero + size(refs,1)
!        potencial_dat_sur%modo2(pos:pos+size(refs,1)-1) = 2
!        !Constant value
!        call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!        &'/Potential drop/A constant/Constant value', real1)
!        potencial_dat_vol%valor2(pos:pos+size(refs,1)-1) = real1
!      else
!        print * , 'Constant current source with 0 references: skipping'
!      endif
!    case default; call error('readxml: in surface potential drop: case not implemented.')
!    end select
!  enddo
!
!  call fread(idme, '/Sources/Surface/Potential drop/Sources/'//trim(list(i))//&
!  &'/Electrical conductivity', sval)
!
!  call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity', list2)
!  do k = 1, size(list2,1) 
!    select case(trim(list2(k)))
!    case('A constant')
!      call flist(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant', list3)
!      do l = 1, size(list3,1)
!        select case(trim(list3(l)))
!        case('Isotropic')
!          call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
!          & real1)
!          potencial_dat_sur%modo1(i) = 2
!          potencial_dat_sur%valor1(i) = real1
!        case default
!          call error(trim(prefix)//trim(sval)//'/Electrical conductivity/A constant/'//&
!          & trim(sval2)//': not implemented')
!        end select
!      enddo
!    case('A function')
!      call fread(idma, trim(prefix)//trim(sval)//'/Electrical conductivity/A function/Function name', sval2)
!      potencial_dat_sur%modo1(i) = 1
!      potencial_dat_sur%etiqueta1(i) = trim(sval2)
!    case default
!      call error(trim(prefix)//trim(sval)//'/Electrical conductivity'//trim(sval1)//': not implemented')      
!    end select
!  enddo
!
!enddo 


  call fread_alloc(idme, '/Data/Time/Interval', aux1, realloc=.true.)
  if (size(aux1,1) < 2) then
     call error('readxml: time interval must have two real numbers at least.') 
  endif
  ti = aux1(1)
  npat = size(aux1,1)
  tf = aux1(npat)

! materials , Magnetic properties

iopteta = 0
ndnolin = 0
iopdli = 0 !No hay materiales no-lineales
ndnolin = 0

call flist(idme, '/Properties/Materials/Materials', list)
permagrel%numero = size(list,1)


do i = 1, size(list,1)

  permagrel%referencias(i) = int(list(i))

  call fread(idme, '/Properties/Materials/Materials/'//trim(list(i)), sval)
  call flist(idma, trim(prefix)//trim(sval)//'/Magnetic properties', list1)

  select case(trim(list1(1)))

    case('Relative magnetic permeability')
      permagrel%ioplin(i) = 1 !Material lineal
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
              permagrel%valorx(i) = real1
              permagrel%valory(i) = real1
            case('Orthotropic')
              call fread(idma, trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(rmp)//&
              &'/A constant/Orthotropic/X Value', real1)    
              permagrel%valorx(i) = real1
              call fread(idma, trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(rmp)//&
              &'/A constant/Orthotropic/Y Value', real1)    
              permagrel%valory(i) = real1
              case default
                call error('readxml: in constant magnitude: case not implemented.')
          end select

        case('A function')
          permagrel%iopermagr(i) = 1 !Definido por funcion
        case default
          call error('readxml: ' //trim(list2(1))// ' : case not implemented.')
      end select

    case('H-B Table')
      ndnolin = ndnolin + 1
      idnolin(ndnolin)=permagrel%referencias(i)
      iopdli = 1 ! hay materiales no-lineales
      call flist(idma, trim(prefix)//trim(sval)//'/Magnetic properties/H-B Table', list3)
      select case(trim(list3(1)))
          case('From file')
               call fread(idma, trim(prefix)//trim(sval)//'/Magnetic properties/H-B Table/From file/Gr2 file', sval2)
! Lectura del fichero gr2----------------------------------------------------------------
               open(unit=999,file=trim(sval2))
               read(999,*) numsigs
               if(numsigs>1) then
                    call error('readxml: too much signals in the file '//trim(sval2)//'. Only one supported')
               endif
               read(999,*, iostat=ios) n1
               if(ios/=0) stop 'ERROR: /H-B table/From file: GR2 file format failure'
               n2 = n1
               if(allocated(aux1)) deallocate(aux1)
               allocate(aux1(n1))
               if(allocated(aux2)) deallocate(aux2)
               allocate(aux2(n2)) 
               read(999,*, iostat=ios)(aux1(k),k=1,n1)
               if(ios/=0) stop 'ERROR: /H-B table/From file: GR2 file format failure'
               read(999,*, iostat=ios)(aux2(k),k=1,n2)
               if(ios/=0) stop 'ERROR: /H-B table/From file: GR2 file format failure'
               close(999)
! ----------------------------------------------------------------------------------------
          case('Build table')
               call fread_alloc(idma,trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(hb)//&
                  &'/Build table/H values', aux1, realloc=.true.)
               call fread_alloc(idma,trim(prefix)//trim(sval)//'/Magnetic properties/'//trim(hb)//&
                  &'/Build table/B values', aux2, realloc=.true.)
               print*,'Read HB done'
           case default
               call error('readxml: ' //trim(list2(1))// ' : case not implemented.')
       end select

       n1 = size(aux1,1)
       n2 = size(aux2,1)
       if (n1/=n2) then
           call error('readxml: wrong number of element in H-B table')
       else
           if (allocated(permagrel%hb(ndnolin)%h)) deallocate(permagrel%hb(ndnolin)%h)
           if (allocated(permagrel%hb(ndnolin)%b)) deallocate(permagrel%hb(ndnolin)%b) 
           allocate(permagrel%hb(ndnolin)%h(n1), permagrel%hb(ndnolin)%b(n2))
           permagrel%hb(ndnolin)%h = aux1
	   permagrel%hb(ndnolin)%b = aux2
           call fread(idme, '/Data/Mathematical parameters/Parameters/Convergence of tolerance', real1)
           e = real1
           call fread(idme, '/Data/Mathematical parameters/Parameters/Maximum number of iterations', real1)
           niter = real1
       endif


    case default
      call error('readxml: ' //trim(list1(1))// ' : case not implemented.')
  end select
enddo

iop = 3
iopf = 1
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


