subroutine readxml()

use module_SO_DEPENDANT
use module_REPORT
use module_xml_parser, inti => int ! para ifort
use fich_electros
use dcurrent_2D
use conductividad
use bloqueo
use derivados

implicit none

 character(len=43) :: dir0 = 'Dirichlet: electric scalar potential, V (V)'
 character(len=25) :: dir1 = 'Electric scalar potential'
 character(len=55) :: neu0 = 'Neumann'
 character(len=22) :: neu1 = 'Normal current density'
 character(len=33) :: neuci0 = 'Neumann: current intensity, I (A)'
 character(len=17) :: neuci1 = 'Current intensity'

 integer :: i, j, pos, ide, im
 !integer :: iopblo,iopblo1,iopblo2,iopblo3,iopneu,iopinneu1,iopinneu2,iopint,iop,iopf
 real(DOUBLE) :: cval
 real(DOUBLE), dimension(:), allocatable :: aux ! xcp
 character(len=MAXPATH) :: matxml, datxml,mat,sval ! fichma,, fichsol
 !character(len=MAXPATH) :: DirBC_Func, NeuBC_Func
 !character(len=MAXPATH) :: fichElectricField, fichCurrentDensity
 character(len=MAXPATH), dimension(:), allocatable :: list, list2, list3, refs
 !type(interfaz) :: dircons, dirfunc, neucons, neufunc
 !type(interfazxy) :: intcons, intfunc

 call set_SO()
 call set_report_level(REPORT_STDOUT)

!inicialización de variables
 iopblo = 0
 iopblo1 = 0
 iopblo2 = 0
 iopblo3 = 0
 iopneu = 0
 iopinneu1 = 0
 iopinneu2 = 0
 iopint = 0
 iop = 3
 iopf = 1

!ficheros
fichsol='fichsol'
fichElectricField='fiche'
fichCurrentDensity='fichdc'

!lectura do ficheiro XML
! datxml = 'local.dat.xml'
! ide = fopen(datxml)
 ide = fopen()

 !Mesh
 call fread(ide, '/Mesh/Open/Mesh file', fichma)
 !call fread(im, '/Materials file/Open/materialsDB', matxml)


pos = 0
nrd = 0
blofron%numero = 0
!Condiciones Dirichlet
 call flist(ide, '/Boundary conditions/'//trim(dir0)//'/'//trim(dir1), list)
 do i = 1, size(list,1)
  pos = 0
  call flist(ide, '/Boundary conditions/'//trim(dir0)//'/'//trim(dir1)//'/'//trim(list(i)), list2)
  do j = 1, size(list2,1)
   select case(trim(list2(j)))
     !Por funcion
     case('A function')
      iopblo1 = 1
      iopblo2 = 0
      iopblo3 = 0

      call fread_alloc(ide, '/Boundary conditions/'//trim(dir0)//'/'//trim(dir1)//'/'//trim(list(i))//&
      &'/A function/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then

		iopblo = 1
		call fread(ide,'/Boundary conditions/'//trim(dir0)//'/'//trim(dir1)//'/'//trim(list(i))//&
      &'/A function/Function name',sval)
		DirBC_Func = trim(sval)
		pos = nrd + 1
		nrd = nrd + size(refs,1)
		irefd(pos:pos+size(refs,1)-1) = inti(refs)
		pos = nrd
      else
       print * , 'Function '//dir0//' with 0 references: skipping'
      endif
     !Por constante
     case('A constant')
        iopblo1 = 0
        iopblo2 = 1
        iopblo3 = 0

        call fread_alloc(ide, '/Boundary conditions/'//trim(dir0)//'/'//trim(dir1)//'/'//trim(list(i))//&
        &'/A constant/Line references', refs, realloc=.true.)
        if (size(refs,1)>0) then
         iopblo = 1
         call fread(ide,'/Boundary conditions/'//trim(dir0)//'/'//trim(dir1)//'/'//trim(list(i))//&
         &'/A constant/Constant value',cval)
         pos = blofron%numero + 1
         blofron%numero = blofron%numero + size(refs,1)
         blofron%referencias(pos:pos+size(refs,1)-1) = inti(refs)
         blofron%valor(pos:pos+size(refs,1)-1)  = cval
         pos = blofron%numero
        else
         print * , 'Constant '//dir0//' with 0 references: skipping'
        endif

     case default; call error('readxml: Case not implemented.')
    end select
  enddo
 enddo

neuman%numero = 0
!Condiciones Neumann
 call flist(ide, '$Boundary conditions$'//trim(neu0)//'$'//trim(neu1), list)
 do i = 1, size(list,1)
  call flist(ide, '$Boundary conditions$'//trim(neu0)//'$'//trim(neu1)//'$'//trim(list(i)), list2)
    do j = 1, size(list2,1)
     select case(trim(list2(j)))
      !Por funcion
      case('A function')
       iopinneu1 = 1
       iopinneu2 = 0
       call fread_alloc(ide, '$Boundary conditions$'//trim(neu0)//'$'//trim(neu1)//'$'//trim(list(i))//&
       &'$A function$Line references', refs, realloc=.true.)
       if (size(refs,1)>0) then
        iopneu = 1
		call fread(ide,'$Boundary conditions$'//trim(neu0)//'$'//trim(neu1)//'$'//trim(list(i))//&
       &'$A function$Function name',sval)
		NeuBC_Func = trim(sval)
		pos = nrn + 1
		nrn = nrn + size(refs,1)
		irefn(pos:pos+size(refs,1)-1) = inti(refs)
		pos = nrn
       else
        print * , 'Function '//neu0//' with 0 references: skipping'
	   endif
       !Por constante
      case('A constant')
        iopinneu1 = 0
        iopinneu2 = 1

        call fread_alloc(ide, '$Boundary conditions$'//trim(neu0)//'$'//trim(neu1)//'$'//trim(list(i))//&
        &'$A constant$Line references', refs, realloc=.true.)

        if (size(refs,1)>0) then
         call fread(ide,'$Boundary conditions$'//trim(neu0)//'$'//trim(neu1)//'$'//trim(list(i))//&
         &'$A constant$Constant value',cval)
         iopneu = 1
         pos = neuman%numero + 1
         neuman%numero = neuman%numero + size(refs,1)
         neuman%referencias(pos:pos+size(refs,1)-1) = inti(refs)
         neuman%valor(pos:pos+size(refs,1)-1)  = cval
         pos = neuman%numero
       else
         print * , 'Constant '//neu0//' with 0 references: skipping'
       endif

       case default; call error('readxml: Case not implemented.')
     end select
  enddo
 enddo

pos = 0
inten%numero = 0
!Condiciones Neumann: Current intensity
call flist(ide, '/Boundary conditions/'//trim(neuci0)//'/'//trim(neuci1), list)
if (allocated(list)) then
  do i = 1, size(list,1)
    call flist(ide, '/Boundary conditions/'//trim(neuci0)//'/'//trim(neuci1)//'/'//trim(list(i)), list2)
      do j = 1, size(list2,1)
        iopinneu1 = 0
        iopinneu2 = 1
        call fread_alloc(ide, '/Boundary conditions/'//trim(neuci0)//'/'//trim(neuci1)//'/'//trim(list(i))//&
        &'/Line references', refs, realloc=.true.)
        if (size(refs,1)>0) then
        iopint = 1
        iopint2 = 1
          call fread(ide,'/Boundary conditions/'//trim(neuci0)//'/'//trim(neuci1)//'/'//trim(list(i))//&
          &'/Constant value',cval)
          pos  = inten%numero + 1
          inten%numero = inten%numero + size(refs,1)
          inten%referencias(pos:pos+size(refs,1)-1) = inti(refs)
          inten%valor(pos:pos+size(refs,1)-1) = cval
          call fread(ide,'/Boundary conditions/'//trim(neuci0)//'/'//trim(neuci1)//'/'//trim(list(i))//&
          &'/Thickness',cval)
          inten%thickness(pos:pos+size(refs,1)-1) = cval
          pos = inten%numero
        else
          print * , 'Constant '//neuci0//' with 0 references: skipping'
        endif
     enddo
  enddo
endif

!Datos del ficheo de materiales
! matxml = 'materials.dat.xml'
 call fread(ide, '/Materials file/Open/materialsDB', matxml)
 im = fopen(matxml)

call flist(ide,'/Properties/Materials/Materials',list)
conduc%numero = size(list,1)
do i = 1, size(list,1) !Recorre subdominios
  call fread(ide,'/Properties/Materials/Materials/'//list(i), mat)
  conduc%referencias(i) = inti(list(i))
  call flist(im,'/Materials database/Open/Materials/'//trim(mat)//'/Electrical conductivity', list2)
  if (size(list2,1)==0) call error('readxml: missing Electrical conductivity type for material')
  select case(trim(list2(1)))
      case('A constant')
         call flist(im,'/Materials database/Open/Materials/'//trim(mat)//'/Electrical conductivity/A constant', list3)
         if (size(list3,1)==0) call error('readxml: missing constant Electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(im, '/Materials database/Open/Materials/'//trim(mat)//'/Electrical conductivity/A constant/Isotropic',&
                 conduc%valorx(i))
                 conduc%valory(i) = conduc%valorx(i)
             case('Orthotropic')
                 call fread(im, '/Materials database/Open/Materials/' &
                     &//trim(mat)//'/Electrical conductivity/A constant/Orthotropic/X value',&
                 conduc%valorx(i))
                 call fread(im, '/Materials database/Open/Materials/' &
                     &//trim(mat)//'/Electrical conductivity/A constant/Orthotropic/Y value',&
                 conduc%valory(i))
             case default; call error('readxml: Case not implemented.')
         end select
         conduc%iopcond(i) = 2
      case('A temperature dependant table')
         call flist(im,'/Materials database/Open/Materials/' &
             &//trim(mat)//'/Electrical conductivity/A temperature dependant table', list3)
         if (size(list3,1)==0) call error('readxml: missing temperature dependant Electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
             !utilizamos unha variable auxiliar (aux) allocatable para saber o nº de elementos
                call fread_alloc(im, '/Materials database/Open/Materials/' &
                &//trim(mat)//'/Electrical conductivity/A temperature dependant table/Isotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                &//trim(mat)//'/Electrical conductivity/A temperature dependant table/Isotropic/Values', conduc%valtabx(i,:))
                conduc%ntab(i) = size(aux,1)
                conduc%teta(i,1:conduc%ntab(i)) = aux
                conduc%valtaby(i,1:conduc%ntab(i)) = conduc%valtabx(i,1:conduc%ntab(i))
             case('Orthotropic')
                call fread_alloc(im, '/Materials database/Open/Materials/'//trim(mat)//&
                &'/Electrical conductivity/A temperature dependant table/Orthotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                &//trim(mat)//'/Electrical conductivity/A temperature dependant table/Orthotropic/X values', conduc%valtabx(i,:))
                call fread(im, '/Materials database/Open/Materials/' &
                &//trim(mat)//'/Electrical conductivity/A temperature dependant table/Orthotropic/Y values', conduc%valtaby(i,:))
                conduc%ntab(i) = size(aux,1)
                conduc%teta(1,1:conduc%ntab(i)) = aux
             case default; call error('readxml: Case not implemented.')
         end select
         conduc%iopcond(i) = 3
         iopteta = 1
         call fread(ide,'/Data/Temperature/Field',sval)
         fichteta = trim(sval)
      case('A function')
         conduc%etiqueta(i) = trim(mat)
         conduc%iopcond(i) = 1
  case default; call error('readxml: Case not implemented.')
  end select
	print*, 'Dominio: ',trim(list(i)),' iopcond: ', conduc%iopcond(i), 'size(conduc): ',size(conduc%referencias)
enddo

 call fclose(ide)
 call fclose(im)

!Pruebas de las escrituras de datos
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                     Escritura al fichero 'datos.dat'                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! open(im,file='datos.dat',status='unknown')
!
!!Bloque de condiciones Dirichlet
! write(im,*) trim(fichma)
! write(im,*) trim('fichplot')
! write(im,*) iopblo, '	&iopblo'
! if (iopblo==1) then
!	if (dircons%numero >0) then
!		write(im,*) 0, '	&iopblo1' !iopblo1: por funcion
!		write(im,*) 1, '	&iopblo2' !iopblo2: por constante
!		write(im,*) 0, '	&iopblo3' !iopblo3: bloqueo puntual, no se contempla
!		write(im,*) dircons%numero, '	&blofron%numero'
!		do i=1,dircons%numero
!			write(im,*) dircons%referencias(i), '	&blofron%referencias'
!			write(im,*) dircons%valor, '	&blofron%valor'
!		enddo
!	endif
!	if (dirfunc%numero >0) then
!		write(im,*) 0, '	&iopblo1' !iopblo1: por funcion
!		write(im,*) 1, '	&iopblo2' !iopblo2: por constante
!		write(im,*) 0, '	&iopblo3' !iopblo3: bloqueo puntual, no se contempla
!		write(im,*) dirfunc%numero, '	&blofron%numero'
!		do i=1,dirfunc%numero
!			write(im,*) dirfunc%referencias(i), '	&blofron%referencias'
!			write(im,*) dirfunc%valor, '	&blofron%valor'
!		enddo
!	endif
! endif
!
!!Bloque de condiciones Neumann
! write(im,*) iopneu, '	&iopneu'
! if (iopneu == 1) then
!	if (neucons%numero >0) then
!		write(im,*) 0, '	&iopneu1' !iopneu1: por funcion
!		write(im,*) 1, '	&iopneu2' !iopneu2: por constante
!		write(im,*) neucons%numero, '	&neuman%numero'
!		do i=1,neucons%numero
!			write(im,*) neucons%referencias(i), '	&neuman%referencias'
!			write(im,*) neucons%valor(i), '	&neuman%valor'
!		enddo
!	endif
!	if (dirfunc%numero >0) then
!		write(im,*) 1, '	&iopneu1' !iopneu1: por funcion
!		write(im,*) 0, '	&iopneu2' !iopneu2: por constante
!		write(im,*) neufunc%numero, '	&neuman%numero'
!		do i=1,neufunc%numero
!			write(im,*) neufunc%referencias(i), '	&neuman%referencias'
!			write(im,*) neufunc%valor(i), '	&neuman%valor'
!		enddo
!	endif
! endif

! call fclose(im)

END subroutine
