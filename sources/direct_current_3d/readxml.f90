subroutine readxml()
!-----------------------------------------------------------------------
! procedure for reading the solver variables
!-----------------------------------------------------------------------
use module_SO_DEPENDANT
use module_REPORT
use module_xml_parser, inti => int ! para ifort
!Solver modules
use fich_electros3D
use electros3D, DOUBLElocal1 => DOUBLE
use cargavol, DOUBLElocal2 => DOUBLE
use cargacur, DOUBLElocal3 => DOUBLE
use cargapun, DOUBLElocal4 => DOUBLE
use conductividad, DOUBLElocal5 => DOUBLE
use bloqueo, DOUBLElocal6 => DOUBLE
use derivados3D, DOUBLElocal7 => DOUBLE, inte => intf
use auxiliar_cargas
!use parametros_electros3D

implicit none

!integer, parameter :: DOUBLElocal  = kind(0.0D0) ! colision en DOUBLE

!tamen en comprobaciones.f90
!para usar sin trim
 character(len=27) :: neu1 = 'Normal current density B.C.'
 character(len=22) :: neu2 = 'Normal current density'
 character(len=14) :: dir1 = 'Potential B.C.'
 character(len=25)  :: dir2 = 'Electric scalar potential'

integer :: i, j, pos, ide, im, fnum
real(DOUBLE) :: cval
real(DOUBLE), dimension(:), allocatable :: aux ! xcp
character(len=MAXPATH) :: matxml, sval, tval
character(len=MAXPATH), dimension(:), allocatable :: list, list2, list3, refs
call set_SO()
call set_report_level(REPORT_STDOUT)


    ! inicializacion de variables (array)
    ! fun_0 == User defined / Function defined by user
    dir%fun = 1
    neu%fun = 1
    vol%fun = 1
    sup%fun = 1
    cur%fun = 1
    inte%fun = 1
    

ide = fopen()

!Mesh
call fread(ide, '/Mesh/Open/Mesh file', fichma) ! era necesario para blocking_node()
!fichma = 'mesh-temp-3d.bin'

!Boundary Condicions
print*,'Neumann', '-', neu2
!Neumann conditions
iopneu = 0; iopneu1 = 0; iopneu2 = 0
nrn = 0
neuman%numero = 0
call flist(ide, '/Boundary conditions/'//neu1//'/'//neu2, list)
do i = 1, size(list,1) !loop for defined Neumann BC's
  call flist(ide, '/Boundary conditions/'//neu1//'/'//neu2//'/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(ide, '/Boundary conditions/'//neu1//'/'//neu2//'/'//trim(list(i))//&
      &'/A function/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopneu = 1
        iopneu1 = 1 ! ok
        !Function
        call fread(ide, '/Boundary conditions/'//neu1//'/'//neu2//'/'//trim(list(i))//&
            &'/A function/Function name', sval)
        pos = nrn + 1
        irefn(pos:pos+size(refs,1)-1) = inti(refs)
        fnum = function_number(sval, functions)
        if (fnum == 0) call error('readxml: unknown function: '//sval)
        neu%fun(pos:pos+size(refs,1)-1) = fnum
        nrn = nrn + size(refs,1)
      else
        print * , 'Function '//neu1//' with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(ide, '/Boundary conditions/'//neu1//'/'//neu2//'/'//trim(list(i))//&
      &'/A constant/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopneu = 1
        iopneu2 = 1 ! ok
        !Constant value
        call fread(ide, '/Boundary conditions/'//neu1//'/'//neu2//'/'//trim(list(i))//&
            &'/A constant/Constant value', cval)
        pos = neuman%numero + 1
        neuman%referencias(pos:pos+size(refs,1)-1) = inti(refs)
        neuman%numero = neuman%numero + size(refs,1)
        neuman%valor(pos:pos+size(refs,1)-1) = cval
      else
        print * , 'Constant '//neu1//' with 0 references: skipping'
      endif
    case default; call error('readxml: Case not implemented.')
    end select
  enddo
enddo


print*, 'Dirichlet', '-', dir2
!Potential (Dirichlet) conditions
iopblo = 0; iopblo1 = 0; iopblo2 = 0; iopblo3 = 0
nrd = 0
blofron%numero = 0
blopun%numero = 0
call flist(ide, '/Boundary conditions/'//dir1//'/'//dir2, list)
do i = 1, size(list,1) !loop for defined potential BC's
  call flist(ide, '/Boundary conditions/'//dir1//'/'//dir2//'/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(ide, '/Boundary conditions/'//dir1//'/'//dir2//'/'//trim(list(i))//&
      &'/A function/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopblo = 1
        iopblo1 = 1 ! ok
        !Function
        call fread(ide, '/Boundary conditions/'//dir1//'/'//dir2//'/'//trim(list(i))//&
            &'/A function/Function name', sval)
        pos = nrd + 1
        irefd(pos:pos+size(refs,1)-1) = inti(refs)
        fnum = function_number(sval, functions)
        if (fnum == 0) call error('readxml: unknown function: '//sval)
        dir%fun(pos:pos+size(refs,1)-1) = fnum
        nrd = nrd + size(refs,1)
      else
        print * , 'Function '//dir1//' with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(ide, '/Boundary conditions/'//dir1//'/'//dir2//'/'//trim(list(i))//&
      &'/A constant/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopblo = 1
        iopblo2 = 1 ! ok
        !Constant value
        call fread(ide, '/Boundary conditions/'//dir1//'/'//dir2//'/'//trim(list(i))//&
            &'/A constant/Constant value', cval)
        pos = blofron%numero + 1
        blofron%referencias(pos:pos+size(refs,1)-1) = inti(refs)
        blofron%numero = blofron%numero + size(refs,1)
        blofron%valor(pos:pos+size(refs,1)-1) = cval
      else
        print * , 'Constant '//dir1//' with 0 references: skipping'
      endif
!    case('Point')
!      iopblo3 = 1 ! ok
!      !References
!      call fread_alloc('/B.C./Define.../B.C. type/Potential/'//trim(list(i))//&
!      &'/Point/Reference number(s)', refs, realloc=.true.)
!      !Constant value
!      call fread('/B.C./Define.../B.C. type/Potential/'//trim(list(i))//&
!      &'/Point/Constant value', cval)
!      if (size(refs,1)>0)
!          iopblo3 = 1 ! ok
!          pos = blopun%numero + 1
!          blopun%referencias(pos:pos+size(refs,1)-1) = int(refs)
!          blopun%numero = blopun%numero + size(refs,1)
!          blopun%valor(pos:pos+size(refs,1)-1) = cval
!      else
!          print * , dir1//' with 0 references: skipping'
!      endif
    case default; call error('readxml: Case not implemented.')
    end select
  enddo
enddo


print*,'Intensity'
!Intensity conditions
iopint = 0; iopint1 = 0; iopint2 = 0
nri = 0
inten%numero = 0
call flist(ide, '/Boundary conditions/Intensity B.C./Current intensity', list)
do i = 1, size(list,1) !loop for defined Intensity BC's
  call flist(ide, '/Boundary conditions/Intensity B.C./Current intensity/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(ide, '/Boundary conditions/Intensity B.C./Current intensity/'//trim(list(i))//&
      &'/A function/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopint = 1
        iopint1 = 1 ! ok
        !Function
        call fread(ide, '/Boundary conditions/Intensity B.C./Current intensity/'//trim(list(i))//&
            &'/A function/Function name', sval)
        pos = nri + 1
        irefi(pos:pos+size(refs,1)-1) = inti(refs)
        fnum = function_number(sval, functions)
        if (fnum == 0) call error('readxml: unknown function: '//sval)
        inte%fun(pos:pos+size(refs,1)-1) = fnum
        nri = nri + size(refs,1)
      else
        print * , 'Function Intensity B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(ide, '/Boundary conditions/Intensity B.C./Current intensity/'//trim(list(i))//&
      &'/A constant/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopint = 1
        iopint2 = 1 ! ok
        !Constant value
        call fread(ide, '/Boundary conditions/Intensity B.C./Current intensity/'//trim(list(i))//&
            &'/A constant/Constant value', cval)
        pos = inten%numero + 1
        inten%referencias(pos:pos+size(refs,1)-1) = inti(refs)
        inten%numero = inten%numero + size(refs,1)
        inten%valor(pos:pos+size(refs,1)-1) = cval
      else
        print * , 'Constant Intensity B.C. with 0 references: skipping'
      endif
    case default; call error('readxml: Case not implemented.')
    end select
  enddo
enddo



!Sources
!print*,'Current sources'
!Volumic sources
iopvol = 0 ! 1 => hai volumic sources
carvol%numero = 0
!call flist(ide, '/Sources/Current sources/Current sources', list)
!do i = 1, size(list,1) !loop for defined volumic sources
!  call flist(ide, '/Sources/Current sources/Current sources/'//trim(list(i)), list2)
!  if (size(list2,1)/=1) call error('readxml: Incorrect number of childs in volume source.')
!  if (trim(list2(1)) == 'A function') then
!    !References
!    call fread_alloc(ide, '/Sources/Current sources/Current sources/'//trim(list(i))//&
!    &'/A function/Domain references', refs, realloc=.true.)
!    if (size(refs,1)>0) then
!        iopvol = 1
!        !Function
!        call fread(ide, '/Sources/Current sources/Current sources/'//trim(list(i))//&
!            &'/A function/Function name', sval)
!        pos = carvol%numero + 1
!        carvol%referencias(pos:pos+size(refs,1)-1) = inti(refs)
!        carvol%numero = carvol%numero + size(refs,1)
!        carvol%valor(pos:pos+size(refs,1)-1) = 0.d0
!        fnum = function_number(sval,functions)
!        if (fnum == 0) call error('readxml: unknown function: '//sval)
!        vol%fun(pos:pos+size(refs,1)-1) = fnum
!        carvol%constante(pos:pos+size(refs,1)-1) = .FALSE.
!    else
!        print * , 'Function volume source with 0 references: skipping'
!    endif
!  elseif (trim(list2(1)) == 'A constant') then
    !References
!    call fread_alloc(ide, '/Sources/Current sources/Current sources/'//trim(list(i))//&
!    &'/A constant/Domain references', refs, realloc=.true.)
!    if (size(refs,1)>0) then
!        iopvol = 1
        !Constant value
!        call fread(ide, '/Sources/Current sources/Current sources/'//trim(list(i))//&
!            &'/A constant/Constant value', cval)
!        pos = carvol%numero + 1
!        carvol%referencias(pos:pos+size(refs,1)-1) = inti(refs)
!        carvol%numero = carvol%numero + size(refs,1)
!        carvol%valor(pos:pos+size(refs,1)-1) = cval
!        carvol%constante(pos:pos+size(refs,1)-1) = .TRUE.
!    else
!        print * , 'Constant volume source with 0 references: skipping'
!    endif
!  else
!    call error('readxml: Incorrect volume source child: '//trim(list2(1))//'.')
!  endif
!enddo

!Surface sources
iopsup = 0 ! 1 => hai surface sources
carsup%numero = 0
!Curvilinear sources
iopcur = 0 ! 1 => hai line sources
carcur%numero = 0
!Point sources
ioppun = 0
ncarpun = 0



! 2010-02-08,11: Blocking node and Blocking value
! 2010-09-21: comentado
!print*,'Blocking node and blocking value'
!      call fread_alloc(ide, '/Data/Blocking for Neumann problem/'//&
!      &'Blocking for Neumann problem/Blocking node', xcp, realloc=.true.)
!      call fread_alloc(ide, '/Data/Blocking for Neumann problem/'//&
!      &'Blocking for Neumann problem/Blocking value', aux, realloc=.true.)

!      if ( size(xcp,1) > 1 ) call error('readxml: Only 0 or 1 blocking node allowed')
!      if ( size(aux,1) > 1 ) call error('readxml: Only 0 or 1 blocking value allowed')
!      if ( ( size(xcp,1) == 1 ) .and. ( size(aux,1) /= 1 ) )&
!        &call error('readxml: Found blocking node but no blocking value')
!      if ( ( size(aux,1) == 1 ) .and. ( size(xcp,1) /= 1 ) )&
!        &call error('readxml: Found blocking value but no blocking node')

!      if ( ( size(xcp,1) == 1 ) .and. ( size(aux,1) == 1 ) ) then
!        iopblo = 1
!        iopblo3 = 1
!        blopun%numero = blopun%numero + 1
!        blopun%referencias(blopun%numero) = inti(xcp(1))
!        blopun%valor(blopun%numero) = aux(1)
!      end if



print*,'Materials database'
!Open materials database
call fread(ide, '/Materials file/Open/materialsDB', matxml)
im = fopen(matxml)


print*,'Magnitudes'
!Magnitudes
call flist(ide, '/Properties/Materials/Materials', list)
conduc%numero = size(list,1)
do i = 1, size(list,1)
  conduc%referencias(i) = inti(list(i))
  call fread(ide, '/Properties/Materials/Materials/'//trim(list(i)), sval)
  call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity', list2)
  if (size(list2,1)==0) call error('readxml: missing electrical conductivity type for material')
  select case(trim(list2(1)))
      case('A constant')
         call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant', list3)
         if (size(list3,1)==0) call error('readxml: missing constant electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(im, '/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                 conduc%valorx(i))
                 conduc%valory(i) = conduc%valorx(i)
                 conduc%valorz(i) = conduc%valorx(i)
             case('Orthotropic')
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Electrical conductivity/A constant/Orthotropic/X Value',&
                 conduc%valorx(i))
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Electrical conductivity/A constant/Orthotropic/Y Value',&
                 conduc%valory(i))
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Electrical conductivity/A constant/Orthotropic/Z Value',&
                 conduc%valorz(i))
             case default; call error('readxml: Case not implemented.')
         end select
         conduc%iopcond(i) = 2
      case('A temperature dependant table')
         call flist(im,'/Materials database/Open/Materials/' &
             //trim(sval)//'/Electrical conductivity/A temperature dependant table', list3)
         if (size(list3,1)==0) call error('readxml: missing temperature dependant electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
             !utilizamos unha variable auxiliar (aux) allocatable para saber o n� de elementos
                call fread_alloc(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Electrical conductivity/A temperature dependant table/Isotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Electrical conductivity/A temperature dependant table/Isotropic/Values', conduc%valtabx(i,:))
                conduc%ntab(i) = size(aux,1)
                conduc%teta(i,1:conduc%ntab(i)) = aux
                conduc%valtaby(i,1:conduc%ntab(i)) = conduc%valtabx(i,1:conduc%ntab(i))
                conduc%valtabz(i,1:conduc%ntab(i)) = conduc%valtabx(i,1:conduc%ntab(i))
             case('Orthotropic')
                call fread_alloc(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Electrical conductivity/A temperature dependant table/Orthotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Electrical conductivity/A temperature dependant table/Orthotropic/X values', conduc%valtabx(i,:))
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Electrical conductivity/A temperature dependant table/Orthotropic/Y values', conduc%valtaby(i,:))
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Electrical conductivity/A temperature dependant table/Orthotropic/Z values', conduc%valtabz(i,:))
                conduc%ntab(i) = size(aux,1)
                conduc%teta(1,1:conduc%ntab(i)) = aux
             case default; call error('readxml: Case not implemented.')
         end select
         conduc%iopcond(i) = 3
         iopteta = 1
      case ('A function')
         !call fread(im, '/Materials database/Open/Materials/'//trim(sval)//&
         !    &'/Electrical conductivity/'//trim(list2(1)), tval)
         !fnum = function_number(tval, functions_cond)
         !if (fnum == 0) call error('readxml: unknown function: '//tval)
         !conduc%fun(i) = fnum
         conduc%etiqueta(i) = trim(sval)
         print*, trim(sval),conduc%etiqueta(i)
         conduc%iopcond(i) = 1
      case default; call error('readxml: Case not implemented.')
  end select
enddo

!Si iopcond = 3 hai q ler un ficheiro de temperaturas fichteta
if (iopteta == 1) then
    print*,'Temperature field'
    call fread(ide, '/Data/Temperature field for materials/Field', fichteta)
endif

call fclose(im)
call fclose(ide)

!Results (missing)
fichsol = 'fichpot'
fichElectricField = 'fichgradpot'
fichCurrentDensity = 'fichcurrentdensity'

!Quadrature options (missing)
iop = 2
iopf = 1

!Another data (missing)
iopej = 0

!fichvexac: usado solo si iopej != 0

! todos os exemplos estan asi
! van fixas
iopsl = 0
epscg = 1.d-50
nitcg = 1000

! ncaras nodc1 nodc2 nodc3 naristas nod1 nod2

! alojamiento de variables
! repetido: aqui y en endat3D.f

      allocate(ncaras(nri+inten%numero),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para ncaras'
      stop 1
      endif      
      
      allocate(nodc1(nri+inten%numero,ndcaras),&
     &          nodc2(nri+inten%numero,ndcaras),& 
     &          nodc3(nri+inten%numero,ndcaras),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nodc1,2 o 3'
      stop 1
      endif
      
      allocate(naristas(carcur%numero),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para naristas'
      stop 1
      endif
      
      allocate(nod1(carcur%numero,ndar),&
     &         nod2(carcur%numero,ndar),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nod1 o nod2'
      stop 1
      endif

end subroutine
