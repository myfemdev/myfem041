!-----------------------------------------------------------------------
! procedure for reading the solver variables
!-----------------------------------------------------------------------

  subroutine readxml()
  
  use module_SO_DEPENDANT
  use module_REPORT
  use module_xml_parser
!Solver modules
  use fich_electros3D
  use electros3D, DOUBLElocal1 => DOUBLE
  use cargavol, DOUBLElocal2 => DOUBLE
  use cargacur, DOUBLElocal3 => DOUBLE
  use cargapun, DOUBLElocal4 => DOUBLE
  use permitividad, DOUBLElocal5 => DOUBLE
  use bloqueo, DOUBLElocal6 => DOUBLE
  use derivados3D, DOUBLElocal7 => DOUBLE
  use auxiliar_cargas

  implicit none

  integer :: i, j, pos, ide, im, fnum
  real(DOUBLE) :: cval
  real(DOUBLE), dimension(:), allocatable :: xcp, aux
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

  ide = fopen()

!Mesh
  call fread(ide, '/Mesh/Open/Mesh file', fichma)

!Boundary Condicions
  print*,'Neumann'
!Neumann conditions
  iopneu = 0; iopneu1 = 0; iopneu2 = 0
  nrn = 0
  neuman%numero = 0
  call flist(ide, '/Boundary conditions/Neumann/Conditions/', list)
  do i = 1, size(list,1) !loop for defined Neumann BC's
     call flist(ide, '/Boundary conditions/Neumann/Conditions/'//trim(list(i)), list2)
     do j = 1, size(list2,1) !loop for data type for each BC
        select case(trim(list2(j)))
        case('A function')
        !References
           call fread_alloc(ide, '/Boundary conditions/Neumann/Conditions/'//trim(list(i))//&
                                 &'/A function/Surface references', refs, realloc=.true.)
           if (size(refs,1)>0) then
              iopneu = 1
              iopneu1 = 1 ! ok
              !Function
              call fread(ide, '/Boundary conditions/Neumann/Conditions/'//trim(list(i))//&
                              &'/A function/Function name', sval)
              pos = nrn + 1
              irefn(pos:pos+size(refs,1)-1) = int(refs)
              fnum = function_number(sval,functions)
              if (fnum == 0) call error('readxml: unknown function: '//sval)
              neu%fun(pos:pos+size(refs,1)-1) = fnum
              nrn = nrn + size(refs,1)
           else
              print * , 'Function Neumann B.C. with 0 references: skipping'
           endif
        case('A constant')
       !References
           call fread_alloc(ide, '/Boundary conditions/Neumann/Conditions/'//trim(list(i))//&
                                &'/A constant/Surface references', refs, realloc=.true.)
           if (size(refs,1)>0) then
              iopneu = 1
              iopneu2 = 1 ! ok
             !Constant value
              call fread(ide, '/Boundary conditions/Neumann/Conditions/'//trim(list(i))//&
                             &'/A constant/Constant value', cval)
              pos = neuman%numero + 1
              neuman%referencias(pos:pos+size(refs,1)-1) = int(refs)
              neuman%numero = neuman%numero + size(refs,1)
              neuman%valor(pos:pos+size(refs,1)-1) = cval
           else
              print * , 'Constant Neumann B.C. with 0 references: skipping'
           endif
        case default; call error('readxml: Case not implemented.')
        end select
     enddo
  enddo

  print*,'Dirichlet'
  !Potential (Dirichlet) conditions
  iopblo = 0; iopblo1 = 0; iopblo2 = 0; iopblo3 = 0
  nrd = 0
  blofron%numero = 0
  blopun%numero = 0
  call flist(ide, '/Boundary conditions/Dirichlet/Conditions', list)
  do i = 1, size(list,1) !loop for defined potential BC's
     call flist(ide, '/Boundary conditions/Dirichlet/Conditions/'//trim(list(i)), list2)
     do j = 1, size(list2,1) !loop for data type for each BC
        select case(trim(list2(j)))
        case('A function')
          !References
          call fread_alloc(ide, '/Boundary conditions/Dirichlet/Conditions/'//trim(list(i))//&
                               &'/A function/Surface references', refs, realloc=.true.)
          if (size(refs,1)>0) then
             iopblo = 1
             iopblo1 = 1 ! ok
            !Function
             call fread(ide, '/Boundary conditions/Dirichlet/Conditions/'//trim(list(i))//&
                            &'/A function/Function name', sval)
             pos = nrd + 1
             irefd(pos:pos+size(refs,1)-1) = int(refs)
             fnum = function_number(sval,functions)
             if (fnum == 0) call error('readxml: unknown function: '//sval)
             dir%fun(pos:pos+size(refs,1)-1) = fnum
             nrd = nrd + size(refs,1)
          else
             print * , 'Function Dirichlet B.C. with 0 references: skipping'
          endif
        case('A constant')
          !References
           call fread_alloc(ide, '/Boundary conditions/Dirichlet/Conditions/'//trim(list(i))//&
                                &'/A constant/Surface references', refs, realloc=.true.)
           if (size(refs,1)>0) then
              iopblo = 1
              iopblo2 = 1 ! ok
              !Constant value
              call fread(ide, '/Boundary conditions/Dirichlet/Conditions/'//trim(list(i))//&
                             &'/A constant/Constant value', cval)
              pos = blofron%numero + 1
              blofron%referencias(pos:pos+size(refs,1)-1) = int(refs)
              blofron%numero = blofron%numero + size(refs,1)
              blofron%valor(pos:pos+size(refs,1)-1) = cval
           else
              print * , 'Constant Dirichlet B.C. with 0 references: skipping'
           endif
!        case('Point')
!           iopblo3 = 1 ! ok
!          !References
!           call fread_alloc('/B.C./Define.../B.C. type/Potential/'//trim(list(i))//&
!                           &'/Point/Reference number(s)', refs, realloc=.true.)
!          !Constant value
!           call fread('/B.C./Define.../B.C. type/Potential/'//trim(list(i))//&
!                     &'/Point/Constant value', cval)
!           if (size(refs,1)>0)
!              iopblo3 = 1 ! ok
!              pos = blopun%numero + 1
!              blopun%referencias(pos:pos+size(refs,1)-1) = int(refs)
!              blopun%numero = blopun%numero + size(refs,1)
!              blopun%valor(pos:pos+size(refs,1)-1) = cval
!           else
!              print * , 'Dirichlet B.C. with 0 references: skipping'
!           endif
        case default; call error('readxml: Case not implemented.')
        end select
     enddo
  enddo

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
!        blopun%referencias(blopun%numero) = int(xcp(1))
!        blopun%valor(blopun%numero) = aux(1)
!      end if

!Sources

  print*,'Volume sources'
  !Volumic sources 
  iopvol = 0 ! 1 => hai volumic sources
  carvol%numero = 0
  call flist(ide, '/Sources/Volumetric/Volumetric sources', list)
  do i = 1, size(list,1) !loop for defined volumic sources
     call flist(ide, '/Sources/Volumetric/Volumetric sources/'//trim(list(i)), list2)
     if (size(list2,1)/=1) call error('readxml: Incorrect number of childs in volume source.')
     if (trim(list2(1)) == 'A function') then
    !References
        call fread_alloc(ide, '/Sources/Volumetric/Volumetric sources/'//trim(list(i))//&
                             &'/A function/Domain references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopvol = 1
         !Function
           call fread(ide, '/Sources/Volumetric/Volumetric sources/'//trim(list(i))//&
                          &'/A function/Function name', sval)
           pos = carvol%numero + 1
           carvol%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carvol%numero = carvol%numero + size(refs,1)
           carvol%valor(pos:pos+size(refs,1)-1) = 0.d0
           fnum = function_number(sval,functions)
           if (fnum == 0) call error('readxml: unknown function: '//sval)
           vol%fun(pos:pos+size(refs,1)-1) = fnum
           carvol%constante(pos:pos+size(refs,1)-1) = .FALSE.
        else
           print * , 'Function volume source with 0 references: skipping'
        endif
     elseif (trim(list2(1)) == 'A constant') then
       !References
        call fread_alloc(ide, '/Sources/Volumetric/Volumetric sources/'//trim(list(i))//&
                             &'/A constant/Domain references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopvol = 1
          !Constant value
           call fread(ide, '/Sources/Volumetric/Volumetric sources/'//trim(list(i))//&
                          &'/A constant/Constant value', cval)
           pos = carvol%numero + 1
           carvol%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carvol%numero = carvol%numero + size(refs,1)
           carvol%valor(pos:pos+size(refs,1)-1) = cval
           carvol%constante(pos:pos+size(refs,1)-1) = .TRUE.
        else
           print * , 'Constant volume source with 0 references: skipping'
        endif
     else
        call error('readxml: Incorrect volume source child: '//trim(list2(1))//'.')
     endif
  enddo

  print*,'Surface sources'
 !Surface sources
  iopsup = 0 ! 1 => hai surface sources
  carsup%numero = 0
  call flist(ide, '/Sources/Surface/Surface sources', list)
  do i = 1, size(list,1) !loop for defined surface sources
     call flist(ide, '/Sources/Surface/Surface sources/'//trim(list(i)), list2)
     if (size(list2,1)/=1) call error('readxml: Incorrect number of childs in surface source.')
     if (trim(list2(1)) == 'A function') then
     !References
        call fread_alloc(ide, '/Sources/Surface/Surface sources/'//trim(list(i))//&
                             &'/A function/Surface references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopsup = 1
          !Function
           call fread(ide, '/Sources/Surface/Surface sources/'//trim(list(i))//&
                          &'/A function/Function name', sval)
           pos = carsup%numero + 1
           carsup%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carsup%numero = carsup%numero + size(refs,1)
           carsup%valor(pos:pos+size(refs,1)-1) = 0.d0
           fnum = function_number(sval,functions)
           if (fnum == 0) call error('readxml: unknown function: '//sval)
           sup%fun(pos:pos+size(refs,1)-1) = fnum
           carsup%constante(pos:pos+size(refs,1)-1) = .FALSE.
        else
           print * , 'Function surface source with 0 references: skipping'
        endif
     elseif (trim(list2(1)) == 'A constant') then
       !References
        call fread_alloc(ide, '/Sources/Surface/Surface sources/'//trim(list(i))//&
                             &'/A constant/Surface references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopsup = 1
          !Constant value
           call fread(ide, '/Sources/Surface/Surface sources/'//trim(list(i))//&
                          &'/A constant/Constant value', cval)
           pos = carsup%numero + 1
           carsup%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carsup%numero = carsup%numero + size(refs,1)
           carsup%valor(pos:pos+size(refs,1)-1) = cval
           carsup%constante(pos:pos+size(refs,1)-1) = .TRUE.
        else
           print * , 'Constant surface source with 0 references: skipping'
        endif
     else
        call error('readxml: Incorrect surface source child: '//trim(list2(1))//'.')
     endif
  enddo

  print*,'Line sources'
 !Curvilinear sources
  iopcur = 0 ! 1 => hai line sources
  carcur%numero = 0
  call flist(ide, '/Sources/Line/Line sources', list)
  do i = 1, size(list,1) !loop for defined curvilinear sources
     call flist(ide, '/Sources/Line/Line sources/'//trim(list(i)), list2)
     if (size(list2,1)/=1) call error('readxml: Incorrect number of childs in line source.')
     if (trim(list2(1)) == 'A function') then
      !References
        call fread_alloc(ide, '/Sources/Line/Line sources/'//trim(list(i))//&
                             &'/A function/Line references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopcur = 1
          !Function
           call fread(ide, '/Sources/Line/Line sources/'//trim(list(i))//&
                          &'/A function/Function name', sval)
           pos = carcur%numero + 1
           carcur%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carcur%numero = carcur%numero + size(refs,1)
           carcur%valor(pos:pos+size(refs,1)-1) = 0.d0
           fnum = function_number(sval,functions)
           if (fnum == 0) call error('readxml: unknown function: '//sval)
           cur%fun(pos:pos+size(refs,1)-1) = fnum
           carcur%constante(pos:pos+size(refs,1)-1) = .FALSE.
        else
           print * , 'Function line source with 0 references: skipping'
        endif
     elseif (trim(list2(1)) == 'A constant') then
       !References
        call fread_alloc(ide, '/Sources/Line/Line sources/'//trim(list(i))//&
                             &'/A constant/Line references', refs, realloc=.true.)
        if (size(refs,1)>0) then
           iopcur = 1
          !Constant value
           call fread(ide, '/Sources/Line/Line sources/'//trim(list(i))//&
                          &'/A constant/Constant value', cval)
           pos = carcur%numero + 1
           carcur%referencias(pos:pos+size(refs,1)-1) = int(refs)
           carcur%numero = carcur%numero + size(refs,1)
           carcur%valor(pos:pos+size(refs,1)-1) = cval
           carcur%constante(pos:pos+size(refs,1)-1) = .TRUE.
        else
           print * , 'Constant line source with 0 references: skipping'
        endif
     else
        call error('readxml: Incorrect surface source child: '//trim(list2(1))//'.')
     endif
  enddo

  print*,'Point sources'
  !Point sources
  ioppun = 0
  ncarpun = 0
  call flist(ide, '/Sources/Point/Point sources', list)
  if (size(list,1) > 0) ioppun = 1
  do i = 1, size(list,1) !loop for defined point sources
     pos = ncarpun + 1
     call fread_alloc(ide, '/Sources/Point/Point sources/'//trim(list(i))//'/X coordinates',xcp, realloc=.true.)
     xcarpun(pos:pos+size(xcp,1)-1) = xcp
     call fread_alloc(ide, '/Sources/Point/Point sources/'//trim(list(i))//'/Y coordinates',xcp, realloc=.true.)
     ycarpun(pos:pos+size(xcp,1)-1) = xcp
     call fread_alloc(ide, '/Sources/Point/Point sources/'//trim(list(i))//'/Z coordinates',xcp, realloc=.true.)
     zcarpun(pos:pos+size(xcp,1)-1) = xcp
     call fread_alloc(ide, '/Sources/Point/Point sources/'//trim(list(i))//'/Values',xcp, realloc=.true.) !val
!para face ser se da un �nica valor na fonte para varios puntos
! alg�n outro sitio onde sexa necesario ?
! if (size(val,1) == 1) then extendemos val � lonxitude de xcp
     carpun(pos:pos+size(xcp,1)-1) = xcp
     ncarpun = ncarpun + size(xcp,1)
  enddo

  print*,'Materials database'
!Open materials database
  call fread(ide, '/Materials file/Open/materialsDB', matxml)
  im = fopen(matxml)

  print*,'Magnitudes'
 !Magnitudes
  call flist(ide, '/Properties/Materials/Materials', list)
  permirel%numero = size(list,1)
  do i = 1, size(list,1)
     permirel%referencias(i) = int(list(i))
     call fread(ide, '/Properties/Materials/Materials/'//trim(list(i)), sval)
     call flist(im, '/Materials database/Open/Materials/'//trim(sval)//'/Relative permittivity', list2)
     if (size(list2,1)==0) call error('readxml: missing relative permittivity type for material')
     select case(trim(list2(1)))
      case('A constant')
         call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Relative permittivity/A constant', list3)
         if (size(list3,1)==0) call error('readxml: missing constant relative permittivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(im, '/Materials database/Open/Materials/'//trim(sval)//'/Relative permittivity/A constant/Isotropic',&
                 permirel%valorx(i))
                 permirel%valory(i) = permirel%valorx(i)
                 permirel%valorz(i) = permirel%valorx(i)
             case('Orthotropic')
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Relative permittivity/A constant/Orthotropic/X Value',&
                 permirel%valorx(i))
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Relative permittivity/A constant/Orthotropic/Y Value',&
                 permirel%valory(i))
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Relative permittivity/A constant/Orthotropic/Z Value',&
                 permirel%valorz(i))
             case default; call error('readxml: Case not implemented.')
         end select
         permirel%iopermir(i) = 2
      case('A temperature dependant table')
         call flist(im,'/Materials database/Open/Materials/' &
             //trim(sval)//'/Relative permittivity/A temperature dependant table', list3)
         if (size(list3,1)==0) call error('readxml: missing temperature dependant relative permittivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
             !utilizamos unha variable auxiliar (aux) allocatable para saber o n� de elementos
                call fread_alloc(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Isotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Isotropic/Values', permirel%valtabx(i,:))
                permirel%ntab(i) = size(aux,1)
                permirel%teta(i,1:permirel%ntab(i)) = aux
                permirel%valtaby(i,1:permirel%ntab(i)) = permirel%valtabx(i,1:permirel%ntab(i))
                permirel%valtabz(i,1:permirel%ntab(i)) = permirel%valtabx(i,1:permirel%ntab(i))
             case('Orthotropic')
                call fread_alloc(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/X values', permirel%valtabx(i,:))
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/Y values', permirel%valtaby(i,:))
                call fread(im, '/Materials database/Open/Materials/'&
     &//trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/Z values', permirel%valtabz(i,:))
                permirel%ntab(i) = size(aux,1)
                permirel%teta(1,1:permirel%ntab(i)) = aux
             case default; call error('readxml: Case not implemented.')
         end select
         permirel%iopermir(i) = 3
         iopteta = 1
      case ('A function')
         !call fread(im, '/Materials database/Open/Materials/'//trim(sval)//&
         !    &'/Relative permittivity/'//trim(list2(1)), tval)
         !fnum = function_number(tval, functions_perm)
         !if (fnum == 0) call error('readxml: unknown function: '//tval)
         !permirel%fun(i) = fnum
         permirel%etiqueta(i) = trim(sval)
         permirel%iopermir(i) = 1
      case default; call error('readxml: Case not implemented.')
     end select
  enddo

!Se iopermir = 3 hai q ler un ficheiro de temperaturas fichteta
  if (iopteta == 1) then
     print*,'Temperature field'
     call fread(ide, '/Data/Temperature field for materials/Field', fichteta)
  endif

  call fclose(im)
  call fclose(ide)

 !Results (missing)
  fichsol = 'fichpot'
  fichgradsol = 'fichgradpot'

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

  allocate(ncaras(carsup%numero),stat=ierror)
  if (ierror.ne.0) then
     print*,'error: no se ha podido reservar memoria para ncaras'
     stop 1
  endif

  allocate(nodc1(carsup%numero,ndcaras),&
           nodc2(carsup%numero,ndcaras),&
           nodc3(carsup%numero,ndcaras),stat=ierror)
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
          nod2(carcur%numero,ndar),stat=ierror)
  if (ierror.ne.0) then
     print*,'error: no se ha podido reservar memoria para nod1 o nod2'
     stop 1
  endif
      
end subroutine
