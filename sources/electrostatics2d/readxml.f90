!-----------------------------------------------------------------------
!              procedure for reading the solver variables             
!-----------------------------------------------------------------------

subroutine readxml()

use module_SO_DEPENDANT
use module_REPORT
use module_xml_parser
!Solver modules
use fich_electros
use electros_2D
use cargavol
use cargacur
use cargapun
use permitividad
use bloqueo
use derivados

implicit none

integer,parameter                                 :: ucs4 = selected_char_kind('ISO_10646')
integer                                           :: i, j, pos, id, im
real(DOUBLE)                                      :: cval
real(DOUBLE),dimension(:),allocatable             :: xcp, aux
character(len=MAXPATH)                            :: matxml, sval
character(len=MAXPATH), dimension(:), allocatable :: list, list2, list3, refs 

character(len=9)  :: dir0 = 'Dirichlet'
character(len=7)  :: neu0 = 'Neumann'
character(len=10) :: surf0 = 'Volumetric'
character(len=7)  :: line0 = 'Surface'
character(len=4)  :: poin0 = 'Line'
character(len=23) :: temp0 = '/Data/Temperature/Field'

call set_SO()
call set_report_level(REPORT_STDOUT)

id = fopen()

!Mesh
call fread(id, '/Mesh/Open/Mesh file', fichma)

!Boundary Condicions

!Neumann conditions
iopneu = 0; iopinneu1 = 0; iopinneu2 = 0
nrn = 0
neuman%numero = 0
call flist(id, '%Boundary conditions%'//neu0//'%Conditions', list)
do i = 1, size(list,1) !loop for defined Neumann BC's
  call flist(id, '%Boundary conditions%'//neu0//'%Conditions%'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(id, '%Boundary conditions%'//neu0//'%Conditions%'//trim(list(i))//&
      &'%A function%Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopneu = 1
        iopinneu1 = 1
        call fread(id,'%Boundary conditions%'//neu0//'%Conditions%'//trim(list(i))//&
        &'%A function%Function name',sval)
        NeuBC_Func = trim(sval)
        pos = nrn + 1
        irefn(pos:pos+size(refs,1)-1) = int(refs)
        nrn = nrn + size(refs,1)
      else
        print * , 'Function Neumann B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(id, '%Boundary conditions%'//neu0//'%Conditions%'//trim(list(i))//&
      &'%A constant%Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopneu = 1
        iopinneu2 = 1
        pos = neuman%numero + 1
        neuman%referencias(pos:pos+size(refs,1)-1) = int(refs)
        neuman%numero = neuman%numero + size(refs,1)
        !Constant value
        call fread(id, '%Boundary conditions%'//neu0//'%Conditions%'//trim(list(i))//&
        &'%A constant%Constant value', cval)
        neuman%valor(pos:pos+size(refs,1)-1) = cval
      else
        print * , 'Constant Neumann B.C. with 0 references: skipping'
      endif
    case default; call error('readxml: Case not implemented.')      
    end select
  enddo
enddo    

!Potential (Dirichlet) conditions
iopblo = 0; iopblo1 = 0; iopblo2 = 0; iopblo3 = 0
nrd = 0
blofron%numero = 0
blopun%numero = 0
call flist(id, '/Boundary conditions/'//dir0//'/Conditions', list)
do i = 1, size(list,1) !loop for defined potential BC's
  call flist(id, '/Boundary conditions/'//dir0//'/Conditions/'//trim(list(i)), list2)
  do j = 1, size(list2,1) !loop for data type for each BC 
    select case(trim(list2(j)))
    case('A function')
      !References
      call fread_alloc(id, '/Boundary conditions/'//dir0//'/Conditions/'//trim(list(i))//&
      &'/A function/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopblo = 1
        iopblo1 = 1
        call fread(id,'/Boundary conditions/'//dir0//'/Conditions/'//trim(list(i))//&
      &'/A function/Function name',sval)
        DirBC_Func = trim(sval)
        pos = nrd + 1
        irefd(pos:pos+size(refs,1)-1) = int(refs)
        nrd = nrd + size(refs,1)
      else
        print * , 'Function Dirichlet B.C. with 0 references: skipping'
      endif
    case('A constant')
      !References
      call fread_alloc(id, '/Boundary conditions/'//dir0//'/Conditions/'//trim(list(i))//&
      &'/A constant/Line references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        iopblo = 1
        iopblo2 = 1
        pos = blofron%numero + 1
        blofron%referencias(pos:pos+size(refs,1)-1) = int(refs)
        blofron%numero = blofron%numero + size(refs,1)
        !Constant value
        call fread(id, '/Boundary conditions/'//dir0//'/Conditions/'//trim(list(i))//&
        &'/A constant/Constant value', cval)
        blofron%valor(pos:pos+size(refs,1)-1) = cval
      else
        print * , 'Constant Dirichlet B.C. with 0 references: skipping'
      endif
    case default; call error('readxml: Case not implemented.')
    end select
  enddo
enddo

!Sources

!Volumic sources
iopvol = 0; iopinvol = 0
carvol%numero = 0
call flist(id, '%Sources%'//surf0//'%Volumetric sources ', list)
do i = 1, size(list,1) !loop for defined volumic sources
  !References
  call fread_alloc(id, '%Sources%'//surf0//'%Volumetric sources%'//trim(list(i))//&
  &'%Volumetric references', refs, realloc=.true.)
  if (size(refs,1)>0) then
    iopvol = 1
    iopinvol = 2 ! tablero
    pos = carvol%numero + 1
    carvol%referencias(pos:pos+size(refs,1)-1) = int(refs)
    carvol%numero = carvol%numero + size(refs,1)
    !Constant value
    call fread(id, '%Sources%'//surf0//'%Volumetric sources%'//trim(list(i))//&
        &'%Constant value', cval)
    carvol%valor(pos:pos+size(refs,1)-1) = cval
  else
    print * , 'Constant surface source with 0 references: skipping'
  endif
enddo

!Curvilinear sources
iopcur = 0; iopincur = 0
carcur%numero = 0
call flist(id, '%Sources%'//line0//'%Surface sources', list)
do i = 1, size(list,1) !loop for defined curvilinear sources
  !References
  call fread_alloc(id, '%Sources%'//line0//'%Surface sources%'//trim(list(i))//&
  &'%Surface references', refs, realloc=.true.)
  if (size(refs,1)>0) then
    iopcur = 1
    iopincur = 2 ! tablero
    pos = carcur%numero + 1
    carcur%referencias(pos:pos+size(refs,1)-1) = int(refs)
    carcur%numero = carcur%numero + size(refs,1)
    !Constant value
    call fread(id, '%Sources%'//line0//'%Surface sources%'//trim(list(i))//&
        &'%Constant value', cval)
    carcur%valor(pos:pos+size(refs,1)-1) = cval
  else
    print * , 'Constant line source with 0 references: skipping'
  endif
enddo

!Point sources
ioppun = 0
ncarpun = 0
call flist(id, '/Sources/'//poin0//'/Line sources', list)
if (size(list,1) > 0) ioppun = 1
do i = 1, size(list,1) !loop for defined point sources
  pos = ncarpun + 1
  call fread_alloc(id, '/Sources/'//poin0//'/Line sources/'//trim(list(i))//'/X coordinates',xcp, realloc=.true.)
  xcarpun(pos:pos+size(xcp,1)-1) = xcp
  call fread_alloc(id, '/Sources/'//poin0//'/Line sources/'//trim(list(i))//'/Y coordinates',xcp, realloc=.true.)  
  ycarpun(pos:pos+size(xcp,1)-1) = xcp
  call fread_alloc(id, '/Sources/'//poin0//'/Line sources/'//trim(list(i))//'/Values',xcp, realloc=.true.) !val 
  carpun(pos:pos+size(xcp,1)-1) = xcp
  ncarpun = ncarpun + size(xcp,1)
enddo

!Open materials database
call fread(id, '/Materials file/Open/materialsDB', matxml)
im = fopen(matxml)

!Magnitudes
call flist(id, '/Properties/Materials/Materials', list)
permirel%numero = size(list,1)
do i = 1, size(list,1)
  permirel%referencias(i) = int(list(i))
  call fread(id, '/Properties/Materials/Materials/'//trim(list(i)), sval)
  call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Relative permittivity', list2)
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
             case('Orthotropic')
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Relative permittivity/A constant/Orthotropic/X Value',&
                 permirel%valorx(i))    
                 call fread(im, '/Materials database/Open/Materials/' &
                     //trim(sval)//'/Relative permittivity/A constant/Orthotropic/Y Value',&
                 permirel%valory(i))    
             case default; call error('readxml: Case not implemented.')
         end select
         permirel%iopermir(i) = 2  
      case('A temperature dependant table')
         call flist(im,'/Materials database/Open/Materials/' &
             //trim(sval)//'/Relative permittivity/A temperature dependant table', list3)
         if (size(list3,1)==0) call error('readxml: missing temperature dependant relative permittivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
             !utilizamos unha variable auxiliar (aux) allocatable para saber o nº de elementos
                call fread_alloc(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Isotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Isotropic/Values', permirel%valtabx(i,:))
                permirel%ntab(i) = size(aux,1)
                permirel%teta(i,1:permirel%ntab(i)) = aux
                permirel%valtaby(i,1:permirel%ntab(i)) = permirel%valtabx(i,1:permirel%ntab(i))
             case('Orthotropic')
                call fread_alloc(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/Temperatures', aux, realloc=.true.)
                call fread(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/X values', permirel%valtabx(i,:))
                call fread(im, '/Materials database/Open/Materials/' &
                //trim(sval)//'/Relative permittivity/A temperature dependant table/Orthotropic/Y values', permirel%valtaby(i,:))
                permirel%ntab(i) = size(aux,1)
                permirel%teta(1,1:permirel%ntab(i)) = aux
             case default; call error('readxml: Case not implemented.')
         end select
         permirel%iopermir(i) = 3
         iopteta = 1
      case('A function')
         permirel%etiqueta = trim(sval)
         permirel%iopermir(i) = 1
  case default; call error('readxml: Case not implemented.')
  end select
enddo

!Si iopermir = 3 hay que leer un fichero de temperaturas fichteta
if (iopteta == 1) then
    call fread(id, temp0, fichteta)
endif


!Results (missing)
fichsol = 'fichpot'
fichElectricField = 'fiche'

!Quadrature options (missing)
iop = 3
iopf = 1

!Another data (missing)
iopej = 0

! Chequeo del tipo de condiciones de contorno
if(iopblo.eq.1.and.(nrd.gt.0.or.blofron%numero.gt.0)) then
  ichneu=0
else
  ichneu=1
endif

call fclose(im)
call fclose(id)

end subroutine
