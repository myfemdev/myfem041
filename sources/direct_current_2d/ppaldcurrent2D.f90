program ppaldcurrent2D

  use parametros_electros
  use dcurrent_2D
  use external_electros
  use fich_electros
  use malla_2DP1
  use module_writeVTU
  use module_fem_extract
  use derivados
  use module_readUNV
  use module_compiler_dependant
  use module_CONVERS
  use LIB_VTK_IO_READ
  
  implicit double precision (a-h,o-z)
  double precision, allocatable :: subz(:,:),subjc(:),evtun(:)
  integer, allocatable          :: submm(:,:),globv(:),globel(:),nn(:,:),nrc(:,:)
  integer                       :: DIM,LNN,LNV,LNE,LNF,nnod,npieces
  double precision              :: xnorexac,xnorerr,rel
  integer                       :: subnel,subnver      
  integer                       :: l,p,nverteta,iformat
  character(len=255)            :: cad
  real, allocatable             :: tetaux(:)
  integer                       :: i,istat,k
  
!---------------------------------------------------------------------------
!                            INPUT DATA                                    
!---------------------------------------------------------------------------

  if (command_argument_count() == 0) then
     call endat()
  else
     call readxml()
  end if

!---------------------------------------------------------------------------
!                     ELECTROMAGNETIC MESH READING                    
!---------------------------------------------------------------------------

  p = index(fichma, '.', back=.true.)
  if (p == 0) stop 'Mesh file has not extension: unable to identify mesh format'
  select case (lcase(fichma(p+1:len_trim(fichma))))
  case('mfm')
     iformat=1
     call leema(iformat)
  case('mum')
     iformat=2
     call leema(iformat)
  case('unv')
     call readUNV(fichma,nel,nnod,nver,DIM,LNN,LNV,LNE,LNF,nn,&
                  mm,nrc,nra,nrv,z,nsd)
  case default
     stop 'Unrecognized mesh file extension'
  end select
  call alloc_after_mesh()

!---------------------------------------------------------------------------
!                     TEMPERATURE READING
!---------------------------------------------------------------------------

  if (iopteta == 1) then !temperature must be read
     if (allocated(teta)) deallocate(teta) !deallocate teta
     p = index(fichteta, '.', back=.true.) !seach extension
     if  (p == 0) stop 'Error (temperature): file has not extension'
     select case (lcase(fichteta(p+1:len_trim(fichteta))))
     case('mff')
       open(unit=10,file=fichteta,form='formatted', position='rewind')
       read(10,*) nverteta
       if (nverteta /= nver) stop 'Error (temperature): nverteta /= nver'
       allocate(teta(nverteta))
       rewind(10)
       read(10,*) nverteta, (teta(i), i=1, nverteta)
       close(10)
     case('vtu')
        print*,'Reading a VTU binary file for temperature...'
        if (vtk_ini_xml_read('Binary',fichteta,'UnstructuredGrid',& 
             npieces) /=0) stop 'Error (temperature): unable open vtu file'
        if (npieces > 1) stop 'Error (temperature): too many pieces'
        if (vtk_var_xml_read('node', nnd, nco, 'Temperature', teta) /=0)&
            stop 'Error (temperature): unable to read pointdata'
        if (vtk_end_xml_read() /=0) stop 'Error (temperature): close'
     case default
        stop 'Unrecognized temp. file extension'
     end select
  end if

!---------------------------------------------------------------------------
!                            COMPUTATIONS                                
!---------------------------------------------------------------------------

  call cmua(mua,mm,nel,nver)

  if(allocated(c))deallocate(c)
  allocate(c(mua(nver+1)),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array c',mua(nver+1)
     stop 1
  endif

  call dc2D() 

! COMPUTATION OF THE ELECTRIC FIELD 
  call ef()
! COMPUTATION OF THE CURRENT DENSITY
  call cd()  
     
!---------------------------------------------------------------------------
!                            RESULTS OUTPUT                             
!---------------------------------------------------------------------------

  call wrtcmp(sol,10,fichsol)

  call writeVTU(nel,nver,mm,z,'triangle',sol,'Potential (V)','scalar',&
                'node',trim(fichsol)//'.vtu')

  call wrtcmpv(e,10,fichElectricField)

  if(allocated(evtu))deallocate(evtu)
  allocate(evtu(2*nel),STAT=istat)
  if (istat.ne.0) stop 'Error al alojar evtu en ef'
  evtu(1:nel*2:2)=e(1,1:nel)
  evtu(2:nel*2:2)=e(2,1:nel)
  call cell2node(nver, mm, evtu, evtun)
  call writeVTU(nel,nver,mm,z,'triangle',evtun,'Electric field (N/C)','vector','node','En.vtu')

  call wrtcmpv(jc,10,fichCurrentDensity)

  evtu(1:nel*2:2)=jc(1,1:nel)
  evtu(2:nel*2:2)=jc(2,1:nel)
  do l = 1, conduc%numero
!       - extracción de la submalla (submm, subz)
     call extract_mesh(nver, mm, z, nsd, [conduc%referencias(l)],submm, subz, globv, globel)
     subnel = size(submm,2); subnver = size(subz,2)
!       - extracción del subcampo (evtu)
     call extract_field(globv, globel, evtu, subjc, 'cell', 2)
!       - paso a nodo (reutilizamos evtun)
     call cell2node(subnver, submm, subjc, evtun)
!       - escritura por nodos
     write(cad,*) conduc%referencias(l)
  call writeVTU(subnel,subnver,submm,subz,'triangle',evtun,'Current density (A/m^2)',&
                'vector','node','Jcn'//trim(adjustl(cad))//'.vtu')
  enddo


  deallocate(evtu,STAT=istat)
  if (istat.ne.0) stop 'Error while deallocating in ppaldcurrent2D'    
  deallocate(e,STAT=istat)
  if (istat.ne.0) stop 'Error while deallocating in ppaldcurrent2D'
  deallocate(jc,STAT=istat)
  if (istat.ne.0) stop 'Error while deallocating in ppaldcurrent2D'

! CALCULATION OF THE EUCLIDEAN NORM OF THE RELATIVE ERROR 
  if(allocated(vexac))deallocate(vexac)
  allocate(vexac(nver),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array vexac',nver
     stop 1
  endif

  if(allocated(err))deallocate(err)
  allocate(err(nver),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array err',nver
     stop 1
  endif

!  if (iopej.eq.0) then
!
!     do i=1,nver
!        vexac(i) = fexac(z(1,i),z(2,i))
!        err(i)	 = dabs(vexac(i)-sol(i))
!     enddo
!     call norl22d(nel,mm,z,vexac,xnorexac)
!     call norl22d(nel,mm,z,err,xnorerr)
!     print*,'xnorerr=',xnorerr
!     print*,'xnorexac=',xnorexac
!     rel = xnorerr/xnorexac
!     print*,'error=',rel
!     print*,'Relative error %',100*rel
!     
!  endif

! MATLAB FILES

  open(11,file='p.dat',form='formatted')
  do i=1,nver
     write(11,*)z(1,i),z(2,i)
  end do

  open(11,file='t.dat',form='formatted')
  do k=1,nel
     write(11,*)mm(1,k),mm(2,k),mm(3,k),nsd(k)
  end do
  close(11)
  open(11,file='u.dat',form='formatted')
  do i=1,nver
     write(11,*)sol(i)
  end do
  close(11)

  stop

end

