!---------------------------------------------------------------------------
!                                                                
!  GOAL : Solve, by means of finite elements, the electrostatics 3D
!         PDE with different boundary conditions and charges
!                                                                 
!              |  -div(permi grad(V))=f                           
!         (1)  |   V = V+ on Dirichlet boundary
!              |   permi d(V)/dn=g 
!                                                                
!                  Dolores Gomez                                 
!                  MC Muñiz                                     
!                  Jose Luis Ferrin Gonzalez                    
!                                                                
!---------------------------------------------------------------------------

 program ppalelectros3D


  use fich_electros3D
  use electros3D
  use cargavol
  use cargacur
  use cargapun
  use permitividad
  use bloqueo
  use derivados3D        
  use malla_3DP1
  use external_electros3D
  use module_writeVTU
  use comprobaciones
  use module_convers
  use module_fem_extract
  use module_conver3d, only: conver3d
  use LIB_VTK_IO_READ
  use module_readUNV
  use module_compiler_dependant

  implicit none
  integer :: i,istat, p, nnod,DIMS,LNN,LNV,LNE,LNF,nnd,nco,npieces,nverteta,iformat
  integer, allocatable :: nn(:,:)
  real(real64), allocatable :: evtun(:)   

!---------------------------------------------------------------------------
!                            INPUT DATA                                    
!---------------------------------------------------------------------------

   if (command_argument_count() == 0) then
       call endat3D()
   else
       call readxml()
   end if

! INPUT DATA VERIFICATION, FOR ENDAT & READXML  
   if (.not. comprueba()) then
      write(error_unit,*) 'Input data check failed'
      stop 1
   else
      write(output_unit,*) 'Input data check passed'
   endif
   
   call calculate_funs()

! 0.0 IS ASSIGNED TO THE LAST VERTEX IN CASE OF NOT HAVING DIRICHLET CONDITIONS  
   if (blocking_node() < 0) then
      write(error_unit,*) 'Error assigning blocking node'
      stop 1
   endif 
   
!---------------------------------------------------------------------------
!                     ELECTROMAGNETIC MESH READING                    
!---------------------------------------------------------------------------
   call calindc(indc,inda)

   p = index(fichma, '.', back=.true.)
   if  (p == 0) stop 'Mesh file has not extension: unable to identify mesh format'
   select case (lcase(fichma(p+1:len_trim(fichma))))
   case('mfm')
     iformat=1
     call leema3D(iformat)
   case('mum')
     iformat=2
     call leema3D(iformat)
   case('unv')
     call readUNV(fichma,nel,nnod,nver,dims,LNN,LNV,LNE,LNF,nn,mm,nrc,nra,nrv,z,nsd)
     call conver3d(nel, nver, mm, z, nemm, det, binv, ib, jb)
   case default
     stop 'Unrecognized mesh file extension'
   end select
   call alloc_after_mesh()
   
!---------------------------------------------------------------------------
!                     TEMPERATURE READING                    
!---------------------------------------------------------------------------
   if (iopteta == 1) call leetmp()

!---------------------------------------------------------------------------
!                            COMPUTATIONS                                
!---------------------------------------------------------------------------
   if (iopblo.eq.1.and.iopblo1.eq.1) then
      call calprebloqueof(nrd,irefd)
   endif
   if (iopblo.eq.1.and.iopblo2.eq.1) then
      call calprebloqueoc(blofron%numero,blofron%referencias)
   endif

   call electrostatica3D()
   
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

!      call wrtcmp(nver,sol,10,fichsol)
!      call writeVTU(nel,nver,mm,z,'tetra',sol,'solucion','scalar', &
!             'node',trim(fichsol)//'.vtu')

! -1: mixed functions
! 0: no data
! 1: User defined / Function defined by user
! ...
   if (dir%funs > 1.or.&
       neu%funs > 1.or.&
       vol%funs > 1.or.&
       sup%funs > 1.or.&
       cur%funs > 1) then
      
      do i=1,nver
         vexac(i) = fexac(z(1,i),z(2,i),z(3,i))
         err(i)   = dabs(vexac(i)-sol(i)) 
      enddo

      if (dir%funs == 7) then ! 'Example 6'
         vexac(376) = sol(376)
         vexac(193) = sol(193)
         err(193)   = dabs(vexac(193)-sol(193))
         err(376)   = dabs(vexac(376)-sol(376))
      elseif (dir%funs == 6) then ! 'Example 5'
         vexac(1292) = sol(1292)
         err(1292)   = dabs(vexac(1292)-sol(1292))
      endif

      call norl2_3D(sol,xnorexac)
      call norl2_3D(vexac,xnorexac)
      call norl2_3D(err,xnorerr)
      rel = xnorerr/xnorexac
      print*,'Relative error (%)',100*rel
        
   endif

! COMPUTATION OF THE ELECTRIC FIELD 
   call ef()    

!---------------------------------------------------------------------------
!                            RESULTS OUTPUT                             
!---------------------------------------------------------------------------
   call wrtcmp(nver,sol,10,fichsol)

   call writeVTU(nel,nver,mm,z,'tetra',sol,'Potential (V)','scalar', &
                                  'node',trim(fichsol)//'.vtu')

   call wrtcmpv(nel,e,10,fichgradsol)

   if(allocated(evtu))deallocate(evtu)
   allocate(evtu(3*nel),STAT=istat)
   if (istat.ne.0) stop 'Error while allocating evtu in principal'
      
   evtu(1:nel*3:3)=e(1,1:nel)
   evtu(2:nel*3:3)=e(2,1:nel)
   evtu(3:nel*3:3)=e(3,1:nel)
   call cell2node(nver, mm, evtu, evtun)
   call writeVTU(nel,nver,mm,z,'tetra',evtun,'Electric field (V/m)',&
                  'vector','node',trim(fichgradsol)//'.vtu')
     
   deallocate(evtu,STAT=istat)
   if (istat.ne.0) stop 'Error while deallocating in principal' 
   deallocate(sol,STAT=istat)
   if (istat.ne.0) stop 'Error while deallocating in principal' 
   deallocate(e,STAT=istat)
   if (istat.ne.0) stop 'Error while deallocating in principal' 
   
   stop 'End of the execution'

 end
      
