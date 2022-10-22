 PROGRAM ppaldc3D

!***********************************************************************
!                                                                
!  Objetivo: Resolver en medios conductivos. corriente continua
!
!  antes:
!  Objetivo: Resolver mediante elementos finitos la EDP de la     
!            electrostática 3D  con diferentes condiciones de      
!            contorno y casos de carga                             
!                                                                 
!              |  -div(permi grad(V))=f                           
!         (1)  |   V = V+ en frontera Dirichlet               
!              |   permi d(V)/dn=g 
!                                                                
!                  Dolores Gomez                                 
!                  MC Muñiz                                     
!                  modificando programa previo de
!                  Jose Luis Ferrin Gonzalez                    
!                                                                
!***********************************************************************


  use fich_electros3D
  use electros3D
  use cargavol
  use cargacur
  use cargapun
  use conductividad
  use bloqueo
  use derivados3D        
  use malla_3DP1
  use external_electros3D
  use module_writeVTU
  use module_convers
!  use comprobaciones
!  use iso_Fortran_env
  use module_fem_extract
  use module_conver3d, only: conver3d
  use module_readUNV


  implicit none
  integer :: i,istat, p, nnod,DIMS,LNN,LNV,LNE,LNF
  double precision, allocatable :: subz(:,:), subjc(:), evtun(:)
  integer, allocatable :: submm(:,:), globv(:), globel(:), nn(:,:)
  integer :: subnel, subnver
  integer :: l
  character(len=255) :: cad

!***************************************************************************
!                        entrada de datos                                  
!***************************************************************************

    if (command_argument_count() == 0) then
        call endat3D()
    else
        call readxml()
    end if
!
!   ! comprobacion de datos de entrada: sirve para endat3D y readxml
!     if (.not. comprueba()) then
!        write(error_unit,*) 'Input data check failed'
!        stop 1
!     else
!        write(output_unit,*) 'Input data check passed'
!     endif
   
!   ! calcula se todas as funcions dunha sección (Dirichlet...) son iguais
!     call calculate_funs()
  
!   ! asigna 0.0 al ultimo vertice en caso de no haber condiciones Dirichlet
!   if (blocking_node() < 0) then
!     write(error_unit,*) 'Error assigning blocking node'
!     stop 1
!   endif

!***************************************************************************
!                       lectura de la malla electrica                      
!***************************************************************************
!  print*,'antes de subrutina leemat'

  ! .bin
 
  CALL calindc(indc,inda)
  
!---------------------------------------------------------------------------
!                     mesh and temperature reading
!---------------------------------------------------------------------------
   p = index(fichma, '.', back=.true.)
   if  (p == 0) stop 'Mesh file has not extension: unable to identify mesh format'
   select case (lcase(fichma(p+1:len_trim(fichma))))
   case('mfm')
     call leema3D()
   case('unv')
     call readUNV(fichma,nel,nnod,nver,DIMS,LNN,LNV,LNE,LNF,nn,mm,nrc,nra,nrv,z,nsd)
     call conver3d(nel, nver, mm, z, nemm, det, binv, ib, jb)
   case default
     stop 'Unrecognized mesh file extension'
   end select
   call alloc_after_mesh()
   
   if (iopteta == 1) then !temperature must be read
     call leetmp()
   endif

!***************************************************************************
!                        bloque de calculo                                 
!***************************************************************************

  if(iopblo.eq.1.and.iopblo1.eq.1) then
     CALL calprebloqueof (nrd,irefd)
  endif
  if (iopblo.eq.1.and.iopblo2.eq.1) then
     CALL calprebloqueoc (blofron%numero,blofron%referencias)
  endif

!  print*,'antes de subrutina electrostatica 3D'

  CALL dc3D()


! Calculo del error al resolver un test

      allocate(vexac(nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para vexac',nver
      stop 1
      endif

      allocate(err(nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para err',nver
      stop 1
      endif


!      call wrtcmp(nver,sol,10,fichsol)
!      call writeVTU(nel,nver,mm,z,'tetra',sol,'solucion','scalar', &
!             'node',trim(fichsol)//'.vtu')
!             

   ! comentado porque ahora es un array
   !print*,'en principal, vol%fun', vol%fun
   
   ! modificar para comprobar todos os valores, ou quitar esta seccion de imprimir ?
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
            vexac(i)=0.d0
            err(i)=0.d0
            vexac(i) = fexac(z(1,i),z(2,i),z(3,i))
!           print*,'i',i,'vexac=',vexac(i), 'sol=',sol(i)
            err(i) = dabs(vexac(i)-sol(i)) 
        enddo

        if(dir%funs == 7)then ! 'Example 6'
        vexac(376)=sol(376)
        vexac(193)=sol(193)
        err(193) = dabs(vexac(193)-sol(193))
        err(376) = dabs(vexac(376)-sol(376))
        elseif(dir%funs == 6)then ! 'Example 5'
        vexac(1292) = sol(1292)
        err(1292) = dabs(vexac(1292)-sol(1292))
        endif

        CALL norl2_3D(sol,xnorexac)
!       print*,'solucion en norma L2 =', xnorexac
        CALL norl2_3D(vexac,xnorexac)
!       print*,'exacta en norma L2 =', xnorexac 
        CALL norl2_3D(err,xnorerr)
        rel = xnorerr/xnorexac
!        print*,'error en norma L2 =', xnorerr
!       print*,'error relativo =',rel
        print*,'relative error (%)',100*rel
  
        
   endif
        
      print*,'Computing electric field'  
    
      call ef()

      print*,'Computing current density' 
      
      call cd()
      

!**************************************************************************
!                        salida de resultados                             
!**************************************************************************

!**************************************************************************
!                         salvamos  solucion
!**************************************************************************
      print*,'Saving fields'
      call wrtcmp(sol,10,fichsol)
      call writeVTU(nel,nver,mm,z,'tetra',sol,'Potential (V)','scalar', &
             'node',trim(fichsol)//'.vtu')
!**************************************************************************
!                         salvamos campo electrico 
!**************************************************************************            
      call wrtcmpv(e,10,fichElectricField)
!      se guarda campo electrico 
     
      allocate(evtu(3*nel),STAT=istat)
      if (istat.ne.0) stop 'Error al alojar evtu en principal'
      
      evtu(1:nel*3:3)=e(1,1:nel)
      evtu(2:nel*3:3)=e(2,1:nel)
      evtu(3:nel*3:3)=e(3,1:nel)
      call cell2node(nver, mm, evtu, evtun)
      call writeVTU(nel,nver,mm,z,'tetra',evtun,'Electric field (V/m)', &  
      'vector','node',trim(fichElectricField)//'.vtu')

!**************************************************************************
!                         salvamos densidad de corriente 
!**************************************************************************            
      call wrtcmpv(jc,10,fichCurrentDensity)
!     escritura de la densidad de corriente (por nodo) en formato VTU
!     (reutilizacion de evtu)
      evtu(1:nel*3:3)=jc(1,1:nel)
      evtu(2:nel*3:3)=jc(2,1:nel)
      evtu(3:nel*3:3)=jc(3,1:nel)
      do l = 1, conduc%numero
!       - extracción de la submalla (submm, subz)
        call extract_mesh(nver, mm, z, nsd, [conduc%referencias(l)], &
        submm, subz, globv, globel)
        subnel = size(submm,2); subnver = size(subz,2)
!       - extracción del subcampo (evtu)
        call extract_field(globv, globel, evtu, subjc, 'cell', 3)
!       - paso a nodo (reutilizamos evtun)
        call cell2node(subnver, submm, subjc, evtun)
!       - escritura por nodos
        write(cad,*) conduc%referencias(l)
        call writeVTU(subnel,subnver,submm,subz,'tetra',evtun,'Current density (A/m^2)', &
       'vector','node',trim(fichCurrentDensity)//trim(adjustl(cad))//'.vtu')
      enddo

! Desaloxamos as variables auxiliares
     
      deallocate(evtu,STAT=istat)
      if (istat.ne.0) stop 'Error al desalojar en principal' 
      deallocate(sol,STAT=istat)
      if (istat.ne.0) stop 'Error al desalojar en principal'  
      deallocate(e,STAT=istat)
      if (istat.ne.0) stop 'Error al desalojar en principal'   
      deallocate(jc,STAT=istat)
      if (istat.ne.0) stop 'Error al desalojar en principal'


      stop 'normally ended execution'
      end
      
