      subroutine leema3D()
 
!***************************************************************************
! objetivo : subrutina para la lectura de la malla 3D               
!***************************************************************************
!                                                                
!   IN:        fichma: fichero de la malla                                                                                                  *
!   OUT:                                                         
!              nel   : numero de elementos de la malla           
!              nver  : numero de vertices de la malla            
!              mm    : tablero de numeracion                     
!              nra   : tablero de referencias de las aristas     
!              nrv   : tablero de referencias de los vertice    
!              z     : tablero de coordenadas                   
!              nsd   : tablero de dominios                       
!***************************************************************************
                       
  use module_compiler_dependant, only: iostat_end
  use fich_electros3D,  only: fichma
  use electros3D
  use malla_3DP1
  use bloqueo
  use derivados3D
  use module_conver3d, only: conver3d
  
  implicit none
  integer :: i, j, k, ios
!***************************************************************************
!                lectura de la malla                                 
!***************************************************************************
 
!      print*,'lectura de la malla'
      !open(unit=10,file=fichma,form='formatted')
         open(unit=10, file=fichma, form='formatted')!, recl=2092290)

      rewind(10)
       ! read(10,*) nel,nver,nemm
      read(10,*) nel,nver


!
!     definimos allocatables para gestion dinamica de memoria
!

!      print*,'dimensionamiento del tablero mm, nel=',nel

      allocate(mm(4,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para mm',nel
      stop 1
      endif


      allocate(nra(6,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nra',nel
      stop 1
      endif

      allocate(nrv(4,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nrv',nel
      stop 1
      endif
      
      allocate(nrc(4,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nrc',nel
      stop 1
      endif

      allocate(nsd(nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nsd',nel
      stop 1
      endif
      
      allocate(z(3,nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para z',nver
      stop 1
      endif

     print*,'Number of mesh elements      :',nel
     print*,'Number of degrees of freedom :',nver
     read(10,*)   ((mm(i,j),i=1,4),j=1,nel), &
                ((nrc(i,j),i=1,4),j=1,nel),&
                ((nra(i,j),i=1,6),j=1,nel),&
                ((nrv(i,j),i=1,4),j=1,nel),&
                ((z(i,j),i=1,3),j=1,nver)                                   
     read(10,*)    (nsd(k),k=1,nel) 
     read(10, *, iostat=ios) nemm
     if (ios == iostat_end) then
       call conver3d(nel, nver, mm, z, nemm, det, binv, ib, jb)
       !write new info into the meshfile
       backspace(unit=10)
       write(10,*)nemm
       write(10,*)(det(k),k=1,nel),                      &
                (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
                (ib(i),i=1,nver+1),(jb(i),i=1,nemm)    
     else
       if(allocated(det)) deallocate(det);  allocate(det(nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate det'
       if(allocated(binv)) deallocate(binv); allocate(binv(3,3,nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate binv'
       if(allocated(ib)) deallocate(ib);   allocate(ib(nver+1), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate ib'
       if(allocated(jb)) deallocate(jb);   allocate(jb(nemm), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate jb'
       read(10,*)(det(k),k=1,nel),                      &
               (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
               (ib(i),i=1,nver+1),(jb(i),i=1,nemm)
     end if
     close(10)

     end subroutine
