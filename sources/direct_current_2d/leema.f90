!------------------------------------------------------------------
! goal : subroutine for the reading of the mesh 
!------------------------------------------------------------------
!                                                                
!   IN:        fichma: mesh file                      
!                                                              
!   OUT:                                                      
!              nel   : number of elements in the mesh
!              nver  : number of vertices in the mesh 
!              mm    : connectivity matrix 
!              nra   : edges references array 
!              nrv   : vertex references array 
!              z     : coordinates array 
!              nsd   : domains array 
!------------------------------------------------------------------

subroutine leema(iformat)
                      
  use fich_electros
  use malla_2DP1
  use dcurrent_2D
 
  implicit none
  integer, intent(in) :: iformat
  integer             :: i,j,k
  
  print*,'Loading the mesh'
  
  select case(iformat)
  
    case(1)
  
      open(unit=10,file=fichma,form='formatted')
      rewind(10)
      read(10,*) nel,nver
      
      if(allocated(mm))deallocate(mm)
      allocate(mm(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array mm',nel
      stop 1
      endif

      if(allocated(nra))deallocate(nra)
      allocate(nra(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nra',nel
      stop 1
      endif

      if(allocated(nrv))deallocate(nrv)
      allocate(nrv(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nrv',nel
      stop 1
      endif

      if(allocated(nsd))deallocate(nsd)
      allocate(nsd(nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nsd',nel
      stop 1
      endif

      if(allocated(z))deallocate(z)
      allocate(z(2,nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array z',nver
      stop 1
      endif

      read(10,*)((mm(j,k),j=1,3),k=1,nel),((nra(j,k),j=1,3),k=1,nel), &
                 ((nrv(j,k),j=1,3),k=1,nel),((z(j,i),j=1,2),i=1,nver)
      read(10,*)(nsd(k),k=1,nel)
      close(unit=10)
    
    case(2)
   
      open(unit=10,file=fichma,form='unformatted')
      rewind(10)
      read(10) nel,nver
      
      if(allocated(mm))deallocate(mm)
      allocate(mm(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array mm',nel
      stop 1
      endif

      if(allocated(nra))deallocate(nra)
      allocate(nra(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nra',nel
      stop 1
      endif

      if(allocated(nrv))deallocate(nrv)
      allocate(nrv(3,nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nrv',nel
      stop 1
      endif

      if(allocated(nsd))deallocate(nsd)
      allocate(nsd(nel),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array nsd',nel
      stop 1
      endif

      if(allocated(z))deallocate(z)
      allocate(z(2,nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'Error while allocating array z',nver
      stop 1
      endif

      read(10)((mm(j,k),j=1,3),k=1,nel),((nra(j,k),j=1,3),k=1,nel), &
                 ((nrv(j,k),j=1,3),k=1,nel),((z(j,i),j=1,2),i=1,nver)
      read(10)(nsd(k),k=1,nel)
      close(unit=10)    
    
  end select

  return
end
