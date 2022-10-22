subroutine alloc_after_mesh()
!Differents arrays are allocated after the lecture of the mesh

  use fich_electros
  use malla_2DP1
  use electros_2D
  use postpro
  
  implicit none
  integer             :: i,nverteta
  
      allocate(mua(nver+1),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: mua cannot be allocated'
        stop 1
      endif
      
!
      allocate(b(nver),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: b cannot be allocated'
        stop 1
      endif
!

      allocate(sol(nver),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: sol cannot be allocated'
        stop 1
      endif

           
      allocate(iver(nver),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: iver cannot be allocated'
        stop 1
      endif
      
      allocate(rotv(2,nel),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: rotv cannot be allocated'
        stop 1
      endif
      
      allocate(rotu(2,nel),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: rotu cannot be allocated',nel
        stop 1
      endif 
      
      allocate(rotuh(2,nel),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: rotuh cannot be allocated',nel
        stop 1
      endif 

      allocate(hv(2,nel),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: hv cannot be allocated'
        stop 1
      endif
      


      allocate(cj(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array cj' 
      
        if (allocated(subcaux)) deallocate(subcaux) 
      allocate(subcaux(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error(lee) when allocating array subcaux'    
!
     
      if (allocated(subcncm)) deallocate(subcncm) 
      allocate(subcncm(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array subcncm'
          
      if (allocated(subcnrms)) deallocate(subcnrms) 
      allocate(subcnrms(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array subcnrms'     
     
   
end subroutine
