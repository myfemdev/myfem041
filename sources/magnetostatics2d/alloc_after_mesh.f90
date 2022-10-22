subroutine alloc_after_mesh()
!Differents arrays are allocated after the lecture of the mesh

  use fich_electros
  use malla_2DP1
  use electros_2D
  
  implicit none
  integer             :: i
  
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
      

   
end subroutine
