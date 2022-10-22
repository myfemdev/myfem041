subroutine alloc_after_mesh()

  use fich_electros
  use malla_2DP1
  use electros_2D

  implicit none

  if(allocated(mua))deallocate(mua)
  allocate(mua(nver+1),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array mua',nver+1
     stop 1
  endif

  if(allocated(b))deallocate(b)
  allocate(b(nver),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array b',nver
     stop 1
  endif

  if(allocated(sol))deallocate(sol)
  allocate(sol(nver),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating array sol',nver
     stop 1
  endif

end subroutine
