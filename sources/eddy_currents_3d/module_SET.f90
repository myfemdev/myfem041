module module_set
!-----------------------------------------------------------------------
! Module for set management
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Francisco Pena, fran.pena@usc.es
! Last update: 07/07/2010
!
! PUBLIC PROCEDURES:
! unique: returns the same values as in v with no repetitions.
!   REMARK: res will not be sorted
! setdiff: returns the values in v that are not in w
!   REMARK: res will not be sorted but its elements will be unique
!-----------------------------------------------------------------------
implicit none

contains

!-----------------------------------------------------------------------
! unique: returns the same values as in v with no repetitions.
! REMARK: res will not be sorted (contrary to Matlab)
!-----------------------------------------------------------------------
subroutine unique(v, res)
integer, dimension(:),              intent(in)  :: v
integer, dimension(:), allocatable, intent(out) :: res
integer, dimension(size(v,1)) :: tmp
integer :: ios, n, i
!character(len=260) :: err_msg

n = 0
do i = 1, size(v,1) !loop to find the elements
  if (all(tmp(1:n) /= v(i))) then
    n = n + 1
    tmp(n) = v(i)
  endif
enddo
allocate(res(n), stat=ios) !, errmsg=err_msg)
if (ios /= 0) then; print*, 'unique, unable to allocate output variable.'; stop 1; endif!//err_msg; stop; endif
res = tmp(1:n)
end subroutine

!-----------------------------------------------------------------------
! setdiff: returns the values in v that are not in w
! REMARK: res will not be sorted but its elements will be unique
!-----------------------------------------------------------------------
subroutine setdiff(v, w, res)
integer, dimension(:),              intent(in)  :: v, w
integer, dimension(:), allocatable, intent(out) :: res
integer, dimension(size(v,1)) :: tmp
integer :: ios, n, i
!character(len=260) :: err_msg

n = 0
do i = 1, size(v,1) !loop to find the elements
  if (all(w /= v(i))) then
    if (any(tmp(1:n) == v(i))) cycle
    n = n + 1
    tmp(n) = v(i)
  endif
enddo
allocate(res(n), stat=ios) !, errmsg=err_msg)
if (ios /= 0) then; print*, 'unique, unable to allocate output variable.'; stop 1; endif!//err_msg; stop; endif
res = tmp(1:n)
 print*, '#####################################################'
 print*, 'Setdiff'
 print*, 'V: size->',size(v,1)
 print*, 'W: size->',size(w,1)
 print*, 'res: size->',size(res,1)
 print*, '#####################################################'
end subroutine

end module
