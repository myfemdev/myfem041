module module_bsearch
!-----------------------------------------------------------------------
! Module for binary search
!
! Licensing: This code is distributed under the GNU GPL license.
! Authors: Francisco Pena, fran.pena@usc.es
! Last update: 3/10/2011
!
!
! USE:
! - search in an ordered array: 
!
!   pos = bsearch(size(v,1), v, val) 
!
! - insert in an ordered array:
!
!   if (pos < 0) call set(v, val, -pos, fit_row)
!-----------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
! bsearch: If 'u' is in 'x' then its index is returned.
! Otherwise − ind is returned, where ind is the index where 'u'
! should be inserted in 'x'
!-----------------------------------------------------------------------
function bsearch(n, x, u) result(pos)
integer,               intent(in) :: n !components of seq
integer, dimension(:), intent(in) :: x !array, qhere to search
integer,               intent(in) :: u !value to search
integer :: pos, i, j, k

pos = -1
if (size(x,1) <= 0 .or. n <= 0) return
if (u < x(1))  return

i=1; j=n
do
  k=(i+j)/2
  if (u < x(k)) then
    j=k
  else
    i=k
  end if
  if (i+1 >= j) exit
end do
if (u== x(i)) then
  pos = i
elseif ((i+1) <= n) then
  if (u == x(i+1)) then
    pos = i+1
  elseif (u < x(i+1)) then
    pos = -(i+1)
  else
    pos = -(i+2)
  end if  
else
  pos = -(i+1)
end if
    
!  right = n
!  left = 1
!  previous_center = 0
!  do
!    center = ceiling((left + right) / 2.)
!    candidate = seq(center)
!    if (search == candidate) then
!      pos = center; return
!    end if
!    if (center == previous_center) then
!      pos = -center-1; return
!    elseif (search < candidate) then
!      right = center
!    else
!      left = center
!    end if
!    previous_center = center
!  end do
end function

end module
