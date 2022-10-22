module module_ALLOC_char_r1
!-----------------------------------------------------------------------
! Module for memory allocation of character rank 1 allocatable arrays
! Last update: 30/10/2009
! Programmer: fran.pena@usc.es
!
! PUBLIC PROCEDURES: dealloc, alloc, extend, reduce, enlarge and set
! - you only have to use "set" since it implies allocation, if necessary
! - "reduce" cuts off an array filled without fitting
! - "enlarge" ensures the allocation of a given position
!-----------------------------------------------------------------------
use module_REPORT, only: error, info
implicit none

!Constants
integer, parameter, private :: DEFAULT_ALLOC  = 1000 !initial size for allocation
integer, parameter, private :: DEFAULT_EXTEND = 1000 !additional size for extension

!Private procedures
private :: dealloc_prv,  alloc_prv,  extend_prv,  reduce_prv, enlarge_prv
private :: set_prv, enlarge1

!Interface
interface dealloc; module procedure dealloc_prv; end interface
interface   alloc; module procedure   alloc_prv; end interface
interface  extend; module procedure  extend_prv; end interface
interface  reduce; module procedure  reduce_prv; end interface
interface enlarge; module procedure enlarge_prv; end interface
interface     set; module procedure     set_prv; end interface

contains

!-----------------------------------------------------------------------
! dealloc: dealloc memory
!-----------------------------------------------------------------------
subroutine dealloc_prv(v)

  character(len=*), dimension(:), allocatable :: v
  integer :: res

  if (.not. allocated(v)) return
  deallocate(v, stat = res) !In f2003 add: errmsg = cad
  if (res /= 0) call error('(module_ALLOC_char_r1/dealloc) Unable to deallocate variable')

end subroutine

!-----------------------------------------------------------------------
! alloc: alloc memory
!-----------------------------------------------------------------------
subroutine alloc_prv(v, rows)

  character(len=*), dimension(:), allocatable :: v
  integer, intent(in), optional      :: rows
  integer :: res, n

  n = DEFAULT_ALLOC; if (present(rows)) n = rows
  if (allocated(v)) then
    if (size(v,1) == n) then
      v(1:n) = ' '; return
    end if
    call dealloc(v)
  end if
  allocate(v(n), stat = res) !In f2003 add: errmsg = cad
  if (res /= 0) call error('(module_alloc_char_r1/alloc) unable to allocate variable')
  v(1:n) = ' '

end subroutine

!-----------------------------------------------------------------------
! extend: extend the array
!-----------------------------------------------------------------------
subroutine extend_prv(v, extension)

  character(len=*), dimension(:), allocatable :: v
  integer, intent(in), optional      :: extension
  character(len=len(v)), dimension(:), allocatable :: temp
  integer :: ext, n0, n

  ext = DEFAULT_EXTEND !extension
  if (present(extension))then
    if (extension <= 0) return
    ext = extension
  end if
  if (.not. allocated(v)) then !v not allocated
     call alloc(v, ext); return
  end if
  n = size(v,1) + ext
  n0 = size(v,1)
  call alloc(temp, n) !temporal storage
  temp(1:n0) = v
  temp(n0+1:n) = ' '  !reset new positions
  call dealloc(v)
  call alloc(v, n)    !final allocation
  v(1:n) = temp       !data copy
  call dealloc(temp)

end subroutine

!-----------------------------------------------------------------------
! reduce: reduce the array
!-----------------------------------------------------------------------
subroutine reduce_prv(v, length)

  character(len=*), dimension(:), allocatable :: v
  integer, intent(in)                :: length
  character(len=len(v)), dimension(:), allocatable :: temp

  if (.not. allocated(v)) then  !v not allocated
    call info('(module_ALLOC_char_r1/reduce) Variable not allocated'); return
  end if
  if (length > size(v, 1)) then !p too large
    call info('(module_ALLOC_char_r1/reduce) Given dimension is too large'); return
  end if
  call alloc(temp, length)      !temporal storage
  temp(1:length) = v(1:length)  !data copy
  call dealloc(v)
  call alloc(v, length)         !final allocation
  v(1:length) = temp            !data copy
  call dealloc(temp)

end subroutine

!-----------------------------------------------------------------------
! enlarge: enlarge an array for ensuring a given position
!-----------------------------------------------------------------------
subroutine enlarge_prv(v, row, fit_row)

  character(len=*), dimension(:), allocatable :: v
  integer, intent(in),  optional     :: row
  logical, intent(in),  optional     :: fit_row
  integer :: r

  r = enlarge1(v, row, fit_row) !extension in rows, if necessary

end subroutine

!-----------------------------------------------------------------------
! set: set a value to the array
!-----------------------------------------------------------------------
subroutine set_prv(v, val, row, fit_row)

  character(len=*), dimension(:), allocatable :: v
  character(len=*), intent(in)                :: val
  integer, intent(in), optional      :: row
  logical, intent(in), optional      :: fit_row
  integer :: r

  r = enlarge1(v, row, fit_row) !extension in rows, if necessary
  v(r) = val

end subroutine

!***********************************************************************
! PRIVATE PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! enlarge1: enlarge an array in the given dimension
! RESULT: p, position prepared to set data
!-----------------------------------------------------------------------
function enlarge1(v, puser, fit) result(p)

  character(len=*), dimension(:), allocatable :: v
  integer, intent(in), optional      :: puser
  logical, intent(in), optional      :: fit
  integer :: p, n

  n = 0 !size in dimension
  if (allocated(v)) n = size(v, 1)
  p = n + 1 !position
  if (present(puser)) p = puser
  !ALLOCATE
  if (present(fit)) then
    if (fit) then; call extend(v, extension=p-n)   !fit is true
    else; do while (n < p); call extend(v); n = size(v,1); end do !fit is false
    end if
  else; do while (n < p); call extend(v); n = size(v,1); end do   !fit is false
  end if

end function

end module
