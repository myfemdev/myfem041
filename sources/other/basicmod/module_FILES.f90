module module_FILES
!-----------------------------------------------------------------------
! Module for files management
! Last update: 27/05/2008
!
! PROCEDURES:
!   get_unit: seeks a non connected unit number
!   file_exists: checks the existence of a file
!-----------------------------------------------------------------------
use module_REPORT, only: error
use module_CONVERS, only: string
implicit none

contains

!--------------------------------------------------------------------
! get_unit: gets a non-connected unit number
!--------------------------------------------------------------------
function get_unit() result(next)

  implicit none
  integer :: next, ios

  integer, parameter :: min = 10, max = 999
  logical :: open_

  do next = min, max
    inquire(unit = next, opened = open_, iostat = ios)
       if (ios /= 0) call error('get_unit/inquire, #'//trim(string(ios)))
       if (.not. open_) return
  end do
  call error('get_unit, unused unit number not found')
 
end function

!--------------------------------------------------------------------
! file_exists: checks the existence of a file
!--------------------------------------------------------------------
function file_exists(filename) result(res)

  character(len=*) :: filename
  logical          :: res
  integer :: iu, ios
  logical :: exists, already_open

  inquire(file = filename, exist = exists, opened = already_open, iostat = ios)
  if (ios /= 0) call error('file_exists/inquire, #'//trim(string(ios)))
  if (.not. exists) then
    res = .false.
    return
  elseif (.not. already_open) then
    iu = get_unit()
    open(iu, file = filename, iostat = ios)
    if (ios /= 0) then
      res = .false.
      return
    else
       close(iu)
    end if
  end if
  res =.true.

end function

end module
