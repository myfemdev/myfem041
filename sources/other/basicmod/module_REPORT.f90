module module_REPORT
!-----------------------------------------------------------------------
! Module for messages and errors management
! Last update: 14/07/2008
!
! MODULE VARIABLES (private):
!   break: whether or not to stop the program in error()
!   file: actual report file
!   folder: actual report folder
!   level: actual report level
! Posible error level are REPORT_NONE, REPORT_STDOUT, REPORT_FILE, REPORT_ALL
!   errfound: whether or not an error was found 
!
! INPUT PROCEDURES:
!   Set procedures for break, file, folder and level
! OUTPUT PROCEDURES:
!   error: sends a report and stops the program (depending on break)
!   info: sends a report 
!   error_found: checks whether or not an error was found
! PRIVATE PROCEDURES:
!   get_unit: seeks a non-connected unit number
!   add_sep: adds a slash (if necessary) at the end of the string
!-----------------------------------------------------------------------
use module_COMPILER_DEPENDANT, only: STDERR, STDOUT, MAXPATH
use module_SO_DEPENDANT, only: slash
implicit none

!Constants
integer, parameter, public :: REPORT_NONE   = 0
integer, parameter, public :: REPORT_STDOUT = 1
integer, parameter, public :: REPORT_FILE   = 2
integer, parameter, public :: REPORT_ALL    = 3

!Variables
logical,                  private :: break  = .true.
character(len = MAXPATH), private :: file   = 'OUTPUT'
character(len = MAXPATH), private :: folder = '.'
integer,                  private :: level  = REPORT_ALL
logical,                  private :: errfound = .false.

!Interfaces

!Private procedures
private :: get_unit, add_sep

contains

!***********************************************************************
! INPUT PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! set_report_stop: sets the actual value of break
!-----------------------------------------------------------------------
subroutine set_report_stop(b)

  logical, intent(in) :: b
  break = b

end subroutine

!-----------------------------------------------------------------------
! set_report_file: sets the actual value of file
!-----------------------------------------------------------------------
subroutine set_report_file(f)

  character(len = *), intent(in) :: f
  file = f

end subroutine

!-----------------------------------------------------------------------
! set_report_folder: sets the actual value of folder
!-----------------------------------------------------------------------
subroutine set_report_folder(f)

  character(len = *), intent(in) :: f
  folder = add_sep(f)

end subroutine

!-----------------------------------------------------------------------
! set_report_level: sets the actual value of level
!-----------------------------------------------------------------------
subroutine set_report_level(l)

  integer, intent(in) :: l
  level = l

end subroutine

!***********************************************************************
! OUTPUT PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! error: report an error
!-----------------------------------------------------------------------
subroutine error(err)

  character(len = *), intent(in) :: err
  integer :: iu

  errfound = .true.
! file write
  if (level == REPORT_FILE .or. level == REPORT_ALL) then
    iu = get_unit()
    if (iu > 0) then
      open(iu, file = trim(add_sep(folder))//adjustl(file), position = "append")
      write(iu,*) 'ERROR: '//trim(err)
      close(iu)
    end if
  end if
      
! standard write
  if (level == REPORT_STDOUT .or. level == REPORT_ALL) then
    write(STDERR,'(a)') 'ERROR: '//trim(err)
  end if

! program stops
  if (break) stop 1

end subroutine

!-----------------------------------------------------------------------
! info: report an info
!-----------------------------------------------------------------------
subroutine info(inf)

  character(len = *), intent(in) :: inf
  integer :: iu

! file write
  if (level == REPORT_FILE .or. level == REPORT_ALL) then
    iu = get_unit()
    if (iu > 0) then
      open(iu, file = trim(add_sep(folder))//adjustl(file), position = "append")
      write(iu,*) 'INFO: '//trim(inf)
      close(iu)
    end if
  end if
      
! standard write
  if (level == REPORT_STDOUT .or. level == REPORT_ALL) then
    write(STDOUT,'(a)') 'INFO: '//trim(inf)
  end if

end subroutine

!-----------------------------------------------------------------------
! error_found: checks whether or not an error was found
!-----------------------------------------------------------------------
function error_found() result(res)

  logical :: res
  
  res = errfound
  
end function

!***********************************************************************
! PRIVATE PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! add_sep: add a slash (if necessary) at the end of the string
!-----------------------------------------------------------------------
function add_sep(cad) result(res)

  character(len = *) :: cad
  character(len = len(cad) + 1) :: res 

  res = cad
  if (index(cad,slash(),back=.true.).ne.len_trim(cad)) res = trim(cad)//slash()

end function

!--------------------------------------------------------------------
! get_unit: get a non-connected unit number
! NOTE: get_unit (module_FILES) cannot be called, because this module 
! uses module_FILES 
!--------------------------------------------------------------------
function get_unit() result(next)

  implicit none
  integer :: next

  integer, parameter :: min = 10, max = 999
  logical :: open_

  do next = min, max
    inquire(unit = next, opened = open_)
       if (.not. open_) return
  end do
  next = -1

end function

end module
