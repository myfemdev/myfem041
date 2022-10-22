module module_SO_dependant
!-----------------------------------------------------------------------
! Module with all the objects that are SO dependant
! Last update: 14/07/2008
!
! MODULE VARIABLES (private):
!   SO: current operating system 
!       Posible values are 'LINUX' or 'WINDOWS'
!
! INPUT PROCEDURES:
!   Set procedures for SO
! OUTPUT PROCEDURES:
!   slash: returns the symbol for folder separation
!-----------------------------------------------------------------------
use module_COMPILER_DEPENDANT, only: MAXPATH
implicit none

!Variables
character(len=MAXPATH), private :: SO  = 'LINUX'

contains

!***********************************************************************
! INPUT PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! set_SO: sets the actual value of SO
!-----------------------------------------------------------------------
subroutine set_SO(arg)

  character(len=*), optional, intent(in) :: arg
  character(len=MAXPATH) :: path
  
  if (present(arg)) then
    SO = arg
  else
    inquire(file='.', name=path)
    if (index(path,'\') > 0) SO = 'WINDOWS'
  endif

end subroutine

!***********************************************************************
! OUTPUT PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! slash: returns the symbol for folder separation
!--------------------------------------------------------------------
function slash() result(res)

  character(len=1) :: res

  select case(trim(SO))
  case('WINDOWS')
    res = '\'
  case default
    res = '/'
  end select

end function

end module
