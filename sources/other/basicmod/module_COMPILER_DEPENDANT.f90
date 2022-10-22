module module_compiler_dependant
!-----------------------------------------------------------------------
! Module with all the objects that are compiler dependant
! Last update: 19/11/2011
!-----------------------------------------------------------------------
implicit none

!Constants
integer, parameter, public :: STDERR      =   0   
integer, parameter, public :: STDOUT      =   6   
integer, parameter, public :: MAXPATH     = 260 
integer, parameter, public :: input_unit  =   5 
integer, parameter, public :: output_unit =   6 
integer, parameter, public :: error_unit  =   0 
integer, parameter, public :: iostat_end  =  -1 
integer, parameter, public :: iostat_eor  =  -2 
integer, parameter, public :: DOUBLE = selected_real_kind(15, 307)
integer, parameter, public :: real64 = selected_real_kind(15, 307)
integer, parameter, public :: real32 = selected_real_kind(6, 37)

end module
