module module_ALLOC
!-----------------------------------------------------------------------
! Module for memory allocation (gathers modules for particular data types)
! Last update: 28/07/2009
! Programmer: fran.pena@usc.es
!-----------------------------------------------------------------------
!INTEGER
use module_ALLOC_int_r1
use module_ALLOC_int_r2
use module_ALLOC_int_alloc_r2
!REAL
use module_ALLOC_real_r1
use module_ALLOC_real_r2
use module_ALLOC_real_alloc_r2
use module_ALLOC_real_DOUBLE_r1
use module_ALLOC_real_DOUBLE_r2
!use module_ALLOC_real_DOUBLE_alloc_r2
!CHARACTER
use module_ALLOC_char_r1
!use module_ALLOC_char_r2
!use module_ALLOC_char_alloc_r2
!LOGICAL
!use module_ALLOC_log_r1
use module_ALLOC_log_r2
!use module_ALLOC_log_alloc_r2
!use module_ALLOC_complex_DOUBLE_r1
implicit none

end module
