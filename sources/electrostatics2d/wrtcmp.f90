!------------------------------------------------------------------------
!     WRTCMP SUBROUTINE                                                 
!     Subrutine for the writing of a field 
!------------------------------------------------------------------------

subroutine wrtcmp(sol,iusal,fich)

  use malla_2DP1, only: nver

  implicit none
  double precision, intent(in) :: sol(*)
  character(len=*), intent(in) :: fich
  integer,          intent(in) :: iusal
  integer                      :: i
  
  open(iusal, file = fich, form = 'formatted')
  rewind(iusal)
  write(iusal,*)nver,(sngl(sol(i)),i=1,nver) 
  close(iusal)

  return
end
