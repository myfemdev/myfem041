!------------------------------------------------------------------------
!     WRTCMPV SUBROUTINE                                                
!     Subroutine for the writing of a vectorial field 
!------------------------------------------------------------------------

subroutine wrtcmpv(sol,iusal,fich)

  use malla_2DP1, only: nel

  implicit none
  double precision, intent(in) :: sol(2,*)
  character(len=*), intent(in) :: fich
  integer,          intent(in) :: iusal
  integer                      :: i

  open(iusal, file = fich, form = 'formatted')
  rewind(iusal)
  write(iusal,*)nel,(sngl(sol(1,i)),sngl(sol(2,i)),i=1,nel) 
  close(iusal)

  return
end
