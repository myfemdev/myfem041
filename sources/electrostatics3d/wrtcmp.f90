!------------------------------------------------------------------------
!     WRTCMP SUBROUTINE                                                 
!     Subrutine for the writing of a field 
!------------------------------------------------------------------------

subroutine wrtcmp(num,sol,iusal,fich)

  implicit none
  double precision, intent(in) :: sol(*)
  character(len=*), intent(in) :: fich
  integer,          intent(in) :: iusal
  integer,          intent(in) :: num
  integer                      :: i

  open(iusal, file = fich, form = 'formatted')
  rewind(iusal)
  write(iusal,*)num,(sngl(sol(i)),i=1,num) 
  close(iusal)

  return
end
