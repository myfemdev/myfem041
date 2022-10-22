!------------------------------------------------------------------------
!     WRTCMPV SUBROUTINE                                                
!     Subroutine for the writing of a vectorial field 
!------------------------------------------------------------------------

subroutine wrtcmpv(nel,sol,iusal,fich)

  implicit none
  double precision, intent(in) :: sol(3,*)
  character(len=*), intent(in) :: fich
  integer,          intent(in) :: iusal
  integer,          intent(in) :: nel
  integer                      :: i

  open(iusal, file = fich, form = 'formatted')
  rewind(iusal)
  write(iusal,*)nel,(sngl(sol(1,i)),sngl(sol(2,i)),sngl(sol(3,i)),i=1,nel) 
  close(iusal)

  return
end
