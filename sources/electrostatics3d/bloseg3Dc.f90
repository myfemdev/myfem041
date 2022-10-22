!---------------------------------------------------------------------------
!  strong imposition of Dirichlet conditions via interface array 
!   b  : second member
!   nrb: number of references
!   irb: value of the references
!---------------------------------------------------------------------------

subroutine bloseg3Dc(b,nrb,irb,tablo)

  use bloqueo
  use malla_3DP1

  implicit none
  integer,          intent(in)     :: irb(*) 
  double precision, intent (in)    :: tablo(*)
  double precision, intent (inout) :: b(*)
  integer                          :: i, n, nrb,j,iv
      
  do i=1,nrb
     do j=1,nvrebc(irb(i))
        iv=ivrebc(irb(i),j)
        b(iv)=tablo(i)
     enddo     
  enddo

  return
end
