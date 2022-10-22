!---------------------------------------------------------------------------
!  strong imposition of Dirichlet conditions via interface array 
!   b  : second member
!   nrb: number of references
!   irb: value of the references
!---------------------------------------------------------------------------

subroutine bloseg3D(b,nrb,irb)

  use bloqueo
  use malla_3DP1

  implicit none
  integer,          intent(in)     :: irb(*)
  double precision, intent (inout) :: b(*)
  integer                          :: i,n,nrb,j,iv,nr
      
  do i=1,nrb
     do j=1,nvrebf(irb(i))
        iv=ivrebf(irb(i),j)
        nr=nrvg(iv)
        b(iv)=h(z(1,iv),z(2,iv),z(3,iv),nr,i)
     enddo     
  enddo
 
  return
end
