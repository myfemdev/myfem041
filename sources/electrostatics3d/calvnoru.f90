!--------------------------------------------------------------------
! GOAL:   computation of the standarized ortogonal vector to a plane 
!--------------------------------------------------------------------
! IN:     x1, x2, x3 ---> points defining the plane
!
! OUT:    xnor ---------> standarized ortogonal vector
!         dnor ---------> norm of the ortogonal vector
!--------------------------------------------------------------------

subroutine calvnoru(x1,x2,x3,xnor,dnor)

  implicit none 
  double precision, intent(in)  :: x1(*),x2(*),x3(*)
  double precision, intent(out) :: xnor(*)
  double precision, intent(out) :: dnor
  double precision              :: x21(3),x31(3)
  integer                       :: i

  do i=1,3
     x21(i)=x2(i)-x1(i)
     x31(i)=x3(i)-x1(i)
  enddo

  xnor(1)=x21(2)*x31(3)-x21(3)*x31(2)
  xnor(2)=x21(3)*x31(1)-x21(1)*x31(3)
  xnor(3)=x21(1)*x31(2)-x21(2)*x31(1)

  dnor=dsqrt(xnor(1)*xnor(1)+xnor(2)*xnor(2)+xnor(3)*xnor(3))

  xnor(1)=xnor(1)/dnor
  xnor(2)=xnor(2)/dnor
  xnor(3)=xnor(3)/dnor

  return
end
