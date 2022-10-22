!--------------------------------------------------------------------
! GOAL:   Computation of the partial i of the shape functions 
!         when the matrix Bk is (a1-a4|a2-a4|a3-a4) and
!                    ptil1=x1
!                    ptil2=x2
!                    ptil3=x3
!                    ptil4=1-x1-x2-x3
!--------------------------------------------------------------------
! IN:     binv  ---> inverse of the matrix Bk
!         detk  ---> determinant of the matrix Bk.
!
! OUT:    cbar  ---> shape functions partial derivatives
!--------------------------------------------------------------------

subroutine derff(binv,detk,cbar)

  implicit none 
  double precision, intent(in)  :: binv(3,*)
  double precision, intent(in)  :: detk
  double precision, intent(out) :: cbar(4,*)

  cbar(1,1)=detk*binv(1,1)
  cbar(2,1)=detk*binv(2,1)
  cbar(3,1)=detk*binv(3,1)
  cbar(4,1)=detk*(-binv(1,1)-binv(2,1)-binv(3,1))
  cbar(1,2)=detk*binv(1,2)
  cbar(2,2)=detk*binv(2,2)
  cbar(3,2)=detk*binv(3,2)
  cbar(4,2)=detk*(-binv(1,2)-binv(2,2)-binv(3,2))
  cbar(1,3)=detk*binv(1,3)
  cbar(2,3)=detk*binv(2,3)
  cbar(3,3)=detk*binv(3,3)
  cbar(4,3)=detk*(-binv(1,3)-binv(2,3)-binv(3,3))

  return
end
