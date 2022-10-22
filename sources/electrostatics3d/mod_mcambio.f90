!---------------------------------------------------------------------------
! Module that contains: 
!   The definition of the transformation BT*x+CT
!   Function that computes the affine transformation for each element 
!---------------------------------------------------------------------------
! Change of basis affine transformation
!   BT	change of basis matrix
!   CT 	A4
!   det determinant of BT
!   inv inverse of BT
!---------------------------------------------------------------------------

module mod_mcambio

  use malla_3DP1 , only : nel,mm,z
  type cambio
    real*8::BT(3,3)
    real*8::CT(3)
    real*8::det
    real*8::inv(3,3)
  end type cambio

contains

! FUNCTION THAT COMPUTES THE CHANGE OF BASIS MATRIX 
 function afin(ie) result(cambioe)

   implicit none
   integer::ie
   real*8,dimension(4)::x,y,zz
   real*8::invv(3,3)
   type(cambio)::cambioe

! x,y,z COORDINATES OF THE NODES IN THE TETRAHEDRON ie 
   x=(/z(1,mm(1,ie)),z(1,mm(2,ie)),z(1,mm(3,ie)),z(1,mm(4,ie))/)
   y=(/z(2,mm(1,ie)),z(2,mm(2,ie)),z(2,mm(3,ie)),z(2,mm(4,ie))/)
   zz=(/z(3,mm(1,ie)),z(3,mm(2,ie)),z(3,mm(3,ie)),z(3,mm(4,ie))/)

! CHANGE OF BASIS MATRIX 
   cambioe%BT(1,1)=x(1)-x(4)
   cambioe%BT(1,2)=x(2)-x(4)
   cambioe%BT(1,3)=x(3)-x(4)
   cambioe%BT(2,1)=y(1)-y(4)
   cambioe%BT(2,2)=y(2)-y(4)
   cambioe%BT(2,3)=y(3)-y(4)
   cambioe%BT(3,1)=zz(1)-zz(4)
   cambioe%BT(3,2)=zz(2)-zz(4)
   cambioe%BT(3,3)=zz(3)-zz(4)

! DETERMINANT
   cambioe%det=cambioe%BT(1,1)*cambioe%BT(2,2)*cambioe%BT(3,3)+cambioe%BT(2,1)*cambioe%BT(3,2)&
                              *cambioe%BT(1,3)+cambioe%BT(1,2)*cambioe%BT(2,3)*cambioe%BT(3,1)&
              -cambioe%BT(1,3)*cambioe%BT(2,2)*cambioe%BT(3,1)-cambioe%BT(2,3)*cambioe%BT(3,2)&
                              *cambioe%BT(1,1)-cambioe%BT(1,2)*cambioe%BT(2,1)*cambioe%BT(3,3)

! INVERSE
   if (abs(cambioe%det)<1e-16) then
      print*,'Error: determinant of bt equal to zero',cambioe%det
   endif
   invv(1,1)=cambioe%BT(2,2)*cambioe%BT(3,3)-cambioe%BT(2,3)*cambioe%BT(3,2)
   invv(1,2)=-cambioe%BT(2,1)*cambioe%BT(3,3)+cambioe%BT(2,3)*cambioe%BT(3,1)
   invv(1,3)=cambioe%BT(2,1)*cambioe%BT(3,2)-cambioe%BT(2,2)*cambioe%BT(3,1)
   invv(2,1)=-cambioe%BT(1,2)*cambioe%BT(3,3)+cambioe%BT(1,3)*cambioe%BT(3,2)
   invv(2,2)=cambioe%BT(1,1)*cambioe%BT(3,3)-cambioe%BT(1,3)*cambioe%BT(3,1)
   invv(2,3)=-cambioe%BT(1,1)*cambioe%BT(3,2)+cambioe%BT(1,2)*cambioe%BT(3,1)
   invv(3,1)=cambioe%BT(1,2)*cambioe%BT(2,3)-cambioe%BT(1,3)*cambioe%BT(2,2)
   invv(3,2)=-cambioe%BT(1,1)*cambioe%BT(2,3)+cambioe%BT(1,3)*cambioe%BT(2,1)
   invv(3,3)=cambioe%BT(1,1)*cambioe%BT(2,2)-cambioe%BT(2,1)*cambioe%BT(1,2)

   invv=transpose(invv)/cambioe%det

   cambioe%inv=invv
   cambioe%CT(1)=x(4)
   cambioe%CT(2)=y(4)
   cambioe%CT(3)=zz(4)

   return
 end function afin

end module mod_mcambio
