!-----------------------------------------------------------
!     goal:                                           
!                computation of grad(be)                    
!-----------------------------------------------------------
!     parameters:                                         
!  input                                                
!     z1     coordinates array of no1                
!     z2     coordinates array of no2                
!     z3     coordinates array of no3                 
!     be     element vector                            
!  output                                                 
!     g1     first component of the gradient              
!     g2     second component of the gradient              
!-----------------------------------------------------------

subroutine matelgr(z1,z2,z3,be,g1,g2)

  implicit none
  double precision, intent(in)  :: z1(2),z2(2),z3(2),be(3)
  double precision, intent(out) :: g1,g2
  double precision              :: x21,b21,x31,b31,det

! COMPUTATION OF THE DETERMINANT OF THE AFFIN TRANSFORMATION
  x21=z2(1)-z1(1)
  b21=z2(2)-z1(2)
  x31=z3(1)-z1(1)
  b31=z3(2)-z1(2)
  det=x21*b31-x31*b21

! COMPUTATION OF THE GRADIENT 
  g1=((b21-b31)*be(1)+b31*be(2)-b21*be(3))/det   
  g2=((x31-x21)*be(1)-x31*be(2)+x21*be(3))/det 
   
  return
end
