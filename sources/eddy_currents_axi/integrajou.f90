       subroutine integrajou(nel,z,mm,cj,xint)
!Computation of Active power
! Axisymetric case (r in the integrand, 2pi for volumic computation)       
       implicit double precision(a-h,o-z)
       dimension mm(3,*),z(2,*)
       double complex, allocatable:: cj(:)
       double complex:: xint
       xint = 0.d0
       do k=1,nel
       
         mm1=mm(1,k)
         mm2=mm(2,k)
         mm3=mm(3,k)

!    calculos previos

         ab=z(1,mm2)-z(1,mm1)
         bc=z(2,mm2)-z(2,mm1)
         cd=z(1,mm3)-z(1,mm1)
         de=z(2,mm3)-z(2,mm1)
 
!     coef.y jacobiano de la matriz de paso al elem. de referencia
 
         det=ab*de-bc*cd
         
         xint = xint + (cj(mm(1,k))*z(1,mm(1,k)) + cj(mm(2,k))*z(1,mm(2,k)) + cj(mm(3,k))*z(1,mm(3,k)))*det/6.d0
        
      end do
      
!Volumic integral (*2*pi)      
      
      xint = 2.d0*dacos(-1.d0)*xint

      return
      
      end