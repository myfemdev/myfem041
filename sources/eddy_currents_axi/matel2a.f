************************************************************************
*     CONSTRUCION DE LA MATRIZ ELEMENTAL DE MASA                       *
*     Axisymmetric
*     Only baricentrric formula with A' formulation (A' = rA)          *
************************************************************************
      SUBROUTINE matel2a(iop,n,z,det,om,nsdk,a,k)
	
      
      use conductividad	
      use derivados
      use electros_2D,  only: teta
	implicit double precision(a-h,o-z)
      DIMENSION n(3),z(2,*),a(3,3)
      
      iopconduc=0
 	do i=1,conduc%numero
      	if(nsdk.eq.conduc%referencias(i)) then	
            iopconduc=conduc%iopcond(i)
            ndom=i
      	endif
      enddo
      
      if(iopconduc.eq.0) stop 'Domain without electrical conductivity'
      
      
      	do 3 i=1,3
	  do 4 j=1,3
	    a(i,j) = 0.d0
4       continue
3     continue

       if(domains%jsdata(nsdk) .or. domains%mododiel(nsdk))then
         return
       endif
         

         IF (iop .EQ. 1) THEN

C           CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO
c           xint = (.../2)/9
            rbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))) / 3.d0

            xint = (om*conducel(nsdk,ndom)*det/rbar) / 18.D0

            a(1,1) = a(1,1) + xint
            a(2,1) = a(2,1) + xint
            a(2,2) = a(2,2) + xint
            a(3,1) = a(3,1) + xint
            a(3,2) = a(3,2) + xint
            a(3,3) = a(3,3) + xint
   
         END IF
         DO 1 i=1,2
            DO 2 j=i+1,3
               a(i,j) = a(j,i)
    2       CONTINUE
    1    CONTINUE
         RETURN

      END