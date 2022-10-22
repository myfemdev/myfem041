************************************************************************
*     CONSTRUCION DE LA MATRIZ ELEMENTAL DE MASA                       *
************************************************************************
      SUBROUTINE matel2(iop,n,z,det,om,nsdk,a,k)
	
      
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if(domains%jsdata(nsdk) .or. domains%mododiel(nsdk))then
!          print*,'domains%jsdata(nsdk),nsdk',domains%jsdata(nsdk),nsdk
!        print*,'domains%mododiel(nsdk),nsdk',domains%mododiel(nsdk),nsdk
!          print*,'nsd(k) que no ensambla',nsdk
         return
       endif
         
!       print*,'nsd(k) que ensambla',nsdk

         IF (iop .EQ. 1) THEN

C           CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO
c           xint = (.../2)/9
            xint = (om*conducel(nsdk,ndom)*det) / 18.D0

            a(1,1) = a(1,1) + xint
            a(2,1) = a(2,1) + xint
            a(2,2) = a(2,2) + xint
            a(3,1) = a(3,1) + xint
            a(3,2) = a(3,2) + xint
            a(3,3) = a(3,3) + xint
         ELSE IF (iop .EQ. 2) THEN

C           INTEGRAL EN EL TRIANGULO K (FORMULA DE LOS VERTICES)
            a(1,1) = a(1,1) + (om*det*conducel(nsdk,ndom))/6.d0
*           a(2,1) = a(2,1) + 0.D0
            a(2,2) = a(2,2) + (om*det*conducel(nsdk,ndom))/6.d0
*           a(3,1) = a(3,1) + 0.D0
*           a(3,2) = a(3,2) + 0.D0
            a(3,3) = a(3,3) + (om*det*conducel(nsdk,ndom))/6.d0
         ELSE 

C           IDEM MEDIANTE LA FORMULA DE LOS PUNTOS MEDIOS

c           xint?? = (.../6)/4
            xint12 = (conducel(nsdk,ndom)*om*det)/24.d0
		    xint23 = (conducel(nsdk,ndom)*om*det)/24.d0
            xint31 = (conducel(nsdk,ndom)*om*det)/24.d0
            a(1,1) = a(1,1) + xint12 + xint31
            a(2,1) = a(2,1) + xint12
            a(2,2) = a(2,2) + xint12 + xint23
            a(3,1) = a(3,1) + xint31
            a(3,2) = a(3,2) + xint23
            a(3,3) = a(3,3) + xint23 + xint31
         END IF
         DO 1 i=1,2
            DO 2 j=i+1,3
               a(i,j) = a(j,i)
    2       CONTINUE
    1    CONTINUE
         RETURN

      END