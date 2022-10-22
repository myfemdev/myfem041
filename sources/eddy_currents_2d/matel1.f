************************************************************************
*     CONSTRUCION DE LA MATRIZ ELEMENTAL RIGIDEZ                             *
************************************************************************
      SUBROUTINE matel1(iop,n,nsdk,z,ab,bc,cd,de,det,a)
      
      use permeabilidad	
      use electros_2D,  only: teta
	implicit double precision(a-h,o-z)
      DIMENSION n(3),z(2,*),a(3,3)
      
      iopermagr=0
 	do i=1,permagrel%numero
      	if(nsdk.eq.permagrel%referencias(i)) then	
            iopermagr=permagrel%iopermagr(i)
            ndom=i
      	endif
      enddo
      if(iopermagr.eq.0) stop 'Domain without permeability'

	do 3 i=1,3
	  do 4 j=1,3
	    a(i,j) = 0.d0
4       continue
3     continue


         IF (iop .EQ. 1) THEN

C           CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO
            rbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))) / 3.d0
            zbar = (z(2,n(1)) + z(2,n(2)) + z(2,n(3))) / 3.d0

            xint = 1.d0/(perme(rbar,zbar,nsdk,ndom)) * 0.5d0
         ELSE IF (iop .EQ. 2) THEN

C           INTEGRAL EN EL TRIANGULO K (FORMULA DE LOS VERTICES)
            xint = ( 1.d0/(perme(z(1,n(1)),z(2,n(1)),nsdk,ndom))
     &             + 1.d0/(perme(z(1,n(2)),z(2,n(2)),nsdk,ndom) )
     &             + 1.d0/(perme(z(1,n(3)),z(2,n(3)),nsdk,ndom)) )/6.d0
         ELSE 

C           IDEM MEDIANTE LA FORMULA DE LOS PUNTOS MEDIOS

            r12 = (z(1,n(1)) + z(1,n(2))) * 0.5d0
            r23 = (z(1,n(2)) + z(1,n(3))) * 0.5d0
            r31 = (z(1,n(3)) + z(1,n(1))) * 0.5d0
            
            z12 = (z(2,n(1)) + z(2,n(2))) * 0.5d0
            z23 = (z(2,n(2)) + z(2,n(3))) * 0.5d0
            z31 = (z(2,n(3)) + z(2,n(1))) * 0.5d0
            xint = (1.d0/(perme(r12,z12,nsdk,ndom)) + 
     &       1.d0/(perme(r23,z23,nsdk,ndom)) 
     &      + 1.d0/(perme(r31,z31,nsdk,ndom))) / 6.d0
         END IF

C        CALCULO DE LA MATRIZ 3 x 3
         a(1,1) = xint * ((bc-de)**2 + (cd-ab)**2) / det
         a(2,1) = xint * (de*(bc-de) + cd*(-cd+ab)) / det
         a(2,2) = xint * (de**2 + cd**2) / det
         a(3,1) = xint * (bc*(-bc+de) + ab*(cd-ab)) / det
         a(3,2) = xint * (-de*bc - (cd*ab)) / det
         a(3,3) = xint * (bc**2 + ab**2) / det
         DO 1 i=1,2
            DO 2 j=i+1,3
               a(i,j) = a(j,i)
    2       CONTINUE
    1    CONTINUE
   

         RETURN

      END