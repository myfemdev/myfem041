*****************************************************************
*     CONSTRUCION DE LA MATRIZ ELEMENTAL                        *
*****************************************************************

      subroutine matelmagneorto(iop,n,nsdk,z,ab,bc,cd,de,det,a) 
     
      use permeabilidad	
      use electros_2D,  only: teta
      use nolineal

      implicit double precision (a-h,o-z)
   
      dimension n(3),z(2,*),a(3,3)

     
      iopermagr=0
 	do i=1,permagrel%numero
      			if(nsdk.eq.permagrel%referencias(i)) then	
                iopermagr=permagrel%iopermagr(i)
                ndom=i
                ioplini=permagrel%ioplin(i) !alfredo
                ! ioplini determina si el triangulo k esta o no en un dominio lineal (vale 1 o cero)
      			endif
      enddo
       if(iopermagr.eq.0.and.ioplini.eq.1)
     & stop 'dominio sin asignar permitividad'



C           FORMULA DE LOS PUNTOS MEDIOS
            x12 = (z(1,n(1)) + z(1,n(2))) * 0.5D0
            y12 = (z(2,n(1)) + z(2,n(2))) * 0.5D0
            x23 = (z(1,n(2)) + z(1,n(3))) * 0.5D0
            y23 = (z(2,n(2)) + z(2,n(3))) * 0.5D0
            x31 = (z(1,n(3)) + z(1,n(1))) * 0.5D0
            y31 = (z(2,n(3)) + z(2,n(1))) * 0.5D0
            

            xint2 = (1.d0/3.d0) * 
     &( 1.d0/perme(x12,y12,nsdk,1,ndom,ioplini,ndnolin,idnolin,omega) + 
     &	1.d0/perme(x23,y23,nsdk,1,ndom,ioplini,ndnolin,idnolin,omega) +
     &  1.d0/perme(x31,y31,nsdk,1,ndom,ioplini,ndnolin,idnolin,omega) )
     
            xint1 = (1.d0/3.d0) * 
     &( 1.d0/perme(x12,y12,nsdk,2,ndom,ioplini,ndnolin,idnolin,omega) + 
     &	1.d0/perme(x23,y23,nsdk,2,ndom,ioplini,ndnolin,idnolin,omega) + 
     &  1.d0/perme(x31,y31,nsdk,2,ndom,ioplini,ndnolin,idnolin,omega) ) 
        
   
      g11=xint1*de**2+xint2*cd**2
      g12=-xint1*bc*de-xint2*ab*cd
      g22=xint1*bc**2+xint2*ab**2

C        CALCULO DE LA MATRIZ 3 x 3
         a(1,1) =  0.5D0 * (g11+2.*g12+g22) / det
         a(2,1) =  0.5D0 * (-g11-g12) / det
         a(2,2) =  0.5D0 * g11 / det
         a(3,1) =  0.5D0 * (-g12-g22) / det
         a(3,2) =  0.5D0 * g12 / det
         a(3,3) =  0.5D0 * g22 / det
         DO 1 i=1,2
            DO 2 j=i+1,3
               a(i,j) = a(j,i)
    2       CONTINUE
    1    CONTINUE
         RETURN

      END
