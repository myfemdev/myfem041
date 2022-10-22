      subroutine cmrds

      use malla_2DP1
      use electros_2D
      
      
 
      implicit double precision (a-h,o-z)
      
      interface
       subroutine ens(mua1,e,n,c)
        implicit double precision(a-h,o-z)
	  double complex e(3,3),c(*)
        dimension n(*),mua1(*)
        end subroutine
      end interface 

!        Variables internas
         dimension a1(3,3),a2(3,3)
      double complex a3(3,3)

!        -----------------------------------------------------------
!        BUCLE EN ELEMENTOS PARA CALCULO DE LA MATRIZ Y ENSAMBLADO
!        -----------------------------------------------------------
 

C        INICIALIZACION A CERO DEL VECTOR c
      ndcc = mua(nver+1)
      DO 1 i=1,ndcc
          c(i)=0.d0
    1 CONTINUE

C        BUCLE EN ELEMENTOS PARA CALCULO DE LA MATRIZ Y ENSAMBLADO
      DO 2 k=1,nel

        nsdk = nsd(k)

C           ABREVIATURA DE LA NOTACION
        mm1 = mm(1,k)
        mm2 = mm(2,k)
        mm3 = mm(3,k)

C           CALCULOS PREVIOS
        ab = z(1,mm2) - z(1,mm1)
        bc = z(2,mm2) - z(2,mm1)
        cd = z(1,mm3) - z(1,mm1)
        de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA
        det = ab*de-bc*cd
        
    		 if (det.eq.0.d0) then
      	       print*,'det= ',det
      	       print*,'k=', k
      	       print*,'mm=', mm(1,k), mm(2,k), mm(3,k)
               print*,'abcd=', ab, bc, cd, de
      		   stop
            endif        

C           CALCULO DEL LA MATRIZ ELEMENTAL DE RIGIDEZ
        CALL matel1(iop,mm(1,k),nsdk,z,ab,bc,cd,de,det,a1)
         klocal = k

C           CALCULO DEL ELEMENTO DE MASA DE LA MATRIZ ELEMENTAL
	  CALL matel2(iop,mm(1,k),z,det,omega,nsdk,a2,klocal)
C           CALCULO DE LA MATRIZ TOTAL EN EL ELEMENTO
            DO 3 i=1,3
		    DO 4 j=1,3
			   a3(i,j) = dcmplx(a1(i,j),a2(i,j))
4             CONTINUE
3           CONTINUE	
   
C           ENSAMBLADO DE LA MATRIZ a

            CALL ens(mua,a3,mm(1,k),c)



    2    CONTINUE



     
      return
      end
