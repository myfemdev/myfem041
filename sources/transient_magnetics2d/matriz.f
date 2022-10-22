*****************************************************************
*           CALCULO DE LA MATRIZ DEL PROBLEMA TERMICO           *
*****************************************************************

      SUBROUTINE matriz()

      use malla_2DP1
      use electros_2D
      use permeabilidad
      use derivados
 
      implicit double precision (a-h,o-z)

!        Variables internas
         dimension a(3,3)

!        Inicializacion del vector c
         ndcc = mua(nver+1)
         do 1 i=1,ndcc
            c(i)=0.d0
    1    continue 

!        -----------------------------------------------------------
!        BUCLE EN ELEMENTOS PARA CALCULO DE LA MATRIZ Y ENSAMBLADO
!        -----------------------------------------------------------
         do 2 k=1,nel

            mm1=mm(1,k)
            mm2=mm(2,k)
            mm3=mm(3,k)
!           Calculos previos
            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

!           Jacobiano de la matriz de paso al elemento de referencia
            det = ab*de-bc*cd
      		 if (det.eq.0.d0) then
      	       print*,'det= ',det
      	       print*,'k=', k
      	       print*,'mm=', mm(1,k), mm(2,k), mm(3,k)
               print*,'abcd=', ab, bc, cd, de
      		   stop
            endif

!           Calculo de la matriz elemental

            call matelmagneorto(iop,mm(1,k),nsd(k),z,ab,bc,cd,de,
     &            det,a) 
     
      	
!           Ensamblado de la matriz a

            call ens(mua,a,mm(1,k),c)
            
    2    continue
    
       
      if(potencial_dat_vol%numero.gt.0.or.
     &   potencial_dat_sur%numero.gt.0) call matelmasa() ! lo unico que cambia

      return
      
      end
