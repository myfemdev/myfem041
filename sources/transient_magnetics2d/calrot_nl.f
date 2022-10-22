*
      subroutine calrot_nl()
      
!     Calculo de la inducción magnética, B = rot A  
    
      use parametros_electros
      use electros_2D
      use malla_2DP1
      use tiempo

      implicit none
      double precision :: ab,bc,cd,de,det,x1,x2
      integer :: k,mm1,mm2,mm3
              
          
      do 1 k=1,nel

*     abreviatura en la notacion

        mm1=mm(1,k)
        mm2=mm(2,k)
        mm3=mm(3,k)

*     calculos previos

        ab=z(1,mm2)-z(1,mm1)
        bc=z(2,mm2)-z(2,mm1)
        cd=z(1,mm3)-z(1,mm1)
        de=z(2,mm3)-z(2,mm1)
        det=ab*de-bc*cd

        x1=-bvar(mm1)+bvar(mm2)
        x2=-bvar(mm1)+bvar(mm3)

*        gradu(1,k)=(de*x1-bc*x2)/det
*        gradu(2,k)=(-cd*x1+ab*x2)/det
*        deixouse o grad para comprobar que estaba ben calculado

	  rotu(1,k) = (-cd*x1+ab*x2)/det
	  rotu(2,k) = -(de*x1-bc*x2)/det

  1   continue

      return
      end