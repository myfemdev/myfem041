*
      subroutine calrot()
      
!     Calculo de la inducción magnética, B = rot A  
    
      use parametros_electros
      use electros_2D
      use malla_2DP1

      implicit double precision (a-h,o-z)
          
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

        x1=-sol(mm1)+sol(mm2)
        x2=-sol(mm1)+sol(mm3)

*        gradu(1,k)=(de*x1-bc*x2)/det
*        gradu(2,k)=(-cd*x1+ab*x2)/det
*rot -- deixouse o grad para comprobr que estaba ben calculado

	  rotu(1,k) = (-cd*x1+ab*x2)/det
	  rotu(2,k) = -(de*x1-bc*x2)/det

  1   continue
  
! El rotacional de A es constante por elemento, por lo que se promedia a 
! vértices para pintar  

      do 2 i=1,nver
        iver(i)=0
	  rotv(1,i) = 0.d0
	  rotv(2,i) = 0.d0
 2    continue

      do 5 k=1,nel
        do 6 j=1,3
          iver(mm(j,k))=iver(mm(j,k))+1
	    rotv(1,mm(j,k)) = rotv(1,mm(j,k)) + rotu(1,k)
	    rotv(2,mm(j,k)) = rotv(2,mm(j,k)) + rotu(2,k)
  6     continue
  5   continue
  

      do 7 i=1,nver
         rotv(1,i) = rotv(1,i)/iver(i)
	   rotv(2,i) = rotv(2,i)/iver(i)
    7 continue

   
      return
      end



