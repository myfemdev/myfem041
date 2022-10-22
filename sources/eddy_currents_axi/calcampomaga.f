
      subroutine calcampomaga( )
      
!     Calculo del campo magnético H = 1/mu rot A (lineal)
!     Axisymmetric
    
      !use parametros_electros
      use derivados
      use electros_2D
      use malla_2DP1
      use permeabilidad	

      implicit none
      double precision :: ab,bc,cd,de,det,xbar,ybar,
     &                    xint
      double complex x1, x2 
      integer :: k,mm1,mm2,mm3,iopermagr,ndom,i,j,nsdk
      
      do k=1,nel

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
      
        iopermagr=0
 	  do i=1,permagrel%numero
      	   if(nsd(k).eq.permagrel%referencias(i)) then	
              iopermagr=permagrel%iopermagr(i)
              ndom=i
      	   endif
         enddo
      
        if(iopermagr.eq.0) stop 'Domain without permeability'
     
     
           xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
           ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0
           
           xint=1.d0/perme(xbar,ybar,nsdk,ndom)


            

          rotuh(2,k) = ((de*x1-bc*x2)/det + 
     &          (sol(mm1)+ sol(mm2) + sol(mm3))/
     &              (z(1,mm1)+z(1,mm2)+z(1,mm3)))*xint

           rotuh(1,k)=(-(-cd*x1+ab*x2)/det)*xint


       

      end do
  
     
   
! El rotacional de A es constante por elemento, por lo que se promedia a 
! vértices para pintar  

        do 2 i=1,nver
          iver(i)=0
	    hv(1,i) = 0.d0
	    hv(2,i) = 0.d0
 2      continue

        do k=1,nel
         do j=1,3
           iver(mm(j,k))=iver(mm(j,k))+1
	     hv(1,mm(j,k)) = hv(1,mm(j,k)) + rotuh(1,k)
	     hv(2,mm(j,k)) = hv(2,mm(j,k)) + rotuh(2,k)
         enddo
       end do
  

       do  i=1,nver
         hv(1,i) = hv(1,i)/iver(i)
	   hv(2,i) = hv(2,i)/iver(i)
       end do

      return
      end



