
      subroutine calcampomag(p)
      
!     Calculo del campo magnético H = 1/mu rot A (lineal), H = omega* rot A + p
    
      use parametros_electros
      use derivados
      use electros_2D
      use malla_2DP1
      use nolineal
      use permeabilidad	

      implicit none
      double precision :: p(2,*),ab,bc,cd,de,det,x1,x2,omegak,xbar,ybar,
     &                    xint1,xint2
      integer :: k,mm1,mm2,mm3,iopermagr,ndom,i,j,ioplini
      
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
        
        do   i=1,permagrel%numero
      			if(nsd(k).eq.permagrel%referencias(i)) then	
                  iopermagr=permagrel%iopermagr(i)
                  ndom=i
                  ioplini=permagrel%ioplin(i) !alfredo
               
                  if (ioplini.eq.0) then ! dominio no lineal
	              do j=1,ndnolin
      			      if(nsd(k).eq. idnolin(j)) then	
                         omegak=omega(j)
      			      endif
      	            enddo
                  endif
      			endif
        enddo 
        
        
        if(iopermagr.eq.0.and.ioplini.eq.1) 
     &     stop 'domain without assigned permeability'
     
        if(ioplini.eq.1) then  !elemento en dominio lineal  
     
           xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
           ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0
           
           xint2=1.d0/perme(xbar,ybar,nsd(k),1,ndom,ioplini,ndnolin, !alfredo
     &           idnolin,omega)

           xint1=1.d0/perme(xbar,ybar,nsd(k),2,ndom,ioplini,ndnolin,  !alfredo
     &           idnolin,omega)

            

!hay que dividir entre la permeabilidad magnética en cada elemento
!        print*,'rotuh',rotuh(1,k),rotuh(2,k)

	     rotuh(1,k) = (-cd*x1+ab*x2)/det*xint2
	     rotuh(2,k) = -(de*x1-bc*x2)/det*xint1


        else !elemento en dominio no lineal
       
           rotuh(1,k) = (-cd*x1+ab*x2)/det *omegak  + p(1,k)
	     rotuh(2,k) = -(de*x1-bc*x2)/det *omegak  + p(2,k)
	  
	  endif

      end do
  
     
   
! El rotacional de A es constante por elemento, por lo que se promedia a 
! vértices para pintar  

      do 2 i=1,nver
        iver(i)=0
	  hv(1,i) = 0.d0
	  hv(2,i) = 0.d0
 2    continue

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



