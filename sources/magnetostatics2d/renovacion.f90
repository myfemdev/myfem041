      subroutine renovacion(p)

      use malla_2DP1
      use electros_2D
      use nolineal
      
      implicit none
      double precision,  intent(inout) :: p(2,*) 
      double precision w(2), v(2), gamma
      integer k, i

      call calrot_nl() 

      do k=1,nel
        do i=1, ndnolin
          if (nsd(k).eq.idnolin(i)) then
        
            gamma = (1.-lambda(i)*omega(i))/lambda(i)
            v(1)  = (1./lambda(i))*(rotu(1,k)+lambda(i)*p(1,k))
            v(2)  = (1./lambda(i))*(rotu(2,k)+lambda(i)*p(2,k))
     
            call resolvente(gamma,v,w,i)	
                
	        p(1,k)=(1./(1.-lambda(i)*omega(i)))*w(1)-(omega(i)/(1.-lambda(i)*omega(i)))*(rotu(1,k)+lambda(i)*p(1,k))
            p(2,k)=(1./(1.-lambda(i)*omega(i)))*w(2)-(omega(i)/(1.-lambda(i)*omega(i)))*(rotu(2,k)+lambda(i)*p(2,k))

	        exit
	      endif
        enddo
      enddo

      return
      end 
