        subroutine preveddy ()
        
        use conductividad	
        use  parametros_electros
        use derivados
        use malla_2DP1    
        use electros_2D
        implicit double precision(a-h,o-z)
        
        pi = dacos(-1.d0)
        omega = 2.d0*pi*frec   
 
        
        allocate(domains%mododiel(imaxnsd),stat=ierror)
        if (ierror.ne.0) then
          print*,'error: domains%modocond cannot be allocated'
          stop
        endif 
        
        domains%mododiel = .false.  
        
          
       do k=1,nel
!          domains%jsdata(k) = .false.
           nsdk = nsd(k)
!
!          domains%mododiel(nsdk) = .false.
          
 
!        indice recorre todos los subdominios para determinar si dicho subdominio
!        es conductor o no, y si tiene Js asignada o no         
! 	     do i=1,sourcevol%numero
!      	   if(nsdk.eq.sourcevol%referencias(i)) then	
!             domains%jsdata(k) = .true. 
!      	   endif
!        enddo 
        
        do i=1,conduc%numero
      	if(nsdk.eq.conduc%referencias(i)) then	
            if(conducel(indice,i)< 1.e-12)then
               domains%mododiel(nsdk) = .true.
            endif
      	endif
      enddo
      
      enddo
        
       
        return
        end
