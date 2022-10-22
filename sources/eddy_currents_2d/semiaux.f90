      SUBROUTINE semiaux(ind,baux)
      

      use malla_2DP1
      use electros_2D
      use derivados
      use voltage_drop
      use conductividad
      use intensity_input
    

      implicit double precision (a-h,o-z)   
      character*9 etiqueta 

	  double complex aux,xint
	  double complex:: baux(:)
	  
	   if(num_inputsi.gt.0)then

	    do i=1,num_inputsi
		
          do j = 1, nelemint(i)
            k = ensdint(i,j)    
            mm1 = mm(1,k)
            mm2 = mm(2,k)
            mm3 = mm(3,k)
                    
!                  CALCULOS PREVIOS

            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

!           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

            det = ab*de - bc*cd  

		    aux = co(i,ind)
!Calculo de la conductividad eléctrica

            iopconduc=0
 	          do ii=1,conduc%numero
      	        if(nsd(k).eq.conduc%referencias(ii)) then	
                   iopconduc=conduc%iopcond(ii)
                   ndom=ii
      	        endif
             enddo


			 sigmak = conducel(nsd(k),ndom)
   
!En el caso de sigma constante, las tres formulas de cuadratura proporcionan el mismo valor
        	 xint = sigmak*aux*det / 6.

             baux(mm1) = baux(mm1) + xint
             baux(mm2) = baux(mm2) + xint
             baux(mm3) = baux(mm3) + xint 



          enddo
        enddo
      end if

	  
	  if(num_inputsv.gt.0)then

	    do i=1,num_inputsv
		
          do j = 1, nelempo(i)
            k = ensdpo(i,j)    
            mm1 = mm(1,k)
            mm2 = mm(2,k)
            mm3 = mm(3,k)
                    
!                  CALCULOS PREVIOS

            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

!           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

            det = ab*de - bc*cd  

		    aux = co(i+num_inputsi,ind)
!Calculo de la conductividad eléctrica

            iopconduc=0
 	          do ii=1,conduc%numero
      	        if(nsd(k).eq.conduc%referencias(ii)) then	
                   iopconduc=conduc%iopcond(ii)
                   ndom=ii
      	        endif
             enddo


			 sigmak = conducel(nsd(k),ndom)
   
!En el caso de sigma constante, las tres formulas de cuadratura proporcionan el mismo valor

        	 xint = sigmak*aux*det / 6.D0

             baux(mm1) = baux(mm1) + xint
             baux(mm2) = baux(mm2) + xint
             baux(mm3) = baux(mm3) + xint



          enddo
        enddo
      end if
      
      
    
 
         

      return
      end          