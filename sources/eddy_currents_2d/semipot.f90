      SUBROUTINE semipot()
      

      use malla_2DP1
      use electros_2D
      use derivados
      use voltage_drop
      use conductividad
    

      implicit double precision (a-h,o-z)   
      character*9 etiqueta 

	  double complex aux,xint,zi
	  
	  zi = dcmplx(0.d0,1.d0)

	  
	  if(num_inputsv.gt.0)then

	    do i=1,num_inputsv
		   valor_rms =  inputsv(i)%vrms*dsqrt(2.d0)
		   valor_phase = inputsv(i)%vphase
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

		    aux = valor_rms*(dcos(valor_phase*pi/180.d0) + zi*dsin(valor_phase*pi/180.d0))
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

             b(mm1) = b(mm1) + xint
             b(mm2) = b(mm2) + xint
             b(mm3) = b(mm3) + xint 



          enddo
        enddo
      end if
 
         

      return
      end          