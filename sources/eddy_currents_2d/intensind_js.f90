      SUBROUTINE intensind_js(ind,xintj_js,baux2)
!Calculo de la intensidad que atraviesa cada "inductor" debido a la parte fija del problema 
! (stranded conductors)
      

      use malla_2DP1
      use electros_2D
      use derivados
      use voltage_drop
      use conductividad
      use intensity_input
    

      implicit double precision (a-h,o-z)   

	  double complex aux(3),xintj_js(:),baux2(:),zi
	  
	  xintj_js = dcmplx(0.d0,0.d0)
	  
	  zi = dcmplx(0.d0,1.d0)
	  
	  
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

            do l=1,3
               aux(l) =-sigmak*zi*omega*baux2(mm(l,k))    
            enddo
            
            xintj_js(i) = xintj_js(i) + (aux(1)+aux(2)+aux(3))*det/6.d0
 

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

!Calculo de la conductividad eléctrica

            iopconduc=0
 	          do ii=1,conduc%numero
      	        if(nsd(k).eq.conduc%referencias(ii)) then	
                   iopconduc=conduc%iopcond(ii)
                   ndom=ii
      	        endif
             enddo


			 sigmak = conducel(nsd(k),ndom)
                
             do l=1,3
               aux(l) = -sigmak*zi*omega*baux2(mm(l,k))    
             enddo

             xintj_js(i+num_inputsi) = xintj_js(i+num_inputsi) + (aux(1)+aux(2)+aux(3))*det/6.d0

           enddo
        enddo
      end if
      
      
      
 
         

      return
      end          