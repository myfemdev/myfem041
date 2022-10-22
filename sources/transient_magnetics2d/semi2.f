*****************************************************************
*      incorporacion de intensidades en segundo miembro         *
*****************************************************************
      SUBROUTINE semi2(time)
      

      use malla_2DP1
      use electros_2D
      use potenciales_vol
      use potenciales_sur
      use tiempo
    

      implicit double precision (a-h,o-z)   
      character*255 etiqueta   
         

      if (potencial_dat_vol%numero.gt.0) then
      
        do i = 1, potencial_dat_vol%numero
        
          do j = 1, nelempo(i)
            k = ensdpo(i,j)    
            mm1 = mm(1,k)
            mm2 = mm(2,k)
            mm3 = mm(3,k)
                    
C                  CALCULOS PREVIOS

            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

            det = ab*de - bc*cd 
            
            
            if(potencial_dat_vol%ncouples.gt.0)then
              do i1=1, potencial_dat_vol%ncouples
                if(i.eq.potencial_dat_vol%icouple(1,i1)) then
                
                  modo=potencial_dat_vol%modo4(i1)
                  valor=potencial_dat_vol%valor4(i1)
                  etiqueta=potencial_dat_vol%etiqueta4(i1)
                
                  xint=(1.d0/deltat*xintsavol(i)/xintsvol(i)
     &           -1.d0/deltat*xintsavol(potencial_dat_vol%icouple(2,i1))
     &            /xintsvol(potencial_dat_vol%icouple(2,i1))       
     & 		      -voltaje_vol(time,i1,modo,valor,etiqueta))/
     &            (1.d0/xintsvol(i)+1.d0/xintsvol(potencial_dat_vol%
     &            icouple(2,i1)))/areapo(i)*det/6.d0
     
                  goto 1
                
                elseif(i.eq.potencial_dat_vol%icouple(2,i1)) then
                
                  modo=potencial_dat_vol%modo4(i1)
                  valor=potencial_dat_vol%valor4(i1)
                  etiqueta=potencial_dat_vol%etiqueta4(i1)
                
                  xint=(1.d0/deltat*xintsavol(i)/xintsvol(i)
     &           -1.d0/deltat*xintsavol(potencial_dat_vol%icouple(1,i1))
     &            /xintsvol(potencial_dat_vol%icouple(1,i1))        
     & 		      +voltaje_vol(time,i1,modo,valor,etiqueta))/
     &            (1.d0/xintsvol(i)+1.d0/xintsvol(potencial_dat_vol
     &            %icouple(1,i1)))/areapo(i)*det/6.d0
     
                  go to 1
     
                endif
                
              enddo
            endif 
          
            modo=potencial_dat_vol%modo2(i)
            valor=potencial_dat_vol%valor2(i)
            etiqueta=potencial_dat_vol%etiqueta2(i)
   
      		xint=(1.d0/deltat*xintsavol(i)
     & 		     - pot_vol(time,i,modo,valor,etiqueta)*xintsvol(i))
     &            /areapo(i)*det/6.d0
     
1           b(mm1) = b(mm1) + xint
            b(mm2) = b(mm2) + xint
            b(mm3) = b(mm3) + xint

          enddo
        enddo
      end if
          
         

      if(potencial_dat_sur%numero.gt.0) then
       
        do j=1,ntapo
                  
          ipo=indblopo(j)


C                       EXTREMOS DE LAS ARISTAS
                       
          nov1 = nvapo(1,j)
          nov2 = nvapo(2,j)
                      

C                       LONGITUD DEL INTERVALO

          delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
          if(potencial_dat_sur%ncouples.gt.0)then
              do i=1, potencial_dat_sur%ncouples
                if(ipo.eq.potencial_dat_sur%icouple(1,i)) then
                
                  modo=potencial_dat_sur%modo4(i)
                  valor=potencial_dat_sur%valor4(i)
                  etiqueta=potencial_dat_sur%etiqueta4(i)
                
                  xint= (1.d0/deltat*xintsasur(ipo)/xintssur(ipo)
     &            -1.d0/deltat*xintsasur(potencial_dat_sur%icouple(2,i))
     &             /xintssur(potencial_dat_sur%icouple(2,i))      
     & 		       -voltaje_sur(time,i,modo,valor,etiqueta))/
     &             (1.d0/xintssur(ipo)+1.d0/xintssur(potencial_dat_sur%
     &             icouple(2,i)))/xlongpo(ipo)*delta*0.5d0 
          
                  go to 2
                
                elseif(ipo.eq.potencial_dat_sur%icouple(2,i)) then
          
                  modo=potencial_dat_sur%modo4(i)
                  valor=potencial_dat_sur%valor4(i)
                  etiqueta=potencial_dat_sur%etiqueta4(i)  
     
                  xint=(1.d0/deltat*xintsasur(ipo)/xintssur(ipo)
     &            -1.d0/deltat*xintsasur(potencial_dat_sur%icouple(1,i))
     &             /xintssur(potencial_dat_sur%icouple(1,i))        
     & 		       +voltaje_sur(time,i,modo,valor,etiqueta))/
     &             (1.d0/xintssur(ipo)+1.d0/xintssur(potencial_dat_sur%
     &             icouple(1,i)))/xlongpo(ipo)*delta*0.5d0 
     
                go to 2
     
                endif
             enddo
          endif
          
          modo=potencial_dat_sur%modo2(ipo)
          valor=potencial_dat_sur%valor2(ipo)
          etiqueta=potencial_dat_sur%etiqueta2(ipo)
     
          xint= (1.d0/deltat*xintsasur(ipo)
     & 		     - pot_sur(time,ipo,modo,valor,etiqueta)*xintssur(ipo))
     &            /xlongpo(ipo)*delta*0.5d0 
     

2         b(nov1)= b(nov1)+xint 
          b(nov2)= b(nov2)+xint
           
        end do
      end if

   
      end
