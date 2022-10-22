*****************************************************************
*      incorporacion de intensidades en segundo miembro         *
*****************************************************************
      SUBROUTINE semivar()
      

      use malla_2DP1
      use electros_2D
      use potenciales_vol
      use potenciales_sur
    

      implicit double precision (a-h,o-z)      


C        ELECCION DEL METODO PARA LA INTEGRAL EN LA FRONTERA 
        
      if (iopf .EQ. 1) then
          xnod11 = -0.577350269189626d0
          xnod22 =  0.577350269189626d0
      elseif (iopf .EQ. 2) then
          xnod11 = -1.d0
          xnod22 =  1.d0
      endif
      
          xnod1 = (1. + xnod11) * 0.5d0
          xnod2 = (1. + xnod22) * 0.5d0
         




C        OPCION PARA FUENTES VOLUMICAS
!        forma de calcular utilizando los arrays de sourcevol

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
                        

C              CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO

            xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
            ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0


      		xint= fpo(i,potencial_dat_vol%valor3(i)) * det / 6.D0

            bvar(mm1) = bvar(mm1) + xint
            bvar(mm2) = bvar(mm2) + xint
            bvar(mm3) = bvar(mm3) + xint

         enddo
       enddo
          
          
C           INTEGRAL EN LA FRONTERA FUENTE SUPERFICIAL 

      if(potencial_dat_sur%numero.gt.0) then
       
        do j=1,ntapo
                  
          iref = nrapo(j)
          ipo=indblopo(j)

C                       EXTREMOS DE LAS ARISTAS
                       
          nov1 = nvapo(1,j)
          nov2 = nvapo(2,j)
C                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
          x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
          y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
          x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
          y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

C                       LONGITUD DEL INTERVALO

          delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     

          bvar(nov1)= bvar(nov1)+gspo(ipo,potencial_dat_sur%valor3(ipo))
     &            *delta * 0.5d0  
          bvar(nov2)= bvar(nov2)+gspo(ipo,potencial_dat_sur%valor3(ipo))
     &            *delta * 0.5d0    
        end do
      end if

   
      end
