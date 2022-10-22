!*****************************************************************
!*        CONSTRUCION DEL SEGUNDO MIEMBRO DEL PROBL. TERMICO     *
!*****************************************************************
      SUBROUTINE semi1()
      

      use malla_2DP1
      use electros_2D
      use sourcevolumic
      use sourcesurface
      use neumann
      use derivados
      use potenciales_vol
      use potenciales_sur
	  use tiempo
    

      implicit double precision (a-h,o-z)
      character*255 etiqueta 
      


!        ELECCION DEL METODO PARA LA INTEGRAL EN LA FRONTERA 
        
      if (iopf .EQ. 1) then
          xnod11 = -0.577350269189626d0
          xnod22 =  0.577350269189626d0
      elseif (iopf .EQ. 2) then
          xnod11 = -1.d0
          xnod22 =  1.d0
      endif
      
          xnod1 = (1. + xnod11) * 0.5d0
          xnod2 = (1. + xnod22) * 0.5d0
         
!        INICIALIZACION A CERO DEL VECTOR b   
      
      do i=1,nver
         b(i) = 0.d0
      enddo



!        OPCION PARA CARGAS VOLUMICAS
!        forma de calcular utilizando los arrays de sourcevol
      do i = 1, sourcevol%numero
        do j = 1, nelem(i)
          k = ensd(i,j)    
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
                        
          if (iop .eq. 1) then

!              CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO

            xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
            ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0


      		xint =f(ti,xbar,ybar,nsd(k),i,sourcevol%itipo(i),sourcevol%modo(i),&
     &            sourcevol%valor(i),sourcevol%etiqueta(i)) * det / 6.D0

            b(mm1) = b(mm1) + xint
            b(mm2) = b(mm2) + xint
            b(mm3) = b(mm3) + xint

          elseif (iop .eq. 2) then

!              INTEGRAL EN EL TRIANGULO K (FORMULA DE LOS VERTICES)

 
      	    f1=f(ti,z(1,mm1),z(2,mm1),nsd(k),i,sourcevol%itipo(i),&
     &       sourcevol%modo(i),sourcevol%valor(i),sourcevol%etiqueta(i))
           	f2=f(ti,z(1,mm2),z(2,mm2),nsd(k),i,sourcevol%itipo(i),&
     &       sourcevol%modo(i),sourcevol%valor(i),sourcevol%etiqueta(i))
           	f3=f(ti,z(1,mm3),z(2,mm3),nsd(k),i,sourcevol%itipo(i),&
     &       sourcevol%modo(i),sourcevol%valor(i),sourcevol%etiqueta(i))

      		b(mm1)= b(mm1)+f1*det/6.D0
            b(mm2)= b(mm2)+f2*det/6.D0
            b(mm3)= b(mm3)+f3*det/6.D0

          else if (iop.eq.3) then

!              INTEGRAL EN EL TRIANGULO K (FORMULA DE LOS PUNTOS MEDIOS)

             x12 = (z(1,mm1) + z(1,mm2)) * 0.5D0
             x23 = (z(1,mm2) + z(1,mm3)) * 0.5D0
             x31 = (z(1,mm3) + z(1,mm1)) * 0.5D0
             y12 = (z(2,mm1) + z(2,mm2)) * 0.5D0
             y23 = (z(2,mm2) + z(2,mm3)) * 0.5D0
             y31 = (z(2,mm3) + z(2,mm1)) * 0.5D0


             xint12 = f(ti,x12,y12,nsd(k),i,sourcevol%itipo(i),&
     &         sourcevol%modo(i),sourcevol%valor(i),&
     &         sourcevol%etiqueta(i)) * det 
             xint23 = f(ti,x23,y23,nsd(k),i,sourcevol%itipo(i),&
     &         sourcevol%modo(i),sourcevol%valor(i),&
     &         sourcevol%etiqueta(i)) * det 
             xint31 = f(ti,x31,y31,nsd(k),i,sourcevol%itipo(i),&
     &         sourcevol%modo(i),sourcevol%valor(i),&
     &         sourcevol%etiqueta(i)) * det 
             b(mm1)= b(mm1) + (xint12 + xint31) / 12.d0
             b(mm2)= b(mm2) + (xint12 + xint23) / 12.d0 
             b(mm3)= b(mm3) + (xint23 + xint31) / 12.d0 

           end if
         enddo
       enddo
          
          
!           INTEGRAL EN LA FRONTERA FUENTE SUPERFICIAL 
       
      do j=1,ntasu
                  
        iref = nrasu(j)
        isu=indblosu(j)

!                       EXTREMOS DE LAS ARISTAS
                       
        nov1 = nvasu(1,j)
        nov2 = nvasu(2,j)
!                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
        x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
        y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
        x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
        y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

!                       LONGITUD DEL INTERVALO

        delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +&
     &                (z(2,nov2) - z(2,nov1))**2)
 
        b(nov1) = b(nov1) + delta *  0.5d0 * &
     &   (gs(ti,x1,y1,iref,isu,sourcesur%itipo(isu),sourcesur%modo(isu),&
     &   sourcesur%valor(isu),sourcesur%etiqueta(isu))&
     &   *xnod2 + &
     &   gs(ti,x2,y2,iref,isu,sourcesur%itipo(isu),sourcesur%modo(isu),&
     &   sourcesur%valor(isu),sourcesur%etiqueta(isu))&
     &   *xnod1)
     
        b(nov2) = b(nov2) + delta *  0.5d0 * &
     &    (gs(ti,x1,y1,iref,isu,sourcesur%itipo(isu),sourcesur%modo(isu),&
     &    sourcesur%valor(isu),sourcesur%etiqueta(isu))&
     &    *xnod1 + &
     &     gs(ti,x2,y2,iref,isu,sourcesur%itipo(isu),sourcesur%modo(isu),&
     &    sourcesur%valor(isu),sourcesur%etiqueta(isu))&
     &    *xnod2)
     

     
      end do
  
!          INTEGRAL EN LA FRONTERA NEUMANN 

    
      do j=1,ntane
                   
        iref = nrane(j)
        ine  = indblone(j)

!                       EXTREMOS DE LAS ARISTAS
                       
        nov1 = nvane(1,j)
        nov2 = nvane(2,j)

!                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
        x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
        y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
        x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
        y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

!                       LONGITUD DE LA ARISTA

        delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +&
     &                (z(2,nov2) - z(2,nov1))**2)
     
     
        b(nov1) = b(nov1) + delta *  0.5d0 * &
     &   (g(ti,x1,y1,iref,ine,neumann_bc%modo(ine),&
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))&
     &   *xnod2 + &
     &   g(ti,x2,y2,iref,ine,neumann_bc%modo(ine),&
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))&
     &   *xnod1)
     
        b(nov2) = b(nov2) + delta *  0.5d0 * &
     &   (g(ti,x1,y1,iref,ine,neumann_bc%modo(ine),&
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))&
     &   *xnod1 + &
     &   g(ti,x2,y2,iref,ine,neumann_bc%modo(ine),&
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))&
     &   *xnod2)
     
      end do
      
       if (potencial_dat_vol%numero.gt.0) then
      
        do i = 1, potencial_dat_vol%numero
        
          modo=potencial_dat_vol%modo2(i)
          valor=potencial_dat_vol%valor2(i)
          etiqueta=potencial_dat_vol%etiqueta2(i)
          do j = 1, nelempo(i)
            k = ensdpo(i,j)    
            mm1 = mm(1,k)
            mm2 = mm(2,k)
            mm3 = mm(3,k)
                    
!                 CALCULOS PREVIOS

            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

!           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

            det = ab*de - bc*cd  
       		xint= fpo(i,potencial_dat_vol%valor3(i)) * det / 6.D0
           

            b(mm1) = b(mm1) + xint
            b(mm2) = b(mm2) + xint
            b(mm3) = b(mm3) + xint

          enddo
        enddo
      end if
      
       if(potencial_dat_sur%numero.gt.0) then
       
        do j=1,ntapo
                  
          iref = nrapo(j)
          ipo=indblopo(j)

!                       EXTREMOS DE LAS ARISTAS
                       
          nov1 = nvapo(1,j)
          nov2 = nvapo(2,j)
!                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
          x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
          y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
          x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
          y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

!                       LONGITUD DEL INTERVALO

          delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +&
     &                (z(2,nov2) - z(2,nov1))**2)
     
          b(nov1)= b(nov1)+gspo(ipo,potencial_dat_sur%valor3(ipo))&
     &            *delta * 0.5d0 
          b(nov2)= b(nov2)+gspo(ipo,potencial_dat_sur%valor3(ipo))&
     &            *delta * 0.5d0    
        end do
      end if

 
      return      
      end
