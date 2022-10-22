*****************************************************************
*        CONSTRUCION DEL SEGUNDO MIEMBRO                        *
*****************************************************************
      SUBROUTINE semia()
      

      use malla_2DP1
      use electros_2D
      use sourcevolumic
      use neumann
      use derivados
    

      implicit double precision (a-h,o-z)
      double complex xint,f1,f2,f3,xint12,xint23,xint31
      double complex fjs, g, aux
      


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
         
C        INICIALIZACION A CERO DEL VECTOR b   
      
      do i=1,nver
         b(i) = 0.d0
      enddo



C        OPCION PARA FUENTE J_s dada
!        forma de calcular utilizando los arrays de sourcevol
      do i = 1, sourcevol%numero
        do j = 1, nelem(i)
          k = ensd(i,j)    
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
                        
          if (iop .eq. 1) then

C              CALCULO DE LA INTEGRAL MEDIANTE LA FORMULA DEL BARICENTRO

            xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
            ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0

!fjs es RMS, por lo que se multiplica por raiz de 2
      		xint =dsqrt(2.d0)*
     &     		fjs(xbar,ybar,k,i,sourcevol%itipo(i),sourcevol%modo(i),
     &            sourcevol%vrms(i),sourcevol%vphase(i),
     &            sourcevol%etiqueta(i)) * det / 6.D0

            b(mm1) = b(mm1) + xint
            b(mm2) = b(mm2) + xint
            b(mm3) = b(mm3) + xint

    
               end if
         enddo
       enddo
          
  
  
C           INTEGRAL EN LA FRONTERA NEUMANN 

    
      do j=1,ntane
                   
        iref = nrane(j)
        ine  = indblone(j)

C                       EXTREMOS DE LAS ARISTAS
                       
        nov1 = nvane(1,j)
        nov2 = nvane(2,j)

C                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
        x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
        y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
        x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
        y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

C                       LONGITUD DE LA ARISTA

        delta = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
     
        b(nov1) = b(nov1) + delta *  0.5d0 * 
     &   (g(x1,y1,iref,ine,neumann_bc%modo(ine),
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))
     &   *xnod2 + 
     &   g(x2,y2,iref,ine,neumann_bc%modo(ine),
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))
     &   *xnod1)
     
        b(nov2) = b(nov2) + delta *  0.5d0 * 
     &   (g(x1,y1,iref,ine,neumann_bc%modo(ine),
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))
     &   *xnod1 + 
     &   g(x2,y2,iref,ine,neumann_bc%modo(ine),
     &   neumann_bc%valor(ine),neumann_bc%etiqueta(ine))
     &   *xnod2)
     
      end do
            
      end
