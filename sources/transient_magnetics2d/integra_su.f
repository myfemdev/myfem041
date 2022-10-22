      subroutine integra_su(nel,mm,z,xint)
****************************************************************
*     Calculo de la integral de una funcion por un campo escalar
*     P1 a trozos y continuo
*     Se utiliza la formula de los vertices
****************************************************************

      use derivados
      use potenciales_sur
      
      implicit double precision(a-h,o-z)
      character*255 etiqueta
      dimension mm(3,*),z(2,*),xint(*)
      
!       ELECCION DEL METODO PARA LA INTEGRAL EN LA FRONTERA 
        
      if (iopf .EQ. 1) then
          xnod11 = -0.577350269189626d0
          xnod22 =  0.577350269189626d0
      elseif (iopf .EQ. 2) then
          xnod11 = -1.d0
          xnod22 =  1.d0
      endif
      
      xnod1 = (1. + xnod11) * 0.5d0
      xnod2 = (1. + xnod22) * 0.5d0

      
      if(potencial_dat_sur%numero.le.0) then
        print*,'error: no hay ningun dominio con potencial como dato'
        stop
      end if
      
      do i=1,potencial_dat_sur%numero
        xint(i)=0.d0
      end do
      
   

      do j=1,ntapo
                  
        iref = nrapo(j)
        ipo  = indblopo(j)
        modo=potencial_dat_sur%modo1(ipo)
        valor=potencial_dat_sur%valor1(ipo)
        etiqueta=potencial_dat_sur%etiqueta1(ipo)
        espesor=potencial_dat_sur%valor0(ipo)


C                       EXTREMOS DE LAS ARISTAS
                       
        nov1 = nvapo(1,j)
        nov2 = nvapo(2,j)
C                       NODOS DE INTEGRACION Y TEMPERATURA INTERPOLADA                       
        x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
        y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
        x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
        y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      

C                       LONGITUD DEL INTERVALO

        delta = dsqrt((z(1,nov2) - z(1,nov1))**2 +
     &              (z(2,nov2) - z(2,nov1))**2)
 
        xint(ipo)=xint(ipo)+ delta*0.5d0*(sigma_sur(ipo,modo,
     &          valor,etiqueta)+ sigma_sur(ipo,modo,
     &          valor,etiqueta))*espesor
 
      end do

      return
      end

