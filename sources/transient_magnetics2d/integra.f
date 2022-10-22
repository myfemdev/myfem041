      subroutine integra(nel,mm,z,xint)
****************************************************************
*     Calculo de la integral de una funcion por un campo escalar
*     P1 a trozos y continuo
*     Se utiliza la formula de los vertices
****************************************************************

      use derivados
      use potenciales_vol
      
      implicit double precision(a-h,o-z)
      character*255 etiqueta
      dimension mm(3,*),z(2,*),xint(*)

      
      if(potencial_dat_vol%numero.le.0) then
        print*,'error: no hay ningun dominio con potencial como dato'
        stop
      end if
      
   
      do i = 1, potencial_dat_vol%numero
        xint(i)=0.d0
        modo=potencial_dat_vol%modo1(i)
        valor=potencial_dat_vol%valor1(i)
        etiqueta=potencial_dat_vol%etiqueta1(i)
        do j = 1, nelempo(i)
          k = ensdpo(i,j)
!          modo=potencial_dat_vol%modo1(i)
!          valor=potencial_dat_vol%valor1(i)
!          etiqueta=potencial_dat_vol%etiqueta1(i)

        mm1=mm(1,k)
        mm2=mm(2,k)
        mm3=mm(3,k)

*    calculos previos

        ab=z(1,mm2)-z(1,mm1)
        bc=z(2,mm2)-z(2,mm1)
        cd=z(1,mm3)-z(1,mm1)
        de=z(2,mm3)-z(2,mm1)
 
*     coef.y jacobiano de la matriz de paso al elem. de referencia
 
        det=ab*de-bc*cd
        xint(i)=xint(i)+(sigma_vol(i,modo,valor,
     &                            etiqueta)+
     &             sigma_vol(i,modo,valor,
     &                            etiqueta)+
     &             sigma_vol(i,modo,valor,
     &                            etiqueta))*det/6.d0

        end do
      end do

      return
      end

