      subroutine integra_sa(bt)
****************************************************************
*     Calculo de la integral de una funcion por un campo escalar
*     P1 a trozos y continuo
*     Se utiliza la formula de los vertices
****************************************************************

      use potenciales_vol
      use malla_2DP1
      use electros_2D
      
      implicit double precision(a-h,o-z)
      dimension bt(*)
      character*255 etiqueta
      
      if(potencial_dat_vol%numero.le.0) then
        print*,'error: there are no domains with potential as data'
        stop
      end if
      
   
      do i = 1, potencial_dat_vol%numero
        xintsavol(i)=0.d0
        do j = 1, nelempo(i)
          k = ensdpo(i,j)
          modo=potencial_dat_vol%modo1(i)
          valor=potencial_dat_vol%valor1(i)
          etiqueta=potencial_dat_vol%etiqueta1(i)

 
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
        xintsavol(i)=xintsavol(i)+(sigma_vol(i,modo,valor,
     &                            etiqueta)*bt(mm1)+
     &             sigma_vol(i,modo,valor,
     &                            etiqueta)*bt(mm2)+
     &             sigma_vol(i,modo,valor,
     &                            etiqueta)*bt(mm3))*det/6.d0

        end do
      end do

      return
      end

