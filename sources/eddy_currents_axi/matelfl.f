      subroutine matelfl(z1,z2,z3,be,g1,g2)
      implicit double precision(a-h,o-z)
***********************************************************
*     objetivo: aportacion elemental a la fuerza de Lorentz
*             calculamos grad(be)
***********************************************************
*     parametros:                                         *
*entrada                                                  *
*     z1     vector de coordenadas de no1                 *
*     z2     vector de coordenadas de no2                 *
*     z3     vector de coordenadas de no3                 *
*     be      vector elemental                            *
*salida                                                   *
*     g1    primera componente del gradiente              *
*     g2    segunda componente del gradiente              *
***********************************************************
      dimension z1(2),z2(2),z3(2),be(3)
*
*     calculamos el determinante de la transformacion afin
*
      x21=z2(1)-z1(1)
      b21=z2(2)-z1(2)
      x31=z3(1)-z1(1)
      b31=z3(2)-z1(2)
      det=x21*b31-x31*b21
*
*     calculamos el gradiente 
*
       g1=((b21-b31)*be(1)+b31*be(2)-b21*be(3))/det   
       g2=((x31-x21)*be(1)-x31*be(2)+x21*be(3))/det 
      
*
*     y acabamos
*   
      return
      end
