      subroutine calvnoru(x1,x2,x3,xnor,dnor)
******************************************************************
* GOAL:   calculo del vector normal unitario y del modulo del
*         paralelogramo dado por los tres puntos x1, x2 y x3.
*****
* IN:     x1, x2, x3 ---> puntos que definen el plano
*
* OUT:    xnor ---------> vector normal unitario al plano
*         dnor ---------> norma del vector normal
******************************************************************
      implicit double precision (a-h,o-z)
      dimension x1(*),x2(*),x3(*),xnor(*)
      dimension x21(3),x31(3)
*-----------------------------------------------------------------
* Definimos los vectores x21=x2-x1 y x31=x3-x1
      do 1 i=1,3
        x21(i)=x2(i)-x1(i)
        x31(i)=x3(i)-x1(i)
 1    continue

* Calculamos el producto vectorial de x21 y x31 que es el vector
* normal
      xnor(1)=x21(2)*x31(3)-x21(3)*x31(2)
      xnor(2)=x21(3)*x31(1)-x21(1)*x31(3)
      xnor(3)=x21(1)*x31(2)-x21(2)*x31(1)

* Calculamos la norma del vector xnor
      dnor=dsqrt(xnor(1)*xnor(1)+xnor(2)*xnor(2)+xnor(3)*xnor(3))

* Hacemos unitario el vector normal
      xnor(1)=xnor(1)/dnor
      xnor(2)=xnor(2)/dnor
      xnor(3)=xnor(3)/dnor
*------------------------------------------------------------
      return
      end
