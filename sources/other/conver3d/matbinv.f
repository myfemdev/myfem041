      subroutine matbinv
*****************************************************************
* GOAL:     fichero que calcula la inversa y el determinante de
*          la matriz Bk (3*3) que define la aplicacion que lleva
*          un tetraedro de la malla al tetraedro de referencia.
*           Bk es la matriz de columnas (a1-a4|a2-a4|a3-a4), 
*          siendo ai los vertices de tetraedro de la malla.
****
* IN:     commons > datos de la malla
*
* OUT:    binv ---> 9 elementos de la matriz Bk inversa
*         det(k) -> determinante de la matriz Bk
*****************************************************************
      include 'fcommon'
      dimension x(4),y(4),w(4)
*----------------------------------------------------------------
      do 1 k=1,nel
*  Definimos los vectores diferencias de coordenadas de los
*  vertices con el vertice 1 pues la matriz Bk es la de columnas
*  (a1-a4|a2-a4|a3-a4) siendo ai los vertices de tetraedro de 
*  la malla. Posteriormente calculamos el determinante de Bk.
*
       do 2 i=2,4
        x(i)=z(1,mm(i-1,k))-z(1,mm(4,k))
        y(i)=z(2,mm(i-1,k))-z(2,mm(4,k))
        w(i)=z(3,mm(i-1,k))-z(3,mm(4,k))
 2     continue
*
       det(k)=-(x(2)*(y(3)*w(4)-y(4)*w(3))-x(3)*(y(2)*w(4)-y(4)*w(2))
     &        +x(4)*(y(2)*w(3) -y(3)*w(2)))
*
*  Calculo de la matriz inversa de Bk
*
       binv(1,1,k)=y(3)*w(4)-y(4)*w(3)
       binv(1,2,k)=-x(3)*w(4)+x(4)*w(3)
       binv(1,3,k)=x(3)*y(4)-x(4)*y(3)
       binv(2,1,k)=-y(2)*w(4)+y(4)*w(2)
       binv(2,2,k)=x(2)*w(4)-x(4)*w(2)
       binv(2,3,k)=-x(2)*y(4)+x(4)*y(2)
       binv(3,1,k)=y(2)*w(3)-y(3)*w(2)
       binv(3,2,k)=-x(2)*w(3)+x(3)*w(2)
       binv(3,3,k)=x(2)*y(3)-x(3)*y(2)
*
*  A nosotros nos interesaria tener det(k) positivo. Como en
*  este caso eso no sucede, pues implicaria tener que definir
*  el vol(k)=-det(k)/6d0, lo que hacermos el cambiar el signo
*  del determinante y luego devolverselo para calcular binv.
       do 3 j2=1,3
        do 3 j1=1,3
         binv(j1,j2,k)=-binv(j1,j2,k)/det(k)
 3     continue
* Comprobacion de que los determinantes salen positivos
       if (det(k).le.1d-15) then
         print*,'El elemento ',k,' tiene volumen negativo = ',
     &          det(k)*6d0
         stop 'malla mal construida(por volumen negativo o nulo)'
       endif
 1    continue
*----------------------------------------------------------------
      return
      end
