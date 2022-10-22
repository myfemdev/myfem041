      subroutine norl22d(nel,mm,z,b,xnorl2)
**************************************************************
*     Calculo de la norma 2 del error en una malla bidimensional 
*    con elementos finitos P1
*    Coordenadas cartesianas
*    b: vector de errores absolutos por vertices
*    xnorl2: norma L2(omega)
*    Se utiliza la fórmula de los puntos medios de aristas
***************************************************************
      implicit double precision(a-h,o-z)
      dimension mm(3,*),z(2,*),b(*)

      xnorl2=0.d0
      
      do 1 k=1,nel
 
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
        sum=((b(mm1)+b(mm2))*0.5d0)**2+
     &	  ((b(mm2)+b(mm3))*0.5d0)**2+
     &      ((b(mm1)+b(mm3))*0.5d0)**2



        xnorl2=xnorl2+sum*det/6
 
 1    continue
      xnorl2=dsqrt(xnorl2)
*      print*,'error en norma 2',xnorl2

      return
      end

