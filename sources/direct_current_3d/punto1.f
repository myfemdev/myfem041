***********************************************************************
*     SUBRUTINA punto1                                                *
*     Halla, mediante interpolacion (o extrapolacion) el valor de "y" *
*     cuando (x,y) esta en la recta que forman los puntos (x1,y1) y   *
*     (x2,y2)                                                         *
***********************************************************************
      SUBROUTINE punto1(x1,y1,x2,y2,x,y)
      
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

         IF (DABS(x2-x1) .LE. 1.d-7) THEN 
            y = (y1+y2)*0.5d0
	   ELSE
            y = (y2-y1)*(x-x1)/(x2-x1)+y1
         END IF
         RETURN

      END