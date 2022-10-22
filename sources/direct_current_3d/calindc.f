      subroutine calindc(indc,inda)
******************************************************************
* BUT:    definicion de los numeros de los vertices que pertenecen
*         a cada una de las caras y que se definen teniendo en
*         cuenta la ordenacion de Modulef.
********
* OUT:    indc(1,i) -> vector en el que se guardan los 3 numeros
*                      (de los 4 posibles) correspondientes a los
*                      vertices que forman la cara i
*        inda(i,j)  -> vertice i de la arista j, i=1,2, j=1,6. 
* Para cada tetraedro, son numeros fijos que se obtienen de Modulef 
* al hacer el reconv
******************************************************************

       implicit double precision (a-h,o-z)
      dimension indc(3,*),inda(2,*)
*
      indc(1,1)=1
      indc(2,1)=3
      indc(3,1)=2
      indc(1,2)=1
      indc(2,2)=4
      indc(3,2)=3
      indc(1,3)=1
      indc(2,3)=2
      indc(3,3)=4
      indc(1,4)=2
      indc(2,4)=3
      indc(3,4)=4
      
      
! New: indice de cada arista. 

      inda(1,1)=1
      inda(2,1)=2
      inda(1,2)=2
      inda(2,2)=3
      inda(1,3)=3
      inda(2,3)=1
      inda(1,4)=1
      inda(2,4)=4
      inda(1,5)=2
      inda(2,5)=4
      inda(1,6)=3
      inda(2,6)=4      
      return
      end
