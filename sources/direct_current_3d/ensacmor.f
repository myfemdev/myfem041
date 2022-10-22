      subroutine ensacmor(cmat,ib,jb,mm,xmor)
******************************************************************
* GOAL: ensamblado de la matriz C con almacenamiento de
*       tipo morse segun los punteros ib y jb
*****
* IN:    cmat --> matriz elemental
*        mm ----> numeracion de los vertices del elemento
*        jb ----> puntero para las columnas
*        ib ----> puntero para las filas
*
* OUT:   xmor --> ensamblado de cmat en morse
******************************************************************
      implicit double precision(a-h,o-z)
      dimension cmat(4,*),ib(*),jb(*),mm(*),xmor(*)
*-----------------------------------------------------------------
      do 1 i=1,4
        n1=mm(i)
        do 1 j=1,4
          n2=mm(j)
          do 4 l=ib(n1)+1,ib(n1+1)
            if (n2.eq.jb(l)) then
              xmor(l)=xmor(l)+cmat(i,j)
            endif
 4        continue
 1    continue
*-----------------------------------------------------------------
      return
      end
