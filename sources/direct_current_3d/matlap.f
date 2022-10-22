      subroutine matlap(amat,detk,binv,coefm,coefk,cmat)
*******************************************************************
* GOAL:     construimos el termino correspondienta a integral de
*           gradiente por gradiente.
****
* IN:      detk -----> determinante de la matriz Bk ( =6*vol(k) )
*          cbar -----> tablero de las parciales de las funciones 
*                      de forma
*          binv -----> inversa de la matriz Bk
*          coefm ----> coeficiente del termino de masa
*          coefk ----> coeficiente del termino de rigidez
*
* OUT:     amat -----> matriz elemental (masa - rigidez)
*          cmat -----> matriz elemental que resulta de integrar
*                      un termino de la forma u*v
*          a2mat ----> matriz elemental que resulta de integrar
*                      un termino de la forma grad(u)*grad(v)
*******************************************************************
      implicit double precision (a-h,o-z)
      dimension amat(4,*),a2mat(4,4),cbar(4,3),binv(3,*),cmat(4,*)
*---------------------------------------------------------------------
      call derff(binv,detk,cbar)
      do 1 i=1,4
      do 2 j=1,4
        cmat(i,j)=0d0
        a2mat(i,j)=0d0
        do 3 ii=1,3
         a2mat(i,j)=a2mat(i,j)+cbar(i,ii)*cbar(j,ii)
 3      continue
        a2mat(i,j)=a2mat(i,j)/detk/6d0
        if (i.eq.j) then
          cmat(i,j)=detk/24d0
        endif
        amat(i,j)=coefm*cmat(i,j)+coefk*a2mat(i,j)
 2    continue
 1    continue
*---------------------------------------------------------------------
      return
      end
