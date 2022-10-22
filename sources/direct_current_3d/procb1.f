      subroutine procb1(m,bmat,ib,jb,q,p)
*******************************************************************
* GOAL:      p:=p+A*q
****
* IN       bmat ----> matriz (m*n) almacenada en codigo morse
*          q -------> vector (n*1)
*          m -------> numero de filas de la matriz bmat
*          n -------> numero de columnas de la matriz bmat (no se usa!)
*          ib ------> puntero para las filas de bmat
*          jb ------> puntero para las columnas de bmat
* OUT:     p -------> p
*******************************************************************
      implicit double precision (a-h,o-z)
      dimension bmat(*),ib(*),jb(*),q(*),p(*)
*------------------------------------------------------------------
      do 1 i=1,m
        sum=0.d0
        do 2 j=ib(i)+1,ib(i+1)
          sum=sum+bmat(j)*q(jb(j))
 2	continue
	p(i)=p(i)+sum
 1    continue
*------------------------------------------------------------------
      return
      end
