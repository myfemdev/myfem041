      subroutine procb2(m,bmat,ib,jb,alpha,q,p)
*******************************************************************
* GOAL:      p:=p+alpha*A*q
****
* IN       bmat ----> matriz (m*n) almacenada en codigo morse
*          q -------> vector (n*1)
*          m -------> numero de filas de la matriz bmat
*          n -------> numero de columnas de la matriz bmat (no se usa!)
*          ib ------> puntero para las filas de bmat
*          jb ------> puntero para las columnas de bmat
*	   alpha----> alfa
* OUT:     p -------> 
*******************************************************************
      implicit double precision (a-h,o-z)
      dimension bmat(*),ib(*),jb(*),q(*),p(*)
*------------------------------------------------------------------
      do 1 i=1,m
        sum=0.d0
        do 2 j=ib(i)+1,ib(i+1)
          sum=sum+bmat(j)*q(jb(j))
 2	continue
	p(i)=p(i)+alpha*sum
 1    continue
*------------------------------------------------------------------
      return
      end
