      subroutine facdiag(n,ib,jb,id,a,ca)
***********************************************************************
*   Calculo del precondicionador como la inversa de la diagonal
***********************************************************************
      implicit double precision (a-h,o-z)
      dimension ib(*),jb(*),id(*),a(*),ca(*)
*----------------------------------------------------------------------
      do 1 i=1,ib(n+1)-1
        ca(i)=0d0
 1    continue

      do 2 i=1,n
        ii=id(i)
        ca(ii)=1d0/a(ii)
 2    continue
*----------------------------------------------------------------------
      return
      end
