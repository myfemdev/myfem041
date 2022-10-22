      subroutine sistluc(md,n,ip,iq,a,v,w)
************************************************************
*  resolucion de sistemas LU (pivote total)
************************************************************
* entrada:
*     md    primera dimension de la matriz
*     n     orden del la matriz 
*     ip,iq punteros  
*     a     matriz factorizada 
*     v     termino independiente     
*     w     variable de trabajo 
* salida:
*     v     solucion
************************************************************

      implicit double precision (a-h,o-z)
      complex*16 a,s,w,v
      dimension a(md,*),v(*),w(*),ip(*),iq(*)

*  RESOLUCON DEe Lw=Pv
      do 1 i=1,n
        s=0.d0
        do 2 j=1,i-1
          s=s + a(ip(i),iq(j))*w(j)
  2     continue
        w(i) = v(ip(i)) - s
  1   continue

* RESOLUCION DE UQv=w
      v(iq(n)) =  w(n)/a(ip(n),iq(n))
      do 3 i=n-1,1,-1
        s=0.d0
        do 4 j=i+1,n
          s=s + a(ip(i),iq(j))*v(iq(j))
  4     continue
        v(iq(i)) =  (w(i) - s)/a(ip(i),iq(i))
  3   continue

      return
      end 

