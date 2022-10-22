      subroutine solsc(c,b,n,mua)
c********************************************************************
c                 subprograma   sols
c                 ------------------
c********************************************************************
c   objetivo
c   --------   resolucion de un s.e.l. donde la matriz esta
c              factorizada en forma de vector (sky-line) por
c              el metodo de cholesky
c********************************************************************
c   parametros
c   ----------
c    entrada   c     vector matricial
c              b     vector segundo miembro
c              n     dimension del segundo miembro
c              mua   vector de punteros (metodo sky-line)
c    salida    b     vector solucion del sistema
c********************************************************************

      implicit double precision (a-h,o-z)
      double complex c(*),b(*),sum
      dimension mua(*)

      do 1 i=2,n
        i1=mua(i)+1
        i2=mua(i+1)
        i3=i2-i1
        sum=b(i)
        do 2 j=1,i3
          sum=sum-c(i2-j)*b(i-j)
    2   continue
        b(i)=sum
    1 continue

      do 3 k=1,n
        ik=mua(k+1)
        b(k)=b(k)/c(ik)
    3 continue

      do 4 k=n,2,-1
        i2=mua(k+1)
        i3=i2-mua(k)-1
        do 5 il=1,i3
          b(k-il)=b(k-il)-c(i2-il)*b(k)
    5   continue
    4 continue
    

      return
      end