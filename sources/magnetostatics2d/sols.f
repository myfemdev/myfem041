      subroutine sols(c,b,n,mua)
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
c    programador    p. quintela , j. trastoy
c    -----------    universidad de santiago.spain
c                   (ene.1982)
c********************************************************************
      implicit double precision (a-h,o-z)
      dimension c(*),b(*),mua(*)
      do 1 i=2,n
      i1=mua(i)+1
      i2=mua(i+1)
      i3=i2-i1
      if(i3.eq.0) go to 1
      do 2 j=1,i3
    2 b(i)=b(i)-c(i2-j)*b(i-j)
    1 continue
      do 3 k=1,n
      ik=mua(k+1)
    3 b(k)=b(k)/c(ik)
      do 4 k=n,2,-1
      i2=mua(k+1)
      i3=i2-mua(k)-1
      if(i3.eq.0) go to 4
      do 5 il=1,i3
    5 b(k-il)=b(k-il)-c(i2-il)*b(k)
    4 continue
      return
      end
