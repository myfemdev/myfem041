      subroutine chol(n,mua,c)
c********************************************************************
c                 subprograma   chol
c                 ------------------
c********************************************************************
c   objetivo
c   --------   factorizacion cholesky de una matriz simetrica dada
c              en forma de vector (metodo sky-line)
c********************************************************************
c   parametros
c   ----------
c    entrada   n     dimension de la matriz en forma normal
c              mua   vector de punteros (metodo sky-line)
c              c     vector que contiene a la matriz
c    salida    c     vector matricial  factorizado
c********************************************************************
c    programador    p. quintela , j. trastoy
c    -----------    universidad de santiago.spain
c                   (ene.1982)
c********************************************************************
      implicit double precision (a-h,o-z)
      dimension mua(*),c(*)
      do 1 i=2,n
      lii=mua(i)+1
      lsi=mua(i+1)
      do 1 j=lii,lsi
      kcj=i-lsi+j
      lskcj=mua(kcj+1)
      nfkcj=lskcj-mua(kcj)
      nsum=min(j-lii,nfkcj-1)
      if(nsum)3,3,5
    5 do 2 k=1,nsum
      i1=kcj+1-k
      kl=mua(i1)
    2 c(j)=c(j)-c(j-k)*c(lskcj-k)*c(kl)
    3 if(j-lsi)4,1,4
    4 c(j)=c(j)/c(lskcj)
    1 continue
      return
      end
      