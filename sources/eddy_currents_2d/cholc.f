
      subroutine cholc(n,mua,c)
c********************************************************************
c                 subprograma   chol
c                 ------------------
c********************************************************************
c   objetivo
c   --------   factorizacion cholesky de una matriz simetrica
c              en forma de vector (metodo sky-line)
c********************************************************************
c   parametros
c   ----------
c    entrada   n     dimension de la matriz en forma normal
c              mua   vector de punteros (metodo sky-line)
c              c     vector que contiene a la matriz
c    salida    c     vector matricial  factorizado
c********************************************************************

      implicit double precision (a-h,o-z)
      double complex  c(*),sum
      dimension mua(*)

      do 1 i=2,n
        lii=mua(i)+1
        lsi=mua(i+1)
        do 2 j=lii,lsi
          kcj=i-lsi+j
          lskcj=mua(kcj+1)
          nfkcj=lskcj-mua(kcj)
          nsum=min(j-lii,nfkcj-1)
          sum=c(j)
          do 3 k=1,nsum
            i1=kcj+1-k
            kl=mua(i1)
            sum=sum-c(j-k)*c(lskcj-k)*c(kl)
    3     continue
          if(j.eq.lsi) then
            c(j)=sum
          else
            c(j)=sum/c(lskcj)
          endif
    2   continue
    1 continue

      return
      end

