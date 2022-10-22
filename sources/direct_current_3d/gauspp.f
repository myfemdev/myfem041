c/update add name=gauspp,ssi=84101214
      subroutine gauspp (a,n,np,eps,ires)
      implicit double precision (a-h,o-z)
      dimension a(n,np)
      data zero/1d-30/
      v=0.
      do 10 i=1,n
       do 10 j=1,n
       v=max(v,abs(a(i,j)))
10    continue
      if(v.le.zero) goto 1000
      epsr=eps*v
      do 70 k=1,n-1
       v=abs(a(k,k))
       i0=k
       k1=k+1
        do 20 i=k1,n
         if(v.gt.abs(a(i,k))) goto 20
         v=abs(a(i,k))
         i0=i
20     continue
       if(v.lt.epsr) goto 1000
       do 40 j=k,np
        v=a(k,j)
        a(k,j)=a(i0,j)
        a(i0,j)=v
40     continue
       do 60 i=k1,n
        v=a(i,k)/a(k,k)
        do 60 j=k1,np
         a(i,j)=a(i,j)-a(k,j)*v
60     continue
70     continue
100    continue
       nm1=n-1
       u=a(n,n)
       n1=n+1
       do 130 l=n1,np
        a(n,l)=a(n,l)/u
        do 120 i=1,nm1
         i1=n-i+1
         v=a(n-i,l)
         do 110 j=i1,n
          v=v-a(n-i,j)*a(j,l)
110      continue
         a(n-i,l)=v/a(n-i,n-i)
120     continue
130   continue
      ires=0
      return
1000  continue
      ires=1
      write (6,10000)
10000 format(' gauspp : il y a un pivot nul (< eps) dans a ')
      return
      end
