c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine ordsg3 (k,sgnper)
c
c                         il ordonne i1, ,i3 par ordre decroissant
c      sgnper = sgn de la permutation qui a ordonne
 
      integer k(3),sgnper
      integer j1
      sgnper=1
1     if(k(1).ge.k(2)) go to 2
      sgnper=-sgnper
      j1=k(1)
      k(1)=k(2)
      k(2)=j1
2     if(k(2).ge.k(3)) go to 3
      sgnper=-sgnper
      j1=k(2)
      k(2)=k(3)
      k(3)=j1
      go to 1
3     continue
      return
      end
