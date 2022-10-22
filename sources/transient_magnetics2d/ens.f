******                                                 ******
******esta subrutina efectua el ensamblado de matrices ******
******                                                 ******

      subroutine ens(mua,e,n,c)
      implicit double precision (a-h,o-z)
      dimension e(3,3),n(1),c(1),mua(1)

      do 1 i=1,3
      do 2 j=1,3

      if(n(j).gt.n(i)) goto 2
      l=mua(n(i)+1)-n(i)+n(j)
      c(l)=c(l)+e(i,j)
2     continue
1     continue

      return
      end
