************************************************************************
*              Efectua el ensamblado de matrices		               *
*              Matriz compleja!                                        *
************************************************************************

      subroutine ens(mua1,e,n,c)
      implicit double precision(a-h,o-z)
	double complex e(3,3),c(*)
      dimension n(*),mua1(*)

      do 1 i=1,3

         do 2 j=1,3

         if(n(j).gt.n(i)) goto 2

         l=mua1(n(i)+1)-n(i)+n(j)

         c(l)=c(l)+e(i,j)

  2      continue

1     continue

      return
      end
