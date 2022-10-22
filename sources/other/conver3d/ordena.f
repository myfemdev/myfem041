      subroutine ordena(n,x)
      implicit double precision (a-h,o-z)
      dimension x(*)

      do 1 i=1,n
        do 2 j=1,n-1
            if(x(j).gt.x(j+1)) then
               y=x(j+1)
               x(j+1)=x(j)
               x(j)=y
             end if
   2    continue
   1  continue
      return
      end
