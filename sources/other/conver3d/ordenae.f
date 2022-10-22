      subroutine ordenae(n,ix)
******************************************************************
* GOAL:   Subrutina para ordenar numeros enteros
******************************************************************
      implicit double precision (a-h,o-z)
      dimension ix(*)

      do 1 i=1,n
        do 2 j=1,n-1
            if(ix(j).gt.ix(j+1)) then
               y=ix(j+1)
               ix(j+1)=ix(j)
               ix(j)=y
             end if
   2    continue
   1  continue

      return
      end
