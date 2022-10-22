      subroutine blomat(c,mua)
      use dirichlet
      implicit double precision (a-h,o-z)
      dimension mua(*)  
      double complex c(*)
      
       
      if(nvdi.gt.0) then
        do 1 i=1,nvdi
           c(mua(nvd(i)+1))=(1d50,0.d0)
1       continue  
      endif

      return
      end
