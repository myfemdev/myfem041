  module interf_eddy
  !Define las interfaces para eddy2d.f
  
  interface 
    subroutine bloseg2d(b)
   
      use dirichlet
      use malla_2DP1

      implicit double precision(a-h,o-z)
	  double complex b(*)
	  double complex h
     end subroutine 
      
   end interface 
      
 !*********************************************     
      interface 
      
        subroutine cholc(n,mua,c)
          implicit double precision (a-h,o-z)
          double complex c(*),sum
          dimension mua(*)
        end subroutine 
      end interface 
 !*********************************************     
      interface 
           subroutine solsc(c,b,n,mua)
             implicit double precision (a-h,o-z)
             double complex c(*),b(*),sum
             dimension mua(*)
          end subroutine 
      end interface 
 !*********************************************     
      interface 
        subroutine blomat(c,mua)
        use dirichlet
        implicit double precision (a-h,o-z)
        dimension mua(*)  
        double complex c(*)
        end subroutine 
      
      end interface 
       !*********************************************     
      end module interf_eddy