
      
module interf_impedancia	  
      
       interface
        SUBROUTINE semiauxa(ind,baux) 
        use malla_2DP1
        use electros_2D
        use derivados
        use voltage_drop
        use conductividad
        use intensity_input
        implicit double precision (a-h,o-z)   
        character*9 etiqueta 
        double complex aux,xint
	    double complex baux(:)
        end subroutine
      end interface
      
      interface
        SUBROUTINE intensind(ind,xintj,baux)
       use malla_2DP1
      use electros_2D
      use derivados
      use voltage_drop
      use conductividad
      use intensity_input
    

      implicit double precision (a-h,o-z)   

	  double complex aux(3),xintj(:),baux(:),zi
        end subroutine
      end interface
      
         interface
        SUBROUTINE intensind_js(ind,xintj_js,baux2)
       use malla_2DP1
      use electros_2D
      use derivados
      use voltage_drop
      use conductividad
      use intensity_input
    

      implicit double precision (a-h,o-z)   

	  double complex aux(3),xintj_js(:),baux2(:),zi
        end subroutine
      end interface
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       interface 
        subroutine bloseg2d(b)
   
      use dirichlet
      use malla_2DP1

      implicit double precision(a-h,o-z)
	double complex b(*)
	double complex h
      end subroutine 
      
      end interface 
      
      
      interface 
      
      subroutine cholc(n,mua,c)


      implicit double precision (a-h,o-z)
      double complex  c(*),sum
      dimension mua(*)
      end subroutine 
      end interface 
      
      interface 
      
           subroutine solsc(c,b,n,mua)


      implicit double precision (a-h,o-z)
      double complex c(*),b(*),sum
      dimension mua(*)
      end subroutine 
      end interface 
      
      interface 
      subroutine blomat(c,mua)
      use dirichlet
      implicit double precision (a-h,o-z)
      dimension mua(*)  
      double complex c(*)
      end subroutine 
      
      end interface 
      
      
      interface
      
         subroutine lupt(md,ndim,ip,iq,a)
         implicit double precision (a-h,o-z)
         complex*16 a,s
         dimension a(md,*),ip(*),iq(*)
         end subroutine
      
      end interface     
!*************************************

      interface 
      subroutine sistluc(md,n,ip,iq,a,v,w)

      implicit double precision (a-h,o-z)
      complex*16 a,s,w,v
      dimension a(md,*),v(*),w(*),ip(*),iq(*) 
      end subroutine 
     end interface      
            
end module interf_impedancia
