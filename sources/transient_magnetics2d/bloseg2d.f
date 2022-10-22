      subroutine bloseg2d(b)
      
      
  ! bloqueo del segundo miembro via tablero interfaz
  ! b: segundo miembro
  ! nrd: numero de referencias a bloquear
  ! irefd: valor de las referencias a bloquear

      use dirichlet
      use malla_2DP1

      implicit double precision(a-h,o-z)
	dimension b(*)
  
      
      do j=1,nvdi
	    b(nvd(j)) = 1.d50*h(time,z(1,nvd(j)),z(2,nvd(j)),
     &         		  nrvd(j),indblo(j),dirichlet_bc%modo(indblo(j)),
     &                dirichlet_bc%valor(indblo(j)),
     &                dirichlet_bc%etiqueta(indblo(j))) 
      enddo
 
      
    


      return
      end