      
      subroutine postproceso()
!Postprocessing only in dielectric (J = 0, Power = 0, Lorentz = 0)      
     ! use parametros_electros
      use electros_2D
      use fich_electros
      use malla_2DP1
      use derivados
      use module_writeVTU
      use module_fem_extract
      use voltage_drop
      use sourcevolumic
      use conductividad
      use postpro
      implicit none
      
      
      integer :: ii, i, k, j, nsdl,iopconduc,ic,ndom
      
      character(len=255) :: cad
      integer :: l
      double complex :: h
      logical:: insddiel
      double complex:: zi

      zi = dcmplx(0.d0,1.d0)
            
!    bucle en subdominios

     do l = 1, permagrel%numero  !INDUCCION MAGNETICA (parte real, imaginaria y modulo):
     
       nsdl = permagrel%referencias(l)
       insddiel = domains%mododiel(nsdl)
    
    
       if(insddiel)then
!      - extracción de la submalla (submm, subz)

         call extract_mesh(nver, mm, z, nsd, [permagrel%referencias(l)], submm, subz, globv, globel)
         subnel = size(submm,2); subnver = size(subz,2)
         
                  
         call extract_field(globv, globel, sol, subcn, 'node', 1) 
         write(cad,*) permagrel%referencias(l)
          
         call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Magnetic vector potential (real part)  ',&
		 &'scalar','node',trim(fichsolr)//trim(adjustl(cad))//'.vtu')
        	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcn),'Magnetic vector potential (imaginary part)',&
		  &'scalar','node',trim(fichsoli)//trim(adjustl(cad))//'.vtu')
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Magnetic vector potential (modulus)',&
		  &'scalar','node',trim(fichsolm)//trim(adjustl(cad))//'.vtu') 
		  
!      - extracción del subcampo (subc)
         call extract_field(globv, globel, cb, subc, 'cell', 2)
!      - paso del campo a nodos (subcn)
         call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
         write(cad,*) permagrel%referencias(l)
         call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Magnetic induction (real part)', &
         'vector','node',trim(fichindumr)//trim(adjustl(cad))//'.vtu')
       
      
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcn),'Magnetic induction (imaginary part)', &
          'vector','node',trim(fichindumi)//trim(adjustl(cad))//'.vtu')
       
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Magnetic induction (modulus)', &
          'vector','node',trim(fichindumm)//trim(adjustl(cad))//'.vtu')       
 
!CAMPO MAGNETICO:

!      - extracción del subcampo (subc)
          call extract_field(globv, globel, ch, subc, 'cell', 2)
!      - paso del campo a nodos (subcn)
          call cell2node(subnver, submm,subc, subcn)
!      - escritura por nodos y subdominio
          write(cad,*) permagrel%referencias(l)
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Magnetic field (real part)', &
          'vector','node',trim(fichcampmr)//trim(adjustl(cad))//'.vtu') 
       
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcn),'Magnetic field (imaginary part)', &
          'vector','node',trim(fichcampmi)//trim(adjustl(cad))//'.vtu')     
       
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Magnetic field (modulus)', &
          'vector','node',trim(fichcampmm)//trim(adjustl(cad))//'.vtu') 
!Se escribe J = 0 y Joule = 0 en el dielectrico
          do ii=1,subnver
            subcaux(ii) = dcmplx(0.d0,0.d0)
          enddo

!Real part current density
          write(cad,*) permagrel%referencias(l)
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcaux),'Current density (real part)',&
		  &'scalar','node',trim(fichdenscr)//trim(adjustl(cad))//'.vtu')

!Imaginary part current density
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcaux),'Current density (imaginary part)',&
		  &'scalar','node',trim(fichdensci)//trim(adjustl(cad))//'.vtu')

!Modulus RMS current density
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcaux),'RMS Current density',&
		  &'scalar','node',trim(fichdenscm)//trim(adjustl(cad))//'.vtu') 
		  
		    !Joule effect (scalar -- subcaux)
          
         do ii=1,subnver
            subcaux(ii) = dcmplx(0.d0,0.d0)
          enddo	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcaux),'Active power',&
		  &'scalar','node',trim(fichjoule)//trim(adjustl(cad))//'.vtu') 
		  
! Se guarda 0 en la fuerza de Lorentz (vetor --->subcn)
         subcn = dcmplx(0.d0,0.d0)
          print*,'ultimo'
          print*,'size',size(subcaux)
          
          write(cad,*)permagrel%referencias(l)
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Lorentz force', &
       'vector','node',trim(fichflo)//trim(adjustl(cad))//'.vtu')
    
		  
		  
!Se escribe 0 en la intensidad y potencia

          write(cad,*) permagrel%referencias(l)
		  open(11,file=trim(fichpot)//trim(adjustl(cad)),form='formatted')
		  write(11,*)'Domain with NULL electrical conductivity'
		  write(11,*)'Power active (W/m): ',0
		  write(11,*)'Subdomain: ',permagrel%referencias(l)
          close(11)		
          
		  open(11,file=trim(fichintens)//trim(adjustl(cad)),form='formatted')
		  write(11,*)'Domain with NULL electrical conductivity'
		  write(11,*)'RMS Current intensity (A): ',0
          write(11,*),'Real part (A): ',0
	      write(11,*),'Imaginary part (A): ',0		  
		  write(11,*)'Subdomain: ',permagrel%referencias(l)
          close(11)                             
       endif

       enddo    
    
    
    return
    
    end       
    
    
    
 
                