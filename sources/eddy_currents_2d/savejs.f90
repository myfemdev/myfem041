      subroutine savejs()

	  !use parametros_electros
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
      
      interface 
      subroutine integraj(nel,z,mm,cj,xint)
       implicit double precision(a-h,o-z)
       dimension mm(3,*),z(2,*)
       double complex, allocatable:: cj(:)
       double complex:: xint
      end subroutine
     end interface
      
      integer :: ii, i, k, j, nsdl, iopconduc, ndom
      
      character(len=255) :: cad
      integer :: l
      double complex:: fjs,xintj
	  integer nsdi
      
      
      
      subcncm = dcmplx(0.d0,0.d0)
      subcnrms = dcmplx(0.d0,0.d0)
      
      
       do i = 1, sourcevol%numero


!      - extracción de la submalla (submm, subz)
         call extract_mesh(nver, mm, z, nsd, [sourcevol%referencias(i)], submm, subz, globv, globel)
         subnel = size(submm,2); subnver = size(subz,2)

		 do ii=1,subnver
!        subcncm - complex field Js
           subcnrms(ii) = fjs(subz(1,ii),subz(2,ii),0,i,sourcevol%itipo(i),sourcevol%modo(i),&
		   &sourcevol%vrms(i),sourcevol%vphase(i),sourcevol%etiqueta(i))		 
		   subcncm(ii) = dsqrt(2.d0)*subcnrms(ii)
       	 enddo

	  !Real part current density
          write(cad,*) sourcevol%referencias(i)
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcncm),'Current density (real part)',&
		  &'scalar','node',trim(fichdenscr)//trim(adjustl(cad))//'.vtu')

!Imaginary part current density
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcncm),'Current density (imaginary part)',&
		  &'scalar','node',trim(fichdensci)//trim(adjustl(cad))//'.vtu')
         
          
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcnrms),'RMS Current density',&
		  &'scalar','node',trim(fichdenscm)//trim(adjustl(cad))//'.vtu')
		  
!         Computing RMS intensity		  
		  		  
		  call integraj(subnel,subz,submm,subcnrms,xintj)

		  open(11,file=trim(fichintens)//trim(adjustl(cad)),form='formatted')
		  !write(11,*)'Subdomain',sourcevol%referencias(i)
	      write(11,20)'RMS current intensity (A): ',cdabs(xintj)
	      write(11,20)'Phase (degrees): ',datan2(dimag(xintj),dreal(xintj))*180.d0/pi 
	      write(11,20),'Real part (A): ',dsqrt(2.d0)*dreal(xintj)
	      write(11,20),'Imaginary part (A): ',dsqrt(2.d0)*dimag(xintj)
	      
	      
          close(11)
		  
		   !Joule effect
		   
		   nsdl = sourcevol%referencias(i)
		   !Calculo de la conductividad eléctrica

            iopconduc=0
 	          do ii=1,conduc%numero
      	        if(nsdl.eq.conduc%referencias(ii)) then	
                   iopconduc=conduc%iopcond(ii)
                   ndom=ii
      	        endif
             enddo
!       Computing Lorentz force with the complex field subcncm (Jcomplex = sqrt(2)*fjs)

         call florenfor(nsdl,ndom)
             
!        Computing Joule effect

         subcaux = dcmplx(0.d0,0.d0)
	
          do ii=1,subnver
		    subcaux(ii) = cdabs(subcncm(ii))**2/(2.d0*conducel(nsdl,ndom))
		  enddo
		  
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcaux),'Active power',&
		  &'scalar','node',trim(fichjoule)//trim(adjustl(cad))//'.vtu')
		  
          call integraj(subnel,subz,submm,subcaux,xintj)
		  
		  open(11,file=trim(fichpot)//trim(adjustl(cad)),form='formatted')
		  write(11,30)cdabs(xintj)
          close(11)		  
	  
		  
!Se guardan B y H para estos subdominios, para evitar extraer la malla de nuevo
!      - extracción del subcampo (subc)
       call extract_field(globv, globel, cb, subc, 'cell', 2)
      
!      - paso del campo a nodos (subcn)
       call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
       write(cad,*) nsdl
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
       call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
       write(cad,*) nsdl
       call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Magnetic field (real part)', &
       'vector','node',trim(fichcampmr)//trim(adjustl(cad))//'.vtu') 
       
       call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcn),'Magnetic field (imaginary part)', &
       'vector','node',trim(fichcampmi)//trim(adjustl(cad))//'.vtu')     
       
       call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Magnetic field (modulus)', &
       'vector','node',trim(fichcampmm)//trim(adjustl(cad))//'.vtu')  
       
       subc = dcmplx(0.d0,0.d0)
                 
          do ii=1,subnver
            subc(ii) = sol(globv(ii))	
          enddo          
          
          write(cad,*) nsdl
          
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subc),'Magnetic vector potential (real part)  ',&
		  &'scalar','node',trim(fichsolr)//trim(adjustl(cad))//'.vtu')
        	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subc),'Magnetic vector potential (imaginary part)',&
		  &'scalar','node',trim(fichsoli)//trim(adjustl(cad))//'.vtu')
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subc),'Magnetic vector potential (modulus)',&
		  &'scalar','node',trim(fichsolm)//trim(adjustl(cad))//'.vtu') 		  
     enddo
     
20  FORMAT((A28,G13.7)) 
 
 30  FORMAT((G13.7))

	 return

	 end
