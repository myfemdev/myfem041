      subroutine savej_vdata()

      use electros_2D
      use fich_electros
      use malla_2DP1
      use derivados
      use module_writeVTU
      use module_fem_extract
      use voltage_drop
      use intensity_input
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
      
      interface
       subroutine integrajou(nel,z,mm,cj,xint)
       implicit double precision(a-h,o-z)
       dimension mm(3,*),z(2,*)
       double complex, allocatable:: cj(:)
       double complex:: xint
       end subroutine
     end interface 
      
      integer :: ii, i
      
      character(len=255) :: cad
      integer :: l
	  integer:: nsdi
	  double precision:: valor_rms, valor_phase
	  double complex:: aux, zi
	  integer:: iopconduc, ndom
	  double complex:: xintj
	  
	  
	  zi = dcmplx(0.d0,1.d0)
	  pi = dacos(-1.d0)

     
      if (allocated(int_pot)) deallocate(int_pot) 
      allocate(int_pot(num_inputsv), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array int_pot'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
      
      do i=1,num_inputsv
      
          int_pot(i) = dcmplx(0.d0,0.d0)
          
       do l = 1,inputsv(i)%nsubdo
          nsdi = inputsv(i)%subdo_references(l)
      
          valor_rms =  inputsv(i)%vrms*dsqrt(2.d0)/(2.d0*pi)
		  valor_phase = inputsv(i)%vphase   
      
		  aux = valor_rms*(dcos(valor_phase*pi/180.d0) + zi*dsin(valor_phase*pi/180.d0))

!      - extracción de la submalla (submm, subz)
         call extract_mesh(nver, mm, z, nsd, [nsdi], submm, subz, globv, globel)
         subnel = size(submm,2); subnver = size(subz,2)
       	 
       	 !Calculo de la conductividad eléctrica

            iopconduc=0
 	          do ii=1,conduc%numero
      	        if(nsdi.eq.conduc%referencias(ii)) then	
                   iopconduc=conduc%iopcond(ii)
                   ndom=ii
      	        endif
             enddo
             
             
		 do ii=1,subnver
		    if(dabs(subz(1,ii)).gt.1.e-6)then
		      subcncm(ii) = conducel(nsdi,ndom)*(aux/subz(1,ii) -zi*omega*sol(globv(ii)))
            else
               subcncm(ii) = dcmplx(0.d0,0.d0)
            endif 
		   subcnrms(ii) = cdabs(subcncm(ii))/dsqrt(2.d0)
		 enddo
		 
       	
!Real part current density
          write(cad,*) nsdi
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcncm),'Current density (real part)',&
		  &'scalar','node',trim(fichdenscr)//trim(adjustl(cad))//'.vtu')

!Imaginary part current density
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcncm),'Current density (imaginary part)',&
		  &'scalar','node',trim(fichdensci)//trim(adjustl(cad))//'.vtu')

! RMS current density
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcnrms),'RMS Current density',&
		  &'scalar','node',trim(fichdenscm)//trim(adjustl(cad))//'.vtu')
		  
		  
		  
		  call integraj(subnel,subz,submm,subcncm,xintj)
		  
		  open(11,file=trim(fichintens)//trim(adjustl(cad)),form='formatted')

		  !write(11,*)'Subdomain',nsdi
	      write(11,20)'RMS current intensity (A): ',cdabs(xintj)/dsqrt(2.d0)
	      write(11,20)'Phase (degrees): ',datan2(dimag(xintj),dreal(xintj))*180.d0/pi 
	      write(11,20),'Real part (A): ',dreal(xintj)
	      write(11,20),'Imaginary part (A): ',dimag(xintj)
          close(11)
          
          int_pot(i) = int_pot(i) + xintj
          
!Lorentz force computed with subcncm         
          call florenfor(nsdi,ndom)          
 
 !Joule effect (Active power)
 
          subcaux = dcmplx(0.d0,0.d0)
          
          do ii=1,subnver
		   subcaux(ii) = cdabs(subcncm(ii))**2/(2.d0*conducel(nsdi,ndom))
		  enddo
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcaux),'Active power',&
		  &'scalar','node',trim(fichjoule)//trim(adjustl(cad))//'.vtu')
		  
		  call integrajou(subnel,subz,submm,subcaux,xintj)
		  
		  open(11,file=trim(fichpot)//trim(adjustl(cad)),form='formatted')
		  write(11,30)cdabs(xintj)
          close(11)


!Se guardan B y H para estos subdominios, para evitar extraer la malla de nuevo
!      - extracción del subcampo (subc)
       call extract_field(globv, globel, cb, subc, 'cell', 2)
!      - paso del campo a nodos (subcn)
       call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
       write(cad,*) nsdi
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
       write(cad,*) nsdi
       call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Magnetic field (real part)', &
       'vector','node',trim(fichcampmr)//trim(adjustl(cad))//'.vtu') 
       
       call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcn),'Magnetic field (imaginary part)', &
       'vector','node',trim(fichcampmi)//trim(adjustl(cad))//'.vtu')     
       
       call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcn),'Magnetic field (modulus)', &
       'vector','node',trim(fichcampmm)//trim(adjustl(cad))//'.vtu')  
      
      
          do ii=1,subnver
            subcaux(ii) = sol(globv(ii))	
          enddo          
          
          write(cad,*) nsdi
          
          call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcaux),'Magnetic vector potential (real part)  ',&
		  &'scalar','node',trim(fichsolr)//trim(adjustl(cad))//'.vtu')
        	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',dimag(subcaux),'Magnetic vector potential (imaginary part)',&
		  &'scalar','node',trim(fichsoli)//trim(adjustl(cad))//'.vtu')
	  
          call writeVTU(subnel,subnver,submm,subz,'triangle',cdabs(subcaux),'Magnetic vector potential (modulus)',&
		  &'scalar','node',trim(fichsolm)//trim(adjustl(cad))//'.vtu')     
      enddo
      
      
      !write(cad,*) i+num_inputsi
      !open(11,file=trim(fichvolt)//trim(adjustl(cad)),form='formatted')
      open(11,file=trim(inputsv(i)%etiqueta),form='formatted')
      !write(11,*)'Inductor',i+num_inputsi
      write(11,*)'Subdomains',(inputsv(i)%subdo_references(l),l=1,inputsv(i)%nsubdo)
      write(11,*)'--------------'
      write(11,*)'Voltage drop (input)'
      write(11,20)'RMS voltage drop (V): ',inputsv(i)%vrms

	  write(11,20)'Phase (degrees): ',inputsv(i)%vphase 
	  write(11,*)'--------------'
	  
      valor_phase = datan2(dimag(int_pot(i)),dreal(int_pot(i))) 
	  write(11,*)'Current intensity (output)'
	  write(11,20)'RMS current intensity (A): ',cdabs(int_pot(i))/dsqrt(2.d0)
	  write(11,20)'Phase (degrees): ',valor_phase*180.d0/pi 
      close(11)
     enddo
     
 20  FORMAT((A28,G13.7)) 
 
 30  FORMAT((G13.7))
     
	 return

	 end