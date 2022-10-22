!************************************************************************
!*     LECTURA DE DATOS                                                 *  
!************************************************************************

      SUBROUTINE endat(iu)

      use fich_electros
      use electros_2D
      use sourcevolumic
      use neumann
      use dirichlet
      use derivados
      use voltage_drop
      use intensity_input

     
      implicit none

      integer iu,i,j,n
      character(len=255) :: cad

    ! nombres de ficheros

      print*,'File containing the mesh?'
      read (iu,*), fichma
      print*,fichma
      print*,'File for the solution (real part)?'
      read (iu,*), fichsolr
      print*,fichsolr
      print*,'File for the solution (imag part)?'
      read (iu,*), fichsoli 
      print*,fichsoli
      print*,'File for the solution (modulus)?'
      read (iu,*), fichsolm
      print*,fichsolm
      print*,'File for the current density (real part)?'
      read (iu,*), fichdenscr
      print*,fichdenscr
      print*,'File for the current density (imag part)?'
      read (iu,*), fichdensci 
      print*,fichdensci
      print*,'File for the current density (modulus)?'
      read (iu,*), fichdenscm   
      print*,fichdenscm         
      print*,'File for the magnetic field (real part)?'
      read (iu,*), fichcampmr
      print*,fichcampmr
      print*,'File for the magnetic field (imag part)?'
      read (iu,*), fichcampmi 
      print*,fichcampmi     
      print*,'File for the magnetic field (modulus)?'
      read (iu,*), fichcampmm 
      print*,fichcampmm           
      print*,'File for the magnetic induction (real part)?'
      read (iu,*), fichindumr
      print*, fichindumr

      print*,'File for the magnetic induction (imag part)?'
      read (iu,*), fichindumi
      print*, fichindumi
      print*,'File for the magnetic induction (modulus)?'
      read (iu,*), fichindumm   
      print*,fichindumm   
      
      print*,'File for the Joule effect?'
      read (iu,*), fichjoule  
      print*,fichjoule         
    ! Dirichlet

        print*,'Number of Dirichlet references?'
        read (iu,*), dirichlet_bc%numero
        print*,dirichlet_bc%numero
        if (dirichlet_bc%numero .gt. 0) then
            do i=1,dirichlet_bc%numero
                print*,'Reference number and mode (1:function, 2:constant)?'
                read (iu,*), dirichlet_bc%referencias(i), dirichlet_bc%modo(i)
                print*, dirichlet_bc%referencias(i), dirichlet_bc%modo(i)
                if (dirichlet_bc%modo(i).eq.2) then
                  print*,'Solution constant value?'
                  read (iu,*), dirichlet_bc%valor(i)
                  print*,dirichlet_bc%valor(i)
                else if (dirichlet_bc%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*), dirichlet_bc%etiqueta(i)
                  print*, dirichlet_bc%etiqueta(i)
                end if 
            enddo
        endif




    ! Neumann

        print*,'Number of Neumann references?'
        read (iu,*), neumann_bc%numero
        if (neumann_bc%numero .gt. 0) then
            do i=1,neumann_bc%numero
                print*,'Reference number and mode (1:function, 2: constant)?'
                read (iu,*), neumann_bc%referencias(i),neumann_bc%modo(i)
                if(neumann_bc%modo(i).eq.2) then
                  print*,'Neumann constant value?'
                  read (iu,*), neumann_bc%valor(i)
                else if (neumann_bc%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*), neumann_bc%etiqueta(i)
                end if 
            enddo
        endif

     ! Frecuencia de la corriente
       print*,'Frequency (Hz)'
       read(iu,*)frec

    ! Volumic source (given Js) -- no eddy currents in these domains
    ! For each subdomain, we need: reference, mode, RMS value and phase
    
        print*,'Number of subdomains with volumetric sources?'
        read (iu,*), sourcevol%numero
        if (sourcevol%numero .gt. 0) then
            do i=1,sourcevol%numero
                print*,'Type of source (1: current density, 2: intensity (stranded conductor)?'
                read (iu,*),sourcevol%itipo(i)
                print*,'Domain reference and mode (1:function, 2: constant)?'
                read (iu,*), sourcevol%referencias(i), sourcevol%modo(i)
                if (sourcevol%modo(i).eq.2) then
                  print*,'RMS source value?'
                  read (iu,*), sourcevol%vrms(i)
                  print*,'Phase angle?'
                  read (iu,*), sourcevol%vphase(i)
                else if (sourcevol%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*), sourcevol%etiqueta(i)
                end if 
            enddo
        endif
        
!Voltage drop -- eddy currents are taken into account in these domains
!                voltage drop is given in a connected component
! For each connected component, we need: number of subdomains with compose the 
! component, subdomains, mode, RMS voltage value and phase
        print*,'Number of voltage drops conditions?'
        read(iu,*) num_inputsv
        print*,num_inputsv
        if(num_inputsv .gt. 0)then
           allocate(inputsv(num_inputsv),stat=ierror)
           if (ierror.ne.0) then
             print*,'error: inputsv cannot be allocated'
             stop 1
           endif
           
           do i=1,num_inputsv
                print*,'Number of subdomains'
                read(iu,*)inputsv(i)%nsubdo
                print*,inputsv(i)%nsubdo
                print*,'Domain references'
                read(iu,*) (inputsv(i)%subdo_references(j),j=1,inputsv(i)%nsubdo)
                print*,(inputsv(i)%subdo_references(j),j=1,inputsv(i)%nsubdo)
!Voltage drop must be constant
                  print*,'RMS voltage drop value'
                  read(iu,*) inputsv(i)%vrms
                  print*,'Phase angle'
                  read(iu,*) inputsv(i)%vphase
                  write(cad,*) i
                  inputsv(i)%etiqueta = trim("solid_conductor")//trim(cad)
            enddo
        endif        

!Current intensity -- eddy currents are taken into account in these domains
!                current intensity is given in a connected component
! For each connected component, we need: number of subdomains with compose the 
! component, subdomains, mode, RMS intensity value and phase
        print*,'Number of current intensity conditions?'
        read(iu,*) num_inputsi
        print*,num_inputsi
        if(num_inputsi .gt. 0)then
           allocate(inputsi(num_inputsi),stat=ierror)
           if (ierror.ne.0) then
             print*,'error: inputsi cannot be allocated'
             stop 1
           endif
           
           do i=1,num_inputsi
                print*,'Number of subdomains'
                read(iu,*)inputsi(i)%nsubdo
                print*,inputsi(i)%nsubdo
                print*,'Domain references'
                read(iu,*) (inputsi(i)%subdo_references(j),j=1,inputsi(i)%nsubdo)
                print*,(inputsi(i)%subdo_references(j),j=1,inputsi(i)%nsubdo)
!Intensity must be constant
!                print*,'Mode for intensity (1:function, 2: constant)?'
!                read(iu,*) inputsv(i)%modo
!                print*,inputsv(i)%modo
                  print*,'RMS intensity value'
                  read(iu,*) inputsi(i)%vrms
                  print*,'Phase angle'
                  read(iu,*) inputsi(i)%vphase                 
                  write(cad,*) (num_inputsv+i)
                  inputsi(i)%etiqueta = trim("solid_conductor")//trim(cad)
            enddo
        endif        
       
    ! relative magnetic permeability
   
        iopteta=0; 
        
        print*,'Number of domains?'
        read (iu,*), permagrel%numero
        print*,permagrel%numero

        do i=1,permagrel%numero
            print*,'Domain reference?'
            read (iu,*),permagrel%referencias(i)
            print*,permagrel%referencias(i)
 
            print*, 'Linear material'
            print*,'Case for relative magnetic permeability'
            print*,'1 --> User-defined function'
            print*,'2 --> Constant value'
            print*,'3 --> Temperature dependent'
            read (iu,*),permagrel%iopermagr(i) 
            print*,permagrel%iopermagr(i)
            if (permagrel%iopermagr(i).eq.2) then
                print*,'Relative magnetic permeability (isotropic)?'
                read (iu,*),permagrel%valor(i)
            elseif(permagrel%iopermagr(i).eq.1 .or. permagrel%iopermagr(i).eq.3)then
                print*,'Case not implemented'
                stop 1    
            endif
              
        enddo
        
  
        
        
! Electrical conductivity (isotropic)

 iopteta=0
 
        print*,'Number of domains?'
        read (iu,*), conduc%numero

        do i=1,conduc%numero
            print*,'Domain reference?'
            read (iu,*),conduc%referencias(i)
 
            print*,'Case for electrical conductivity'
            print*,'1 --> User-defined function'
            print*,'2 --> Constant value'
            print*,'3 --> Temperature dependent'

            read (iu,*),conduc%iopcond(i) 
            if (conduc%iopcond(i).eq.2) then
                print*,'Electrical conductivity (isotropic)?'
                read (iu,*),conduc%valor(i)
                print*,'i,sigma',i,conduc%valor(i)
            elseif(conduc%iopcond(i).eq.3)then 
      		    iopteta=1 !hay que leer fichero de temperaturas
      		    read*,conduc%ntab(i)
      		    do j=1,conduc%ntab(i)
                    read*,conduc%teta(i,j),conduc%valtab(i,j)
      		    enddo
      		elseif(conduc%iopcond(i).eq.1)then
                print*,'Case not implemented'
                stop 1 
      		 endif
              
         enddo
           		
                   
 
        if (iopteta.eq.1) then    
          print*,'File name containing the temperature field?'
          read (iu,*), fichteta
        endif

       print*,'Quadrature formula for 2D integrals'
       print*,'1: baricenter, 2:vertices, 3: mid-points'
       read (iu,*),iop


       print*,'Quadrature formula for line integrals'
       print*,'1: two-points Gauss rule, 2: trapeizodal rule'
       read (iu,*),iopf
       
       
       print*,'Terminado endat',num_inputsv
       
       fichintens = 'intensity_current'
       fichpot = 'power_active'
       fichvolt = 'solid_conductor'
       fichflo = 'out_lorentz'


       return

       end
