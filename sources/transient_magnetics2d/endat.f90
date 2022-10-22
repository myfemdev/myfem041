!************************************************************************
!*     LECTURA DE DATOS                                                 *  
!************************************************************************

      SUBROUTINE endat(iu)

      use fich_electros
      use electros_2D
      use sourcevolumic
      use sourcesurface
      use neumann
      use dirichlet
      use derivados
      use nolineal
      use tiempo
      use pwm
      use sine
      use cosine

     
      implicit none

      integer iu,i,n,m,j,k

    ! nombres de ficheros

      print*,'File containing the mesh?'
      read(iu,*) fichma
      print*,'File for the solution?'
      read(iu,*) fichsol
      print*,'File for the current density?'
      read(iu,*) fichdensc
      print*,'File for the magnetic field?'
      read(iu,*) fichcampm
      print*,'File for the magnetic induction?'
      read(iu,*) fichindum
      print*,'File for the intensities?'
      read(iu,*) fichint


    ! Dirichlet

        print*,'Number of Dirichlet references?'
        read(iu,*) dirichlet_bc%numero
        if (dirichlet_bc%numero .gt. 0) then
            do i=1,dirichlet_bc%numero
                print*,'Reference number?'
                read(iu,*) dirichlet_bc%referencias(i)
                print*,'Mode (1:function, 2:constant)?'
                read(iu,*) dirichlet_bc%modo(i)
                if (dirichlet_bc%modo(i).eq.2) then
                  print*,'Solution constant value?'
                  read(iu,*) dirichlet_bc%valor(i)
                else if (dirichlet_bc%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) dirichlet_bc%etiqueta(i)
                end if 
            enddo
        endif




    ! Neumann

        print*,'Number of Neumann references?'
        read(iu,*) neumann_bc%numero
        if (neumann_bc%numero .gt. 0) then
            do i=1,neumann_bc%numero
                print*,'Reference number?'
                read(iu,*) neumann_bc%referencias(i)
                print*,'Mode (1:function, 2: constant)?'
                read(iu,*) neumann_bc%modo(i)
                if(neumann_bc%modo(i).eq.2) then
                  print*,'Neumann constant value?'
                  read(iu,*) neumann_bc%valor(i)
                else if (neumann_bc%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) neumann_bc%etiqueta(i)
                end if 
            enddo
        endif



    ! fonte volumica
    
        print*,'Number of volumetric sources?'
        read(iu,*) sourcevol%numero
        if (sourcevol%numero .gt. 0) then
            do i=1,sourcevol%numero
                print*,'Type of source (1: current density, 2: intensity (stranded conductor)?'
                read(iu,*)sourcevol%itipo(i)
                print*,'Domain reference?'
                read(iu,*) sourcevol%referencias(i)
                print*,'Mode (1:function, 2: constant)?'
                read(iu,*) sourcevol%modo(i)
                if (sourcevol%modo(i).eq.2) then
                  print*,'Constant source value?'
                  read(iu,*) sourcevol%valor(i)
                else if (sourcevol%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) sourcevol%etiqueta(i)
                end if 
            enddo
        endif
   
 
        print*,'Number of surface sources?'
        read(iu,*) sourcesur%numero
        if (sourcesur%numero .gt. 0) then
            do i=1,sourcesur%numero
                print*,'Type of source (1: current density, 2: intensity (stranded conductor))?'
                read(iu,*)sourcesur%itipo(i)
                print*,'Surface reference?'
                read(iu,*) sourcesur%referencias(i)
                print*,'Mode (1:function, 2: constant)?'
                read(iu,*) sourcesur%modo(i)
                if (sourcesur%modo(i).eq.2) then
                  print*,'Constant source value?'
                  read(iu,*) sourcesur%valor(i)
                else if (sourcesur%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) sourcesur%etiqueta(i)
                end if 
            enddo
        endif

    
        print*,'Number of domains with potential as data?'
        read(iu,*) potencial_dat_vol%numero
        if (potencial_dat_vol%numero .gt. 0) then
            do i=1,potencial_dat_vol%numero
                print*,'Domain reference'
                read(iu,*) potencial_dat_vol%referencias(i)
                print*,'Mode for electrical conductivity (1:function, 2: constant)?'
                read(iu,*) potencial_dat_vol%modo1(i)
                if (potencial_dat_vol%modo1(i).eq.2) then
                  print*,'Constant value of electrical conductivity?'
                  read(iu,*) potencial_dat_vol%valor1(i)
                else if (potencial_dat_vol%modo1(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) potencial_dat_vol%etiqueta1(i)
                end if 
            enddo
            print*,'Number of couples of domains with opposite intensities'
            read(iu,*),potencial_dat_vol%ncouples
            if(potencial_dat_vol%ncouples.gt.0) then
              do i=1,potencial_dat_vol%ncouples
                print*,'Numbers of the two domains of the ',i,'-th couple'
                read(iu,*),potencial_dat_vol%icouple(1,i),potencial_dat_vol%icouple(2,i)
              enddo
            endif
        endif
        
        
        if(potencial_dat_vol%ncouples.gt.0) then
                do i=1, potencial_dat_vol%ncouples
                  print*,'Mode for voltage (1:function, 2: constant)?'
                  read(iu,*) potencial_dat_vol%modo4(i)
                  if (potencial_dat_vol%modo4(i).eq.2) then
                    print*,'Constant value for voltage?'
                    read(iu,*) potencial_dat_vol%valor4(i)
                  else if (potencial_dat_vol%modo4(i).eq.1) then
                    print*,'Option: pwm,sine,user'
                    read(iu,*) potencial_dat_vol%etiqueta4(i)
                    if(potencial_dat_vol%etiqueta4(i).eq.'sine')then
                       print*, 'amplitude, frequency?'
                       read(iu,*) amp_vol(i)
                       read(iu,*) freq_vol(i)
                    elseif(potencial_dat_vol%etiqueta4(i).eq.'cosine')then
                       print*, 'amplitude, frequency?'
                       read(iu,*) amp_vol_cos(i)
                       read(iu,*) freq_vol_cos(i)   
                    else if(potencial_dat_vol%etiqueta4(i).eq.'pwm') then
                       print*, 'amplitude?'
                       read(iu,*) amp_pwm_vol(i)
                       print*, 'number of jumps?'
                       read(iu,*), nsaltos_vol(i)
                       print*, 'jumps points'
                       do j=1, nsaltos_vol(i)
                         read(iu,*) tk_vol(i,j)
                       end do
                    end if  
                  end if
                  print*,'Initial intensity'
                  read(iu,*) potencial_dat_vol%valor3(potencial_dat_vol%icouple(1,i))
                  potencial_dat_vol%valor3(potencial_dat_vol%icouple(2,i))=-potencial_dat_vol%valor3(potencial_dat_vol%icouple(1,i))
                enddo 
                do i=1, potencial_dat_vol%numero
                  do j=1,potencial_dat_vol%ncouples
                    if(i.eq.potencial_dat_vol%icouple(1,j).or.i.eq.potencial_dat_vol%icouple(2,j)) goto 1
                  end do
                  print*,'Mode for potential drop (1:function, 2: constant)?'
                  read(iu,*) potencial_dat_vol%modo2(i) 
                  if (potencial_dat_vol%modo2(i).eq.2) then
                    print*,'Constant value for potential drop?'
                    read(iu,*) potencial_dat_vol%valor2(i)
                  else if (potencial_dat_vol%modo2(i).eq.1) then
                    print*,'Option: example_1,example_2,user'
                    read(iu,*) potencial_dat_vol%etiqueta2(i)
                  end if
                  print*,'Initial intensity'
                  read(iu,*) potencial_dat_vol%valor3(i)               
 1                continue         
                end do
               
         else if (potencial_dat_vol%numero.gt.0)then
           
                 do i=1, potencial_dat_vol%numero
                   print*,'Mode for potential drop (1:function, 2: constant)?'
                   read(iu,*) potencial_dat_vol%modo2(i) 
                   if (potencial_dat_vol%modo2(i).eq.2) then
                     print*,'Constant value for potential drop?'
                     read(iu,*) potencial_dat_vol%valor2(i)
                   else if (potencial_dat_vol%modo2(i).eq.1) then
                     print*,'Option: example_1,example_2,user'
                     read(iu,*) potencial_dat_vol%etiqueta2(i)
                   end if   
                   print*,'Initial intensity'
                   read(iu,*) potencial_dat_vol%valor3(i)           
                 end do
                 
          endif
        
        print*,'Number of surfaces with potential as data?'
        read(iu,*) potencial_dat_sur%numero
        if (potencial_dat_sur%numero .gt. 0) then
            do i=1,potencial_dat_sur%numero
                print*,'Surface reference'
                read(iu,*) potencial_dat_sur%referencias(i)
                print*,'Thickness of the surface (meters)'
                read(iu,*)potencial_dat_sur%valor0(i)
                print*,'Mode for electrical conductivity (1:function, 2: constant)?'
                read(iu,*) potencial_dat_sur%modo1(i)
                if (potencial_dat_sur%modo1(i).eq.2) then
                  print*,'Constant value for electrical conductivity?'
                  read(iu,*) potencial_dat_sur%valor1(i)
                else if (potencial_dat_sur%modo1(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*) potencial_dat_sur%etiqueta1(i)
                end if
            enddo
        end if
            
        print*,'Number of couples of surfaces with opposite intensities'
        read(iu,*),potencial_dat_sur%ncouples
        if(potencial_dat_sur%ncouples.gt.0) then
             do i=1,potencial_dat_sur%ncouples
               print*,'Numbers of the two surfaces of the ',i,'-th couple'
               read(iu,*)potencial_dat_sur%icouple(1,i),potencial_dat_sur%icouple(2,i)
             enddo
        endif
            
           
        if(potencial_dat_sur%ncouples.gt.0) then 
           do i=1, potencial_dat_sur%ncouples
             print*,'Mode for voltage (1:function, 2: constant)?'
             read(iu,*) potencial_dat_sur%modo4(i)
             if (potencial_dat_sur%modo4(i).eq.2) then
                print*,'Constant value for voltage?'
                read(iu,*) potencial_dat_sur%valor4(i)
             else if (potencial_dat_sur%modo4(i).eq.1) then
                print*,'Option: pwm,sine,user'
                read(iu,*) potencial_dat_sur%etiqueta4(i)
                if(potencial_dat_sur%etiqueta4(i).eq.'sine')then
                       print*, 'amplitude, frequency?'
                       read(iu,*) amp_sur(i)
                       read(iu,*) freq_sur(i)
                 elseif(potencial_dat_sur%etiqueta4(i).eq.'cosine')then
                       print*, 'amplitude, frequency?'
                       read(iu,*) amp_sur_cos(i)
                       read(iu,*) freq_sur_cos(i)
                else if(potencial_dat_sur%etiqueta4(i).eq.'pwm') then
                       print*, 'amplitude?'
                       read(iu,*) amp_pwm_sur(i)
                       print*, 'number of jumps?'
                       read(iu,*), nsaltos_sur(i)
                       print*, 'jumps points'
                       do j=1, nsaltos_sur(i)
                          read(iu,*) tk_sur(i,j)
                       end do
                end if  
             end if
             print*,'Initial intensity'
             read(iu,*) potencial_dat_sur%valor3(potencial_dat_sur%icouple(1,i))
             potencial_dat_sur%valor3(potencial_dat_sur%icouple(2,i))=-potencial_dat_sur%valor3(potencial_dat_sur%icouple(1,i))
           enddo 
           do i=1, potencial_dat_sur%numero
             do j=1,potencial_dat_sur%ncouples
               if(i.eq.potencial_dat_sur%icouple(1,j).or.i.eq.potencial_dat_sur%icouple(2,j)) goto 2
             end do
             print*,'Mode for potential drop (1:function, 2: constant)?'
             read(iu,*) potencial_dat_sur%modo2(i) !!!! NOTA: Vale 0 en los dominios que forman parte de una pareja
             if (potencial_dat_sur%modo2(i).eq.2) then
               print*,'Constant value for potential drop?'
               read(iu,*) potencial_dat_sur%valor2(i)
             else if (potencial_dat_sur%modo2(i).eq.1) then
               print*,'Option: example_1,example_2,user'
               read(iu,*) potencial_dat_sur%etiqueta2(i)
             end if  
             print*,'initial intensity'
             read(iu,*) potencial_dat_sur%valor3(i)
 2           continue         
             end do
           
          
        else if(potencial_dat_sur%numero.gt.0)then
           
             do i=1, potencial_dat_sur%numero
                 print*,'Mode for potential drop (1:function, 2: constant)?'
                 read(iu,*) potencial_dat_sur%modo2(i) 
                 if (potencial_dat_sur%modo2(i).eq.2) then
                   print*,'Constant value for potential drop?'
                   read(iu,*) potencial_dat_sur%valor2(i)
                 else if (potencial_dat_sur%modo2(i).eq.1) then
                   print*,'Option: example_1,example_2,user'
                   read(iu,*) potencial_dat_sur%etiqueta2(i)
                 end if 
                 print*,'Initial intensity'
                 read(iu,*) potencial_dat_sur%valor3(i)                  
             end do
                 
        endif
           
           
   
   
        iopteta=0; ndnolin=0
        
        print*,'Number of domains?'
        read(iu,*) permagrel%numero

        print*,'Are there nonlinear materials? 1: Yes, 0: No'
        read(iu,*) iopdli
        if (iopdli.eq.1) then
           print*,'Small number for stopping test?'
           read(iu,*) e
           print*,'Maximun number of iterations?'
           read(iu,*) niter
        endif

        do i=1,permagrel%numero
            print*,'Domain reference?'
            read(iu,*)permagrel%referencias(i)
            print*, 'Is it linear? 1--Yes, 0--No'
            read(iu,*) permagrel%ioplin(i)  !FRAN (SUGERENCIA): cambiar 'ioplin' por el LOGICAL 'lineal':
                                              !true, es lineal; false, es no lineal
            if (permagrel%ioplin(i).eq.1) then
                    print*, 'Linear material'
                    print*,'Case for relative magnetic permeability'
                    print*,'1 --> User-defined function'
                    print*,'2 --> Constant value'
                    read(iu,*)permagrel%iopermagr(i) !FRAN (SUGERENCIA): yo cambiaría 'iopermagr' por un logical 'constant':
                                                       !true, es constante; false, es función.
                                                       !si hubiese más opciones lo pondria como el character(len=1) 'give_as':
                                                       !give_as = 'c' (constante); give_as = 'f' (function), ...
                    if (permagrel%iopermagr(i).eq.2) then
                        print*,'Relative magnetic permeability (two values for x and y directions)?'
                        read(iu,*)permagrel%valorx(i), permagrel%valory(i)
                  
                    endif
            else
            
              print*, 'Nonlinear material'
              ndnolin=ndnolin+1 
              idnolin(ndnolin)=permagrel%referencias(i)
              print*,'Number of points for arrays h and b?'
              read(iu,*) n !numero de puntos de los arrays h, b
              if (allocated(permagrel%hb(ndnolin)%h)) deallocate(permagrel%hb(ndnolin)%h)
              if (allocated(permagrel%hb(ndnolin)%b)) deallocate(permagrel%hb(ndnolin)%b) 
              allocate(permagrel%hb(ndnolin)%h(n), permagrel%hb(ndnolin)%b(n))
              print*,'array h'
              read(iu,*) permagrel%hb(ndnolin)%h !lectura del array h
              print*,'array b'
              read(iu,*) permagrel%hb(ndnolin)%b !lectura del array b
            endif             
        enddo
 
        if (iopteta.eq.1) then    
          print*,'File name containing the temperature field?'
          read(iu,*) fichteta
        endif

       print*,'Quadrature formula for 2D integrals'
       print*,'1: baricenter, 2:vertices, 3: mid-points'
       read(iu,*)iop


       print*,'Quadrature formula for line integrals'
       print*,'1: two-points Gauss rule, 2: trapeizodal rule'
       read(iu,*)iopf
       
       print*,'Initial time, final time, number of time intervals'
       read(iu,*) ti,tf,npat

       return

       end
