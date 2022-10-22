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

     
      implicit none

      integer iu,i,j,n

    ! nombres de ficheros

      print*,'File containing the mesh?'
      read (iu,*), fichma
      print*,'File for the solution?'
      read (iu,*), fichsol
      print*,'File for the current density?'
      read (iu,*), fichdensc
      print*,'File for the magnetic field?'
      read (iu,*), fichcampm
      print*,'File for the magnetic induction?'
      read (iu,*), fichindum


    ! Dirichlet

        print*,'Number of Dirichlet references?'
        read (iu,*), dirichlet_bc%numero
        if (dirichlet_bc%numero .gt. 0) then
            do i=1,dirichlet_bc%numero
                print*,'Reference number and mode (1:function, 2:constant)?'
                read (iu,*), dirichlet_bc%referencias(i), dirichlet_bc%modo(i)
                if (dirichlet_bc%modo(i).eq.2) then
                  print*,'Solution constant value?'
                  read (iu,*), dirichlet_bc%valor(i)
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



    ! fonte volumica
    
        print*,'Number of volumetric sources?'
        read (iu,*), sourcevol%numero
        if (sourcevol%numero .gt. 0) then
            do i=1,sourcevol%numero
                print*,'Type of source (1: current density, 2: intensity (stranded conductor)?'
                read (iu,*),sourcevol%itipo(i)
                print*,'Domain reference and mode (1:function, 2: constant)?'
                read (iu,*), sourcevol%referencias(i), sourcevol%modo(i)
                if (sourcevol%modo(i).eq.2) then
                  print*,'Constant source value?'
                  read (iu,*), sourcevol%valor(i)
                else if (sourcevol%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*), sourcevol%etiqueta(i)
                end if 
            enddo
        endif
   
 
        print*,'Number of surface sources?'
        read (iu,*), sourcesur%numero
        if (sourcesur%numero .gt. 0) then
            do i=1,sourcesur%numero
                print*,'Type of source (1: current density, 2: intensity (stranded conductor))?'
                read (iu,*),sourcesur%itipo(i)
                print*,'Surface reference and mode (1:function, 2: constant)?'
                read (iu,*), sourcesur%referencias(i), sourcesur%modo(i)
                if (sourcesur%modo(i).eq.2) then
                  print*,'Constant source value?'
                  read (iu,*), sourcesur%valor(i)
                else if (sourcesur%modo(i).eq.1) then
                  print*,'Option: example_1,example_2,user'
                  read(iu,*), sourcesur%etiqueta(i)
                end if 
            enddo
        endif
    ! relative magnetic permeability
   
        iopteta=0; ndnolin=0
        
        print*,'Number of domains?'
        read (iu,*), permagrel%numero

        print*,'Are there nonlinear materials? 1: Yes, 0: No'
        read (iu,*) iopdli
        if (iopdli.eq.1) then
           print*,'Small number for stopping test?'
           read(iu,*) e
           print*,'Maximun number of iterations?'
           read(iu,*) niter
        endif

        do i=1,permagrel%numero
            print*,'Domain reference?'
            read (iu,*),permagrel%referencias(i)
            print*, 'Is it linear? 1--Yes, 0--No'
            read (iu,*), permagrel%ioplin(i)  !FRAN (SUGERENCIA): cambiar 'ioplin' por el LOGICAL 'lineal':
                                              !true, es lineal; false, es no lineal
            if (permagrel%ioplin(i).eq.1) then
                    print*, 'Linear material'
                    print*,'Case for relative magnetic permeability'
                    print*,'1 --> User-defined function'
                    print*,'2 --> Constant value'
                    read (iu,*),permagrel%iopermagr(i) !FRAN (SUGERENCIA): yo cambiaría 'iopermagr' por un logical 'constant':
                                                       !true, es constante; false, es función.
                                                       !si hubiese más opciones lo pondria como el character(len=1) 'give_as':
                                                       !give_as = 'c' (constante); give_as = 'f' (function), ...
                    if (permagrel%iopermagr(i).eq.2) then
                        print*,'Relative magnetic permeability (two values for x and y directions)?'
                        read (iu,*),permagrel%valorx(i), permagrel%valory(i)
                    endif
            else
            
              print*, 'Nonlinear material'
              ndnolin=ndnolin+1 
              idnolin(ndnolin)=permagrel%referencias(i)
              print*,'number of points for arrays h and b?'
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
          read (iu,*), fichteta
        endif

       print*,'Quadrature formula for 2D integrals'
       print*,'1: baricenter, 2:vertices, 3: mid-points'
       read (iu,*),iop


       print*,'Quadrature formula for line integrals'
       print*,'1: two-points Gauss rule, 2: trapeizodal rule'
       read (iu,*),iopf

       return

       end
