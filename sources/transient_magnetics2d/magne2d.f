***************************************************************************
*                 resolucion del problema de magnetostatica               *
***************************************************************************

      SUBROUTINE magne2d()
    
      use malla_2DP1
      use electros_2D
      use derivados
      use parametros_electros
      use nolineal
      use dirichlet
      use neumann
      use tiempo
      use module_writeVTU
      use module_fem_extract
      use module_writePVD
      use fich_electros
      use potenciales_vol
      use potenciales_sur
      use intensidad
     
 
      implicit none
      

      integer          ::  n,i,j,k,modo2

      double precision ::  errorel, errorabs,time, errorint,solexac,
     &                     error_rel,error_abs,valor2,
     &                     pot_sur,pi
      
      double precision, allocatable :: subz(:,:), cb(:),ch(:), subc(:),
     &  subcn(:)
      integer, allocatable :: submm(:,:), globv(:), globel(:)
      character(len=255) :: cad, cad2
      character*9 etiqueta2
      integer :: subnel, subnver      
      integer :: l,ios
      
      
***************************************************************************     
*                      calculo de la matriz                               *
***************************************************************************
      
      
      if(potencial_dat_vol%numero.gt.0.or.
     &   potencial_dat_sur%numero.gt.0) then
     
      allocate (xints(potencial_dat_vol%numero+potencial_dat_sur%numero,
     & 0:npat))
     
      end if
    
      deltat=(tf-ti)/npat
      
!---------------------------------------------------------------------------
!          intesidades iniciales
!---------------------------------------------------------------------------
      if(potencial_dat_vol%numero.gt.0) then
        do i=1,potencial_dat_vol%numero
          xints(i,0)=potencial_dat_vol%valor3(i)
        end do
      end if
      
      if(potencial_dat_sur%numero.gt.0) then
        do i=1,potencial_dat_sur%numero
         xints(i+potencial_dat_vol%numero,0)=potencial_dat_sur%valor3(i)
        end do
      end if


!---------------------------------------------------------------------------
!          calculo de la condicion inicial       
!--------------------------------------------------------------------------- 
     
      call sol1()
      
!---------------------------------------------------------------------------
!           salida de resultados para visualizacion        
!---------------------------------------------------------------------------
!SOLUCION

	write(cad2,*)1
      call writeVTU(nel,nver,mm,z,'triangle',sol,'Magnetic vector potential (Wb/m)','scalar',
     &  'node',trim(fichsol)//'_'//trim(adjustl(cad2))//'.vtu')
!     &  ,Scalars='Solution')

!INDUCCION MAGNETICA:

      if (allocated(cb)) deallocate(cb) !lalfredo
      allocate(cb(2*nel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array cb'
      cb(1:nel*2:2) = rotu(1,1:nel)
      cb(2:nel*2:2) = rotu(2,1:nel)
     
!CAMPO MAGNETICO:

      if (allocated(ch)) deallocate(ch) !alfredo
      allocate(ch(2*nel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array ch'
      ch(1:nel*2:2) = rotuh(1,1:nel)  !alfredo
      ch(2:nel*2:2) = rotuh(2,1:nel)    !alfredo

!    bucle en subdominios

      do l = 1, permagrel%numero
     
!       - extracción de la submalla (submm, subz)
        call extract_mesh(nver, mm, z, nsd,[permagrel%referencias(l)]
     &       ,submm, subz, globv, globel)
        subnel = size(submm,2); subnver = size(subz,2)
       
!INDUCCION MAGNETICA:

!       - extracción del subcampo (subc)
        call extract_field(globv, globel, cb, subc, 'cell', 2)
!       - paso del campo a nodos (subcn)
        call cell2node(subnver, submm, subc, subcn)
!       - escritura por nodos y subdominio
        write(cad,*) permagrel%referencias(l)
        call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,
     &    'Magnetic flux density (T)', 
     &    'vector','node',trim(fichindum)//trim(adjustl(cad))//'_'//
     &    trim(adjustl(cad2))//'.vtu')
!     &    ,Scalars='MagneticInduction')
       
!CAMPO MAGNETICO:

!         - extracción del subcampo (subc)
        call extract_field(globv, globel, ch, subc, 'cell', 2)
!         - paso del campo a nodos (subcn)
        call cell2node(subnver, submm, subc, subcn)
!         - escritura por nodos y subdominio
        write(cad,*) permagrel%referencias(l)
        call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,
     &    'Magnetic field (A/m)', 
     &    'vector','node',trim(fichcampm)//trim(adjustl(cad))//'_'//
     &    trim(adjustl(cad2))//'.vtu')!,Scalars='MagneticField')
      enddo
     

 
 !ESCRITURA DE LOS CAMPOS POR VERTICES EN FORMATO mff

 !     call wrtcmp(nver,sol,10,trim(fichsol)//'.mff')
 !     call wrtcmp(2*nver,rotv,11,trim(fichindum)//'.mff')
 !     call wrtcmp(2*nver,hv,11,trim(fichcampm)//'.mff')
        
!---------------------------------------------------------------------------
!           calculos previos para bucle en tiempo      
!---------------------------------------------------------------------------
 
      call matriz()
      print*,'the matrix has been computed'
      print*,'----------------------------------------------'
     
      
      ! bloqueo de la matriz 


      if (dirichlet_bc%numero.gt.0) then                   
        call blomat(c,mua)    
      end if
        
 
      print*,'the matrix has been blocked'
      print*,'----------------------------------------------'

***************************************************************************
*               factorizacion de Choleski de la matriz                    *
***************************************************************************
      call chol(nver,mua,c)
      print*,'the matrix has been Choleski-factorized'
      print*,'----------------------------------------------'
    

      print*,'starting the time loop'
      
      do ipat=1,npat
      
        time=ipat*deltat+ti
        
        print*
        print*,'------------------------------------'
        print*,'time=',time
        print*,'------------------------------------'


***************************************************************************
*                  calculo del vector segundo miembro                     *
***************************************************************************     
       
        call semi (time)
        if (potencial_dat_vol%numero.gt.0) then
                call integra_sa(sol)
                xintsavol0=xintsavol
        endif
                
        if (potencial_dat_sur%numero.gt.0)then
                call integra_sa_su(sol)
                xintsasur0=xintsasur
        endif

        if (potencial_dat_vol%numero.gt.0.or.
     &      potencial_dat_sur%numero.gt.0) call semi2(time)
     

        if (iopdli.eq.0) then 

*****************************************************************************            
*todos los dominios son lineales y (no hay dominios con potenciales como dato
*o es el primer paso de tiempo)
*****************************************************************************
         
               
***************************************************************************
*                bloqueo segundo miembro  (cond. Dirichlet)               *
***************************************************************************

          if (dirichlet_bc%numero. gt.0) then
             call bloseg2d(b)
          end if
         

***************************************************************************
*                      resolucion del sistema                             *
***************************************************************************

          print*,'-----------------------------------------------------'
          print*,'solving the linear system'
          print*,'-----------------------------------------------------'

          call sols(c,b,nver,mua)
    
          do 4 i=1,nver
            sol(i) = b(i)
    4     continue

          print*,'the linear system has been solved'
          print*,'-----------------------------------------------------'
   
        else  !es necesario iterar

          print*,'starting iterations'
          
          do n=1,niter
            print*,'-------------------------------------------'
            print*,'iteration ',n
            print*,'-------------------------------------------'
       
            bvar=b
          
            call segvar(nel,nver,z,mm,bvar,ndnolin,idnolin,nsd)
            

***************************************************************************
*                bloqueo segundo miembro  (cond. Dirichlet)               *
***************************************************************************


            if (dirichlet_bc%numero .gt. 0) then
              call bloseg2d(bvar)
            end if

   
***************************************************************************
*                      resolucion del sistema                             *
***************************************************************************

   
!          print*,'Solving the linear system'
!            print*,'--------------------------------------------------'

            call sols(c,bvar,nver,mua)
   
            
            p0=p

            call renovacion()
            

!calculo de errores y test de parada
            
 
            errorabs=maxval(dabs(p-p0))
            errorel=maxval(dabs(p-p0)/(dabs(p)+e))
           

            print*,'relative error=', errorel
            print*,'absolute error=', errorabs


       
            if (errorel.le.e.or. errorabs.le.e)then
              print*
              print*,'----------------------------------------'
              print*,'convergence in ',n,' iterations'
              print*,'----------------------------------------'
              print*
              do  i=1,nver
                sol(i) = bvar(i)
              end do
              exit        
            endif

          end do 
        
          if(n.ge.niter) then
       
            print*
            print*,'--------------------------------------------------'
            print*,'no convergence in ',niter,' iterations'
            print*,'--------------------------------------------------'
            print*
          
          endif
        
        endif
        
        call intensidades()
        
        !calculo de B y H
   
        call calrot()
        call calcampomag() 
        

      
!---------------------------------------------------------------------------
!           salida de resultados para visualizacion        
!---------------------------------------------------------------------------

        

!SOLUCION


	  write(cad2,*) ipat+1
        call writeVTU(nel,nver,mm,z,'triangle',sol,'Magnetic vector potential (Wb/m)','scalar',
     &  'node',trim(fichsol)//'_'//trim(adjustl(cad2))//'.vtu')
!     &  ,Scalars='Solution')

!INDUCCION MAGNETICA:

        if (allocated(cb)) deallocate(cb) !lalfredo
        allocate(cb(2*nel), STAT = ierror)
        if (ierror .ne. 0) stop 'Error when allocating array cb'
        cb(1:nel*2:2) = rotu(1,1:nel)
        cb(2:nel*2:2) = rotu(2,1:nel)
     
!CAMPO MAGNETICO:

        if (allocated(ch)) deallocate(ch) !alfredo
        allocate(ch(2*nel), STAT = ierror)
        if (ierror .ne. 0) stop 'Error when allocating array ch'
        ch(1:nel*2:2) = rotuh(1,1:nel)  !alfredo
        ch(2:nel*2:2) = rotuh(2,1:nel)    !alfredo

!    bucle en subdominios

        do l = 1, permagrel%numero
     
!       - extracción de la submalla (submm, subz)
          call extract_mesh(nver, mm, z, nsd,[permagrel%referencias(l)]
     &       ,submm, subz, globv, globel)
          subnel = size(submm,2); subnver = size(subz,2)
       
!INDUCCION MAGNETICA:

!       - extracción del subcampo (subc)
          call extract_field(globv, globel, cb, subc, 'cell', 2)
!       - paso del campo a nodos (subcn)
          call cell2node(subnver, submm, subc, subcn)
!       - escritura por nodos y subdominio
          write(cad,*) permagrel%referencias(l)
          call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,
     &    'Magnetic flux density (T)', 
     &    'vector','node',trim(fichindum)//trim(adjustl(cad))//'_'//
     &    trim(adjustl(cad2))//'.vtu')
!     &    ,Scalars='MagneticInduction')

       
!CAMPO MAGNETICO:

!         - extracción del subcampo (subc)
          call extract_field(globv, globel, ch, subc, 'cell', 2)
!         - paso del campo a nodos (subcn)
          call cell2node(subnver, submm, subc, subcn)
!         - escritura por nodos y subdominio
          write(cad,*) permagrel%referencias(l)
          call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,
     &    'Magnetic field (A/m)', 
     &    'vector','node',trim(fichcampm)//trim(adjustl(cad))//'_'//
     &    trim(adjustl(cad2))//'.vtu')!,Scalars='MagneticField')
        enddo
     

 
 !ESCRITURA DE LOS CAMPOS POR VERTICES EN FORMATO mff

 !       call wrtcmp(nver,sol,10,trim(fichsol)//'.mff')
 !       call wrtcmp(2*nver,rotv,11,trim(fichindum)//'.mff')
 !       call wrtcmp(2*nver,hv,11,trim(fichcampm)//'.mff')
       
      end do !fin del bucle en pasos de tiempo
      
 !ESCRITURA DE INTENSIDADES
 
 !formato gr2
 
      if(potencial_dat_vol%numero+potencial_dat_sur%numero.gt.0) then
        open(file=trim(fichint)//'.gr2', unit=99, form='formatted',
     &       iostat=ios)
        write(99,*) potencial_dat_vol%numero+potencial_dat_sur%numero
        do i=1,potencial_dat_vol%numero+potencial_dat_sur%numero
          write(99,*) npat+1
          write(99,*) (deltat*j,j=0,npat)
          write(99,*) (xints(i,j),j=0,npat)
        end do
        write(99,*)'Intensities'
        write(99,*)'time (s)'
        write(99,*)'intensity (A)'
        do i=1,potencial_dat_vol%numero+potencial_dat_sur%numero
          write(cad,*) i
          write(99,*)trim(cad)
        end do
      end if
      close(99)
      
      !    formato csv (paraview)
      
      open(file=trim(fichint)//'.csv', unit=99, form='formatted',
     &     iostat=ios)  
         
      cad = '"t"'
      
      do i=1, potencial_dat_vol%numero+potencial_dat_sur%numero
        write(cad2,*) i
        cad = trim(cad)//',"'//trim(adjustl(cad2))//'"'
      enddo  
      
      write(99,*) trim(cad)
      
      do j = 0, npat
        write(cad,*) deltat*j
        cad = adjustl(cad)
        
        do i=1,potencial_dat_vol%numero+potencial_dat_sur%numero
          write(cad2,*) xints(i,j)
          cad = trim(cad)//','//adjustl(cad2)
        enddo
        
        write(99,*) trim(cad)  
      enddo
      
      close(99)
      
!write pvd's

      call writePVD(trim(fichsol)//'.pvd', trim(fichsol), 
     &    [(real(ipat*deltat), ipat=0,npat)])
     
      do l = 1, permagrel%numero
         write(cad,*) permagrel%referencias(l)
         call writePVD(trim(fichindum)//trim(adjustl(cad))//'.pvd',
     &   trim(fichindum)//trim(adjustl(cad)), 
     &   [(real(ipat*deltat), ipat=0,npat)])
         call writePVD(trim(fichcampm)//trim(adjustl(cad))//'.pvd',
     &   trim(fichcampm)//trim(adjustl(cad)), 
     &   [(real(ipat*deltat), ipat=0,npat)])
      end do
     
      return
      end subroutine
