      PROGRAM magnetostatics2d

      use comprobaciones

      use parametros_electros
      use electros_2D
      use external_electros
      use fich_electros
      use malla_2DP1
      use dirichlet
      use neumann
      use sourcesurface
      use derivados
      use module_writeVTU
      use nolineal
      use module_fem_extract
      use module_writePVD
      use module_readUNV
      use LIB_VTK_IO_READ


      implicit none
      
      integer :: ii, i
      double precision solexac, error_rel, error_abs, h, hexac,xnorl2,xnorl2sol
      double precision, allocatable :: subz(:,:), cb(:), ch(:), subc(:), subcn(:)
      integer, allocatable :: submm(:,:), globv(:), globel(:),nn(:,:),nrc(:,:)
      character(len=255) :: cad
      integer :: subnel, subnver      
      integer :: l
      integer                       :: DIM,LNN,LNV,LNE,LNF,nnod,npieces
      integer                       :: p,nverteta,iformat,nnd,nco
      real, allocatable             :: tetaux(:)
      integer                       :: istat,k

!---------------------------------------------------------------------------
!*                        entrada de datos                                 *
!---------------------------------------------------------------------------

      character * 300 arg
      integer length, status, ios, iu

    ! determinar lectura por ficheiro ou por entrada estandar

      if (command_argument_count() >= 1) then
        call get_command_argument(1, arg, length, status)
        if (status /= 0) then
          write(0,*) 'magnetostatics2d: the first command argument '&
     &//'cannot be read'
          stop 1
        endif
        if (arg == '-xml') then
          call readxml()
        else
          iu = 99
          open(file=arg, unit=iu, form='formatted', status='old',&
       &  position='rewind', iostat=ios)
        
          if (ios /= 0) then
            write(0,*) 'datafile cannot be open'
            stop 1
          end if

          call endat(iu)
      
          print*
          print*
          print*,'----------------------------------------------------------'
          print*, 'the data has been read'

          close(iu)
         endif     

      endif

    ! chamar a lectura de datos


    ! comprobacion de datos de entrada: serve para endat e readxml

      if (.not. comprueba()) then
          write(0,*) 'input data checking failed'
          stop 1
      endif


!---------------------------------------------------------------------------
!                     ELECTROMAGNETIC MESH READING                    
!---------------------------------------------------------------------------

  p = index(fichma, '.', back=.true.)
  if (p == 0) stop 'Mesh file has not extension: unable to identify mesh format'
  select case (lcase(fichma(p+1:len_trim(fichma))))
  case('mfm')
     iformat=1
     call leema(iformat)
  case('mum')
     iformat=2
     call leema(iformat)
  case('unv')
     call readUNV(fichma,nel,nnod,nver,DIM,LNN,LNV,LNE,LNF,nn,&
                  mm,nrc,nra,nrv,z,nsd)
  case default
     stop 'Unrecognized mesh file extension'
  end select
  
  call alloc_after_mesh()

!---------------------------------------------------------------------------
!                     TEMPERATURE READING
!---------------------------------------------------------------------------
!Magnetostatic: no properties with temperature dependence in this version
!  if (iopteta == 1) then !temperature must be read
!     if (allocated(teta)) deallocate(teta) !deallocate teta
!     p = index(fichteta, '.', back=.true.) !seach extension
!     if  (p == 0) stop 'Error (temperature): file has not extension'
!     select case (lcase(fichteta(p+1:len_trim(fichteta))))
!     case('mff')
!       open(unit=10,file=fichteta,form='formatted', position='rewind')
!       read(10,*) nverteta
!       if (nverteta /= nver) stop 'Error (temperature): nverteta /= nver'
!       allocate(teta(nverteta))
!       rewind(10)
!       read(10,*) nverteta, (teta(i), i=1, nverteta)
!       close(10)
!     case('vtu')
!        print*,'Reading a VTU binary file for temperature...'
!        if (vtk_ini_xml_read('Binary',fichteta,'UnstructuredGrid',& 
!             npieces) /=0) stop 'Error (temperature): unable open vtu file'
!        if (npieces > 1) stop 'Error (temperature): too many pieces'
!        if (vtk_var_xml_read('node', nnd, nco, 'Temperature', teta) /=0)&
!            stop 'Error (temperature): unable to read pointdata'
!        if (vtk_end_xml_read() /=0) stop 'Error (temperature): close'
!     case default
!        stop 'Unrecognized temp. file extension'
!     end select
!  end if
      
      
      if (iopdli.eq.1) then
      
      call calpnolin ()
      
      endif
      
      

!---------------------------------------------------------------------------
!                       bloque de calculo                              
!---------------------------------------------------------------------------

 
      call punteros() 
      
      call cmua(mua,mm,nel,nver) 
      
      allocate(c(mua(nver+1)),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: c cannot be allocated'
        stop 1
      endif
      


     call magne2d() 
     
!---------------------------------------------------------------------------
!*                        salida de resultados                             *
!---------------------------------------------------------------------------

!SOLUCION


	  
     call writeVTU(nel,nver,mm,z,'triangle',sol,'Magnetic vector potential (Wb/m)','scalar',&
   &'node',trim(fichsol)//'.vtu')


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
     
!      - extracción de la submalla (submm, subz)
       call extract_mesh(nver, mm, z, nsd, [permagrel%referencias(l)], submm, subz, globv, globel)
       subnel = size(submm,2); subnver = size(subz,2)
       
!INDUCCION MAGNETICA:

!      - extracción del subcampo (subc)
       call extract_field(globv, globel, cb, subc, 'cell', 2)
!      - paso del campo a nodos (subcn)
       call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
       write(cad,*) permagrel%referencias(l)
       call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,'Magnetic flux density (T)', &
       'vector','node',trim(fichindum)//trim(adjustl(cad))//'.vtu')
       
!CAMPO MAGNETICO:

!      - extracción del subcampo (subc)
       call extract_field(globv, globel, ch, subc, 'cell', 2)
!      - paso del campo a nodos (subcn)
       call cell2node(subnver, submm, subc, subcn)
!      - escritura por nodos y subdominio
       write(cad,*) permagrel%referencias(l)
       call writeVTU(subnel,subnver,submm,subz,'triangle',subcn,'Magnetic field (A/m)', &
       'vector','node',trim(fichcampm)//trim(adjustl(cad))//'.vtu')
     enddo
     
!    creacion del PVD

     call writePVD_static(trim(fichindum)//'.pvd', trim(fichindum), permagrel%referencias(1:permagrel%numero))
     call writePVD_static(trim(fichcampm)//'.pvd', trim(fichcampm), permagrel%referencias(1:permagrel%numero))
 
 !ESCRITURA DE LOS CAMPOS POR VERTICES EN FORMATO mff

     call wrtcmp(nver,sol,10,trim(fichsol)//'.mff')
     call wrtcmp(2*nver,rotv,11,trim(fichindum)//'.mff')
     call wrtcmp(2*nver,hv,11,trim(fichcampm)//'.mff')
     
!!!To solve an example with analytical solution
!!     do i=1,nver
!!       b(i)=solexac(z(1,i),z(2,i))
!!!       write(11,*)(sol(i),b(i))
!!     end do
!     call writeVTU(nel,nver,mm,z,'triangle',b,'Solution','scalar',&
!   &'node','solexac'//'.vtu')
    
!!      b=dabs(sol-b)
!!      call norl22d(nel,mm,z,b,xnorl2)
!!!     error_rel=maxval(abs(sol-b)/(abs(sol)+e))
!!      call norl22d(nel,mm,z,sol,xnorl2sol)
    
!!     print*, 'norma 2 del error= ',xnorl2
 !!    print*, 'error relativo en norma= ',xnorl2/xnorl2sol
     
     print*, 'END OF THE PROGRAM'       

     stop
     
     end
  
