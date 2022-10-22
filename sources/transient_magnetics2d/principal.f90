      PROGRAM magnetostatics2d

      use comprobaciones
      use parametros_electros
      use electros_2D
      use fich_electros
      use malla_2DP1
      use dirichlet
      use neumann
      use sourcesurface
      use derivados
      use module_writeVTU
      use nolineal, only: iopdli
      use module_fem_extract
      use module_writePVD
      use potenciales_vol
      use potenciales_sur
      use tiempo
      
      use module_readUNV
      use LIB_VTK_IO_READ
      
      implicit none
      
      integer :: ii, i
      double precision solexac, error_rel, error_abs, h, hexac,xnorl2
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
!Transient: no properties with temperature dependence in this version
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

      if(potencial_dat_vol%numero.gt.0) then
        call integra(nel,mm,z,xintsvol)
      end if

      if(potencial_dat_sur%numero.gt.0) then
        call integra_su(nel,mm,z,xintssur)
      end if

      call cmua2(mua,mm,nel,nver)    

      !write(17,*) (mua(i),i=1,nver+1)
      
      allocate(c(mua(nver+1)),stat=ierror)
      
      if (ierror.ne.0) then
        print*,'error: c cannot be allocated'
        stop
      endif

     call magne2d() 
     

   
     print*, 'END OF THE PROGRAM'       

     stop
     
     end
  
