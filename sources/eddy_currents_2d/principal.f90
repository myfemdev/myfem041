      PROGRAM eddy2d_plano
!---------------------------------------------------------------------------
!*                        Time-harmonic eddy current problem               *
!                         2D geometry, J = (0,0,J_z)                       *
!                         ALl fields are independent of z-coordinate       *
!---------------------------------------------------------------------------

!      use comprobaciones

      use parametros_electros
      use electros_2D
      use fich_electros
      use malla_2DP1
      use dirichlet
      use neumann
      use derivados
      use module_writeVTU
      use module_fem_extract
      use voltage_drop
      use sourcevolumic
      use postpro
      use intensity_input
      
      use module_readUNV
      
      
      implicit none
      
      
      
      
      interface 
        subroutine cmua(MUA,MM,NEL,NVER)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DIMENSION MUA(*),MM(3,*)
        end subroutine
      end interface
      
      integer :: ii, i, k, j
      integer                       :: l,p,iformat
      character(len=255)            :: cad
      integer                       :: DIM,LNN,LNV,LNE,LNF,nnod,npieces
      integer,          allocatable :: nn(:,:),nrc(:,:)


 
      

!---------------------------------------------------------------------------
!*                        entrada de datos                                 *
!---------------------------------------------------------------------------

      character * 300 arg
      integer length, status, ios, iu

    ! determinar lectura por ficheiro ou por entrada estandar

      if (command_argument_count() >= 1) then
        call get_command_argument(1, arg, length, status)
        if (status /= 0) then
          write(0,*) 'eddy2d_plane: the first command argument '&
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
        endif
      endif

         ! iu = 99
         !open(file='3elec_vdata.dat',unit=iu,form='formatted', status='old',&
         !&  position='rewind', iostat=ios)
         !open(file='muestra_js.dat',unit=iu,form='formatted', status='old',&
         !&  position='rewind', iostat=ios)
         ! open(file='muestra_js.dat',unit=iu,form='formatted', status='old',&
         !&  position='rewind', iostat=ios)
         !open(file='3elec_mixdata.dat',unit=iu,form='formatted', status='old',&
         !&  position='rewind', iostat=ios)
         ! call endat(iu)
      
          !print*
          !print*
          !print*,'----------------------------------------------------------'
          !print*, 'the data has been read'

          !close(iu)
 !interfaz_pendiente        endif     

!interfaz_pendiente      endif

! comprobacion de datos de entrada: serve para endat e readxml

!interfaz_pendiente      if (.not. comprueba()) then
!          write(0,*) 'input data checking failed'
!          stop 1
!      endif


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

  !eddy currents: iopteta = 0 at the moment

      
!---------------------------------------------------------------------------
!     Computing pointers .....                              
!---------------------------------------------------------------------------

      call punteros() 
      
      call cmua(mua,mm,nel,nver) 
      
      allocate(c(mua(nver+1)),stat=ierror)
      if (ierror.ne.0) then
        print*,'error: c cannot be allocated'
        stop 1
      endif            


      call eddy2d() 
      
      
      
    
!---------------------------------------------------------------------------
!*                        salida de resultados                             *
!---------------------------------------------------------------------------
!ESCRITURA DE LOS CAMPOS POR VERTICES EN FORMATO mff

    ! call wrtcmp(nver,dreal(sol),10,trim(fichsolr)//'.mff') 
    ! call wrtcmp(nver,dimag(sol),10,trim(fichsoli)//'.mff') 
    ! call wrtcmp(nver,cdabs(sol),10,trim(fichsolm)//'.mff') 
     
     
     !INDUCCION MAGNETICA:

     if (allocated(cb)) deallocate(cb) 
     allocate(cb(2*nel), STAT = ierror)
     if (ierror .ne. 0) stop 'Error when allocating array cb'
     cb(1:nel*2:2) = rotu(1,1:nel)
     cb(2:nel*2:2) = rotu(2,1:nel)

       
!CAMPO MAGNETICO:

     if (allocated(ch)) deallocate(ch) 
     allocate(ch(2*nel), STAT = ierror)
     if (ierror .ne. 0) stop 'Error when allocating array ch'
     ch(1:nel*2:2) = rotuh(1,1:nel)  
     ch(2:nel*2:2) = rotuh(2,1:nel)

     
     if(sourcevol%numero.gt.0)then
       call savejs()
     endif
     
     
 !Postprocessing in dielectric domain    
     call postproceso()
     
     if(num_inputsv.gt.0)then
        call savej_vdata()
     endif
   
     if(num_inputsi.gt.0)then
       call savej_idata()
     endif

     stop
     
     end
  
