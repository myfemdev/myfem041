     module parametros_electros

     
       integer, parameter :: ndrane =   30000
       integer            :: ierror
       integer, parameter :: ndvdi = 30000
       
      end module parametros_electros
      
!*-----------------------------------------------------------------------
!     MODULE DERIVADOS
!-----------------------------------------------------------------------                                             
!*-----------------------------------------------------------------------
      module derivados


       type interfaz2

        integer                           :: numero
        integer, dimension (100)          :: itipo ! 1: densidad de corriente, 2:intensidad
        integer,dimension(100)            :: modo ! 1: funcion 2: constante
        integer,dimension(100)            :: referencias
        double complex,dimension(100)   :: valor
        character*9,dimension(100)        :: etiqueta

       end type interfaz2
       
       type interfaz3

        integer                           :: numero
        integer, dimension (100)          :: itipo ! 1: densidad de corriente, 2:intensidad
        integer,dimension(100)            :: modo ! 1: funcion 2: constante
        integer,dimension(100)            :: referencias
        double precision,dimension(100)   :: vrms
        double precision,dimension(100)   :: vphase
        character*9,dimension(100)        :: etiqueta

       end type interfaz3


      
       type interfazprop1
        integer                         :: numero
        integer,dimension(100)          :: referencias
        integer,dimension(100)          :: iopermagr
        double precision,dimension(100) :: valor
       end type interfazprop1
       
       
        type  interfazprop2
          integer                             :: numero
          integer,dimension(200)              :: referencias
          integer,dimension(200)              :: iopcond
          integer,dimension(200)              :: ntab
          double precision,dimension(200,200) :: teta
          double precision,dimension(200,200) :: valtab
          double precision,dimension(200) :: valor
       end type interfazprop2
       
        type interfaz4
       
        integer                           :: numero
        integer,dimension(200)            :: referencias     
        integer,dimension(200)            :: modo ! 1: funcion 2: constante (caida potential)
        double precision,dimension(200)   :: vrms
        double precision,dimension(200)   :: vphase

        character*9,dimension(200)        :: etiqueta !para function


       end type interfaz4

       
       type domain_prop
           logical,allocatable                :: mododiel(:) ! true dielectric; false conductor
           logical,allocatable                :: jsdata(:) ! true Js dada; false voltage or intensity data
           logical,allocatable                :: vdata(:)
           logical, allocatable               :: idata(:)
       end type domain_prop

       
       type voltage_input
          integer :: nsubdo
          integer, dimension(100):: subdo_references
          double precision:: vrms
          double precision:: vphase
          character(len=255)  :: etiqueta
!          integer :: modo
       end type voltage_input
 
          
       type(interfaz3)   :: sourcevol
       type(interfaz2)   :: dirichlet_bc
       type(interfaz2)   :: neumann_bc
       type(interfazprop1)  :: permagrel
       type(interfazprop2)  :: conduc
!       type(interfaz4)      :: potencial_dat_vol
!       type(interfaz4)      :: intens_dat
       type(domain_prop)    :: domains
       type (voltage_input), dimension(:), allocatable :: inputsv
       type (voltage_input), dimension(:), allocatable :: inputsi
       

      end module derivados

!*-----------------------------------------------------------------------
! MODULE MALLA_2DP1
!-----------------------------------------------------------------------
! Variables asociadas a una malla 2D de elementos P1
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
!    nel   : numero de elementos de la malla                          
!    nver  : numero de vertices de la malla                           
!    mm    : tablero de numeracion de los vertices                                  
!    nra   : tablero de referencias de las aristas                    
!    nrv   : tablero de referencias de los vertices                                          
!    nsd   : tablero de dominios    
!    z     : tablero de coordenadas                                              
!*-----------------------------------------------------------------------
      
      module malla_2DP1

       integer                      ::  nel
       integer                      ::  nver
       integer,allocatable          ::  mm(:,:)
       integer,allocatable          ::  nra(:,:)
       integer,allocatable          ::  nrv(:,:)
       integer,allocatable          ::  nsd(:)
       double precision,allocatable ::  z(:,:)
       
       ! Se definen como en electrostatica 3D para el ensamblado de las cargas volumicas
       integer,allocatable :: nelem(:) ! numero de elementos para cada subdominio
       integer,allocatable :: ensd(:,:) ! serie de elementos para cada subdominio
       
      
      end module malla_2DP1


!*-----------------------------------------------------------------------
! MODULE ELECTROS_2D
!-----------------------------------------------------------------------
! Variables utilizadas en el bloque de cálculo 
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
! 
!    nfd      : numero de referencias Dirichlet
!    nfn      : numero de referencias Neumann 
!    ifd      : tablero de referencias Dirichlet
!    ifn      : tablero de referencias Neumann
!    iop      : opcion para formula de cuadratura matriz y segundo miembro
!    iopf     : opcion para formula de cuadratura para terminos frontera 
!    iopvol
!    iopcur
!    iopinvol
!    mua      : puntero de la  matriz almacenada en perfil
!    c        : matriz del sistema                          
!    b        : vector segundo miembro                                                   
!    sol      : vector solucion del sistema 
!    vrefd    : vector valor de la solucion fronteras de bloqueo                                                         
!*-----------------------------------------------------------------------

      module electros_2D

       use parametros_electros, only: ierror

      ! entrada
       integer           ::  nrd
       integer           ::  nrdc
       integer           ::  npd
       integer           ::  nrn
       integer           ::  nrsu
      ! integer           ::  irefd(ndrd)
      ! integer           ::  irefn(ndrn)
       integer		     ::  iop
       integer		     ::  iopf
       integer           ::  iopteta
       
       double precision,allocatable  ::  teta(:)
       integer,allocatable           ::  mua(:)
       double complex,allocatable    ::  c(:)
       double complex,allocatable    ::  b(:)
       double complex,allocatable    ::  sol(:)
       
       double precision              :: pi
       double precision              :: frec
       double precision              :: omega
       double complex, allocatable   :: xintens(:)
       !vectores postproceso
       integer, allocatable          :: iver(:)
       double complex, allocatable :: rotu(:,:) ! B por elementos
       double complex, allocatable :: rotuh(:,:) ! H por elementos
       double complex, allocatable :: rotv(:,:)  ! B por vertices
       double complex,allocatable  :: campob(:) !B por vertices
       double complex, allocatable :: hv(:,:)  !H por vertices 
       double complex,allocatable  :: campoh(:) !H  por vertices
       
       integer  :: imaxnsd

       


      end module electros_2D



     
!*-----------------------------------------------------------------------
! MODULE FICH_ELECTROS
!-----------------------------------------------------------------------

! Ficheros
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
! 
!    fichma    : nombre del fichero de la malla
!    fichsol   : nombre del fichero de la solucion
!*-----------------------------------------------------------------------

      module fich_electros

       character (len=245) fichma
       character (len=245) fichsolr
       character (len=245) fichsoli
       character (len=245) fichsolm
       
       character (len=245) fichteta
       character (len=245) fichdenscr
       character (len=245) fichdensci
       character (len=245) fichdenscm

       character (len=245) fichcampmr
       character (len=245) fichcampmi
       character (len=245) fichcampmm
       
       character (len=245) fichindumr
       character (len=245) fichindumi
       character (len=245) fichindumm
       
       character (len=245) fichjoule
       
       character(len=245) fichpot
       character(len=245) fichintens
       character(len=245) fichvolt
       
       character(len=245) fichflo

      end module fich_electros

!*-----------------------------------------------------------------------
! MODULE SOURCEVOLUMIC
!-----------------------------------------------------------------------


      module sourcevolumic
      
 
       use derivados
       
       double precision area(10000)

      end module sourcevolumic
      

!*-----------------------------------------------------------------------
! MODULE PERMEABILIDAD
!-----------------------------------------------------------------------
! Permeabilidad
!-----------------------------------------------------------------------
! Se utiliza en:  matel1.f que es llamada por cmrds.f
!*-----------------------------------------------------------------------


      module permeabilidad

       use derivados
 
       CONTAINS

 !    	--------------------------------------------
      	DOUBLE PRECISION FUNCTION permer(x,y,nsd,indice)
 !    	-------------------------------------------- 
      	implicit none
      	double precision, INTENT(IN) :: x, y
        integer, INTENT(IN)::nsd, indice
        integer :: mode,i
        double precision :: perme0
      	
      	perme0 = 1.2566370614e-6 

      	permer = 1.0
    	
    	 !medio lineal
    	
        mode = permagrel%iopermagr(indice)
        opcion_lectura_permeabilidad: if (mode.eq.1) then ! funcion
                 print*,'option not programmed yet'
                 stop 1
 	        elseif (mode.eq.2) then ! constante

   			      permer = permagrel%valor(indice)
   	        endif        opcion_lectura_permeabilidad

      	end	function permer


!     	-----------------------------------
      	DOUBLE PRECISION FUNCTION perme(x,y,nsd,indice)
!     	----------------------------------- 

      	implicit none
      	double precision, INTENT(IN) :: x, y
        integer, INTENT(IN)::nsd,indice
        double precision :: perme0
      	perme0 = 1.2566370614e-6 ! permeabilidad magnetica del vacio (en.wikipedia)
      	perme = perme0 * permer(x,y,nsd,indice)
      	end function perme

      end module permeabilidad 

!----------------------------------------------------------------------------------


!*-----------------------------------------------------------------------
! MODULE CONDUCTIVIDAD
!-----------------------------------------------------------------------
! Conductividad eléctrica
!-----------------------------------------------------------------------
! Se utiliza en:  matel2.f que es llamada por cmrds.f
!*-----------------------------------------------------------------------


      module conductividad

       use derivados
 
       CONTAINS

 !    	--------------------------------------------
      	DOUBLE PRECISION FUNCTION conducel(nsd,indice)
 !    	-------------------------------------------- 
      	implicit none
        integer, INTENT(IN)::nsd, indice
        integer :: mode,i
    	
    	
        mode = conduc%iopcond(indice)
        opcion_lectura_conductividad: if (mode.eq.1) then ! funcion
                 print*,'option not programmed yet'
                 stop 1
 	        elseif (mode.eq.2) then ! constante

   			      conducel = conduc%valor(indice)
   	        endif        opcion_lectura_conductividad

      	end	function conducel



      end module conductividad 
!---------------------------------------------------------------------

      module dirichlet

       use derivados
       use parametros_electros , only : ndvdi


!---------------------------------------------------------------------

!    nvdi   : numero de vertices Dirichlet
!    nvd    : numero de cada vertice Dirichlet
!    nrvd   : referencia de cada vertice Dirichlet
!    indblo : tablero de indices
!---------------------------------------------------------------------
   
       integer :: nvdi
       integer :: nvd(ndvdi)
       integer :: nrvd(ndvdi)
	   integer :: indblo(ndvdi)
     
      end module dirichlet


!*-----------------------------------------------------------------------
! MODULE neumann
!-----------------------------------------------------------------------
! Condicion de contorno Neumann
!-----------------------------------------------------------------------

      module neumann

      use derivados
      
      use parametros_electros, only : ndrane
       
       
      integer                   :: ntane
      integer   :: nrane(ndrane)
      integer   :: indblone(ndrane)
      integer   :: nvane(2,ndrane)
      double precision :: xlongne(10000)

      
      end module neumann

!----------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module voltage_drop
      
      
      
        use derivados
               integer:: num_inputsv

        
       integer,allocatable :: nelempo(:) ! numero de elementos para cada subdominio con potencial dato
       integer,allocatable :: ensdpo(:,:) ! serie de elementos para cada subdominio
       double precision, allocatable :: areapo(:)     
       double complex, allocatable:: int_pot(:)     
  

        
      end module voltage_drop
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module intensity_input
      
      
      
        use derivados
               integer:: num_inputsi

        
       integer,allocatable :: nelemint(:) ! numero de elementos para cada subdominio con intnesidad dato
       integer,allocatable :: ensdint(:,:) ! serie de elementos para cada subdominio
       double precision, allocatable :: areaint(:) 
       double complex, allocatable :: co(:,:) 
       double complex, allocatable:: pot_int(:)     
        
      end module intensity_input     
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module postpro
        double complex, allocatable :: subc(:),cj(:),subcn(:),subcaux(:) 
        double precision, allocatable:: subz(:,:)
        double complex, allocatable :: cb(:), ch(:)
        double complex, allocatable :: subcncm(:),subcnrms(:)
        integer, allocatable :: submm(:,:), globv(:), globel(:)
        integer :: subnel, subnver   
      end module postpro         

      
