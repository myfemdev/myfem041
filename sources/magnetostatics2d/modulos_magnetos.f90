!*-----------------------------------------------------------------------
!     MODULE PARAMETROS_ELECTROS
!-----------------------------------------------------------------------                                                                    
!     ndrd   = maximo numero de fronteras Dirichlet  
!     ndfn   = maximo numero de fronteras Neumann       
!     ndcar  = maximo numero de cargas  
!     ndar   = maximo numero de aristas. Se necesita en calcul.f                            
!*-----------------------------------------------------------------------

      module parametros_electros

       integer, parameter :: ndel   =    500000
       integer, parameter :: ndver  =    500000 
       integer, parameter :: ndrd   =      50
       integer, parameter :: ndrn   =      50
       integer, parameter :: ndsu   =      50
       integer, parameter :: ndrasu =   30000
       integer, parameter :: ndrane =   30000
       integer, parameter :: ndcar  =     100
       integer, parameter :: ndar   =   30000
       integer            :: ierror
       integer, parameter :: ndvdi = 30000
       integer, parameter :: nddnolin = 100 ! numero maximo de dominios no lineales

       
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
        double precision,dimension(100)   :: valor
        character*255,dimension(100)      :: etiqueta

       end type interfaz2


       type tablaHB
        double precision, allocatable :: h(:), b(:)
       end type

       type interfazxy
        integer                         :: numero
        integer,dimension(100)          :: referencias
        integer,dimension(100)          :: ioplin
        integer,dimension(100)          :: iopermagr
        double precision,dimension(100) :: valorx
        double precision,dimension(100) :: valory
        type (tablaHB),  dimension(100) :: hb
       end type interfazxy
 
       type interfaz1
        double precision,allocatable :: t(:,:),q(:,:)
       end type interfaz1
   
       type(interfaz2)   :: sourcevol
       type(interfaz2)   :: sourcesur
       type(interfaz2)   :: dirichlet_bc
       type(interfaz2)   :: neumann_bc
       type(interfazxy)  :: permagrel
       type(interfaz1)   :: rectas_dat


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
! Variables utilizadas en el bloque de cálculo de electrostatica 2D
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

       use parametros_electros

      ! entrada
       integer           ::  nrd
       integer           ::  nrdc
       integer           ::  npd
       integer           ::  nrn
       integer           ::  nrsu
       integer           ::  irefd(ndrd)
       integer           ::  irefn(ndrn)
       integer		     ::  iop
       integer		     ::  iopf
       integer           ::  iopteta
       
       double precision,allocatable  ::  teta(:)
       integer,allocatable           ::  mua(:)
       double precision,allocatable  ::  c(:)
       double precision,allocatable  ::  b(:)
       double precision,allocatable  ::  bvar(:)
       double precision,allocatable  ::  sol(:)
       
       !vectores postproceso
       integer, allocatable          :: iver(:)
       double precision, allocatable :: rotu(:,:) ! B por elementos
       double precision, allocatable :: rotuh(:,:) ! H por elementos
       double precision, allocatable :: rotv(:,:)  ! B por vertices
       double precision,allocatable  :: campob(:) !B por vertices
       double precision, allocatable :: hv(:,:)  !H por vertices 
       double precision,allocatable  :: campoh(:) !H  por vertices


      end module electros_2D


!*-----------------------------------------------------------------------
! MODULE EXTERNAL_ELECTROS
!-----------------------------------------------------------------------
! Funciones externas
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
! 
!    fichma    : nombre del fichero de la malla
!    fichsol   : nombre del fichero de la solucion
!    permi     : permitividad electrica
!    f         : funcion segundo miembro del problema
!    g         : funcion para condicion Neumann 
!    h         : funcion de bloqueo
!                                                            
!*-----------------------------------------------------------------------

      module external_electros


!       double precision, external :: f
 !      double precision, external :: fexac
 

      end module external_electros
     
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
       character (len=245) fichsol
       character (len=245) fichteta
       character (len=245) fichdensc
       character (len=245) fichcampm
       character (len=245) fichindum

      end module fich_electros

!*-----------------------------------------------------------------------
! MODULE SOURCEVOLUMIC
!-----------------------------------------------------------------------
! Opciones para las cargas 
!-----------------------------------------------------------------------
! Se utiliza en:  
!*-----------------------------------------------------------------------


      module sourcevolumic
      
 
       use derivados
       
       double precision area(100)

      end module sourcevolumic
      
!----------------------------------------------------------------------------------

      module sourcesurface
      
        use parametros_electros
      
        use derivados
       
        integer   :: ntasu
        integer   :: nrasu(ndrasu)
        integer   :: indblosu(ndrasu)
        integer   :: nvasu(2,ndrasu) 
        double precision   :: xlongsu(100) 

     end module sourcesurface






!*-----------------------------------------------------------------------
! MODULE PERMEABILIDAD
!-----------------------------------------------------------------------
! Permeabilidad
!-----------------------------------------------------------------------
! Se utiliza en:  matel.f que es llamada por matriz.f
!*-----------------------------------------------------------------------


      module permeabilidad

       use derivados
 
       CONTAINS

    ! pasar o numero de subdominio ademais do seu indice no array de permeabilidade
 !    	--------------------------------------------
      	DOUBLE PRECISION FUNCTION permer(x,y,nsd,nd,indice,ioplin,ndnolin,idnolin,omega)
 !    	-------------------------------------------- 
      	implicit none
      	double precision, INTENT(IN) :: x, y
        integer, INTENT(IN)::nsd, nd, indice,ioplin,ndnolin
        double precision, INTENT(IN):: omega(*)
        integer, INTENT(IN)::idnolin(*) 
        integer :: mode,i
        double precision :: perme0
      	
      	perme0 = 1.2566370614e-6 

      	permer = 1.0
    	
    	if (ioplin.eq.1) then  !medio lineal
    	
                mode = permagrel%iopermagr(indice)

      	        opcion_lectura_permeabilidad: if (mode.eq.1) then ! funcion
                    print*,'option not programmed yet'
                    stop 1

      	        elseif (mode.eq.2) then ! constante

      			    if(nd.eq.1) then	
      			      permer = permagrel%valorx(indice)
                    elseif(nd.eq.2) then
                      permer = permagrel%valory(indice)
                    endif

      	        endif        opcion_lectura_permeabilidad
      	      
      	 else
      	   
      	    do i=1,ndnolin
      	        if (nsd.eq.idnolin(i))then
      	               permer=1.d0/(omega(i)*perme0)
      	        endif
      	    enddo
      	     
      	endif



      	end	function permer


!     	-----------------------------------
      	DOUBLE PRECISION FUNCTION perme(x,y,nsd,nd,indice,ioplin,ndnolin,idnolin,omega)
!     	----------------------------------- 

      	implicit none
      	double precision, INTENT(IN) :: x, y
        integer, INTENT(IN)::nsd,nd,indice,ioplin,ndnolin
        double precision, INTENT(IN):: omega(*)
        integer, INTENT(IN)::idnolin(*) !Alfredo
        double precision :: perme0
      	perme0 = 1.2566370614e-6 ! permeabilidad magnetica del vacio (en.wikipedia)
      	perme = perme0 * permer(x,y,nsd,nd,indice,ioplin,ndnolin,idnolin,omega)
      	end function perme

      end module permeabilidad 

!----------------------------------------------------------------------------------

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
      
      use parametros_electros
       
       
      integer                   :: ntane
      integer   :: nrane(ndrane)
      integer   :: indblone(ndrane)
      integer   :: nvane(2,ndrane)
      double precision :: xlongne(100)

      
      end module neumann

!----------------------------------------------------------------------------------

       module nolineal
       
       use parametros_electros


       integer            :: ndnolin            ! numero de dominios no lineales
       integer            :: idnolin(nddnolin)   ! vector que contiene los numeros de referencia de los dominios no lineales. Alfredo
       double precision   :: omega(nddnolin)
       double precision   :: lambda(nddnolin)
 
       integer            :: iopdli ! variable que indica si hay o no dominios no lineales
       integer            :: niter  !numero maximo de iteraciones del algoritmo
       double precision   :: e !pequeno parametro para el test de parada
       end module nolineal


