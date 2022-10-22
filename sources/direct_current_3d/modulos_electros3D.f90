!-----------------------------------------------------------------------
!     MODULE COMPILER_DEPENDANT
!-----------------------------------------------------------------------  
   module cp
   integer, parameter :: DOUBLE = selected_real_kind(15, 307)
   end module cp

! -----------------------------------------------------------------------
!     MODULE PARAMETROS_ELECTROS3D
!-----------------------------------------------------------------------                                                                    
!     ndrd   = maximo numero de fronteras Dirichlet  
!     ndfn   = maximo numero de fronteras Neumann       
!     ndcar  = maximo numero de cargas  
!     ndar   = maximo numero de aristas. Se necesita en calcul.f 
!     ndcaras = máximo numero de caras. Se utiliza en ....                           
! -----------------------------------------------------------------------

      module parametros_electros3D

       integer, parameter :: ndrd    =      50 !numero total de referencias Dirichlet (dadas por funcion)
       integer, parameter :: ndrn    =      50 !numero total de referencias Neumann (dadas por funcion)
       integer, parameter :: ndri    =      50 !numero total de referencias donde se da la intensidad (dadas por funcion)
       integer, parameter :: ndf     =     100 !numero total de descriptores de función
       integer, parameter :: funlen  =     120 !tamanho maximo de nombre de funcion
       integer, parameter :: ndcar   =     100
       integer, parameter :: ndar	 =   30000
       integer, parameter :: ndcaras =   30000
       integer, parameter :: ndver   =   120000
       integer, parameter :: ndref   =   200
       integer  :: ierror

      end module parametros_electros3D
      
! -----------------------------------------------------------------------
!     MODULE DERIVADOS
!-----------------------------------------------------------------------                                             

      module derivados3D
      use cp
      use parametros_electros3D

      type interfaz
      integer                         :: numero
      integer,dimension(100)          :: referencias
      real(DOUBLE),dimension(100)     :: valor
      real(DOUBLE),dimension(100)     :: area
      end type interfaz

      type interfaz2
      integer                         :: numero
      integer,dimension(100)          :: referencias
      real(DOUBLE),dimension(100)     :: valor
      logical,dimension(100)          :: constante ! true => por constante ; false => por funcion (usa {vol,sup,cur}%fun)
      end type interfaz2

      type  interfazxy
      integer                         :: numero
      integer,dimension(100)          :: referencias
      integer,dimension(100)          :: iopcond
      integer,dimension(100)          :: fun ! index of function: 1=user_defined,..,n      
      integer,dimension(100)          :: ntab
      real(DOUBLE),dimension(100,100) :: teta
      real(DOUBLE),dimension(100,100) :: valtabx
      real(DOUBLE),dimension(100,100) :: valtaby
      real(DOUBLE),dimension(100,100) :: valtabz
      real(DOUBLE),dimension(100) :: valorx
      real(DOUBLE),dimension(100) :: valory
      real(DOUBLE),dimension(100) :: valorz
      character*120,dimension(100)  :: etiqueta
      end type interfazxy


      type userfun !estructura destinada a gardar os descriptores de funcion
!        character(len=funlen), dimension(ndf) :: fun
!        agora traducese a un enteiro (indice do array functions)

        integer, dimension(ndf) :: fun
        
        ! se todas as funcions son iguais, funs colle ese valor. se non: -1 (Function defined by user). 
        ! Temporal: só necesario para fexac en funciones3D e principal ! calculase en function_number máis abaixo
        integer :: funs

      end type userfun

      type(interfaz)  :: blofron,blopun,neuman, inten
      
      type(interfaz2)  :: carvol,carcur,carsup ! almacenan tanto casos por funcion como por constante

      type(interfazxy)  :: conduc

      type(userfun) :: dir, neu, vol, sup, cur , intf ! dir: array de descriptores de funcion para Dirichlet
      
      ! nombres de funciones
      ! se cambia o tamanho deste array, revisar accesos
      character(len=funlen), dimension(1), parameter :: functions = &
      ['Function defined by user                   ']
   

      ! nombres de funciones de conductividad electrica
      character(len=funlen), dimension(2), parameter :: functions_cond = &
      ['Function defined by user    ',& ! 1
       'cell                        '] ! 3 ! barrel

       contains
       
        ! obtains the index of the function_name in the array. 0 if not found
        integer FUNCTION function_number(function_name, array)
      	implicit none
      	character(len=*), intent(in) :: function_name
        character(len=funlen), dimension(:), intent(in) :: array
      	integer :: i
        function_number = 0
      	do i=1,size(array,1)
            if (function_name == array(i)) then
                function_number = i
                exit
      		endif
      	enddo
      	end	function function_number
          
      end module derivados3D

! -----------------------------------------------------------------------
! MODULE MALLA_3DP1
!-----------------------------------------------------------------------
! Variables asociadas a una malla 3D de elementos P1
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
!    nel   : numero de elementos de la malla                          
!    nver  : numero de vertices de la malla                           
!    nra   : tablero de referencias de las aristas                    
!    nrv   : tablero de referencias de los vertices                                          
!    nsd   : tablero de dominios    
!    z     : tablero de coordenadas 
!   det --------> determinante de la matriz asociada a Fk
!                     (Fk transforma el tetraedro k en el
!                      tetraedro de referencia)
!       binv -------> inversa de la matriz asociada a Fk
!       mm ---------> nos. de los vertices del tetraedro corresp.
!       nrvg -------> nos. de referencia de los vertices en la
!                     numeracion global
!       nrncg ------> nos. de referencia de las caras Neumann en la
!                     numeracion global
!       nrdcg ------> nos. de referencia de las caras Dirichlet en la
!                     numeracion global
!       nrc --------> numero de referencia de las caras
!       nrv --------> numero de referencia de los vertices
!       nead -------> tablero con los elementos adyacentes
!                     a cada tetraedro
!       itin -------> tetraedro al que pertenece inicialmente
!                     cada vertice (se mueven por caract.)
!       it ---------> tetraedro al que pertenece cada vertice
!                     (se hace una determinada asignacion)
!                                                 
! -----------------------------------------------------------------------
      
 module malla_3DP1
  use cp

  integer                  ::  nel
  integer                  ::  nemm
  integer                  ::  nver
  integer,allocatable      ::  nead(:,:)
  integer,allocatable      ::  mm(:,:)
  integer,allocatable      ::  nra(:,:)
  integer,allocatable      ::  nrv(:,:)
  integer,allocatable      ::  nrvg(:)
  integer,allocatable      ::  nrc(:,:)
  integer,allocatable      ::  nsd(:)
  integer,allocatable      ::  it(:)
  integer,allocatable      ::  itin(:)
  real*8,allocatable ::  z(:,:)
  real*8,allocatable ::  det (:)
  real*8,allocatable ::  binv (:,:,:)
  
  ! novos datos
  integer,allocatable :: nelem(:) ! numero de elementos para o indice de carvol
  integer,allocatable :: ensd(:,:) ! serie de elementos para cada indice de carvol
   
end module malla_3DP1

! -----------------------------------------------------------------------
! MODULE ELECTROS3D
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
!    ioppun
!    iopinvol
!    iopincur
!    mua      : puntero de la  matriz almacenada en perfil
!    c        : matriz del sistema                          
!    b        : vector segundo miembro                                                   
!    sol      : vector solucion del sistema 
!    vrefd   : vector valor de la solucion fronteras de bloqueo   
!    ib       : puntero morse para almacenamiento de filas
!    jb : puntero morse para almacenamiento de columnas
!    ivb        : puntero morse para numerar vertices vecinos     
!    iopsl : opcion para resolucion del sistema lineal 
!    epscg: epsilon para test de parada en el gradiente conjugado
!    nitcg: numero maximo de iteraciones para el gradiente conjugado 
!     indc(1,i) -> vector en el que se guardan los 3 numeros
!                      (de los 4 posibles) correspondientes a los
!                      vertices que forman la cara i  
!     inda(1,i) -> vector en el que se guardan los 2 numeros
!                      (de los 4 posibles) correspondientes a los
!                      vertices que forman la arista i  
!     areafi(i)--> suma de las areas de las caras que estan sobre la referencia irefi(i)                                            
! -----------------------------------------------------------------------

     module electros3D
       use cp
       use parametros_electros3D
       

      ! entrada
       integer         ::  nrd
       integer         ::  nrdc
       integer         ::  npd
       integer         ::  nrn
       integer         ::  nri
       integer         ::  irefd(ndrd)
       integer         ::  irefn(ndrn)
       integer         ::  irefi(ndri)
       integer		   ::  iop
       integer		   ::  iopf
       integer		   ::  iopvol
       integer		   ::  iopsup
       integer		   ::  iopcur
       integer		   ::  ioppun

!       integer		   ::  iopinvol ! agora van no mesmo array as por funcion e as por constante
!       integer		   ::  iopincur ! agora van no mesmo array as por funcion e as por constante
!       integer		   ::  iopinsup ! agora van no mesmo array as por funcion e as por constante

       integer         ::  iopblo
       integer         ::  iopblo1
       integer         ::  iopblo2
       integer         ::  iopblo3
       integer         ::  iopneu
       integer         ::  iopneu1
       integer         ::  iopneu2
       integer         ::  iopint
       integer         ::  iopint1
       integer         ::  iopint2
       integer		   ::  iopej
       integer		   ::  iopsl
       real(DOUBLE)    ::  epscg
       integer         ::  nitcg
       character (len=245) :: parej

       integer         :: iopteta
       double precision,allocatable  ::  teta(:)
!c

      ! calculo
       integer,allocatable           ::  ib(:)
       integer,allocatable           ::  jb(:)
       integer,allocatable           ::  ivb(:)
       integer,allocatable           ::  mua(:)
       integer,allocatable           ::  id(:)
       real(DOUBLE),allocatable      ::  camor(:)
       real(DOUBLE),allocatable      ::  cper(:)
       real(DOUBLE),allocatable      ::  xmas(:)
       real(DOUBLE),allocatable  ::  b(:)
       real(DOUBLE),allocatable  ::  sol(:)
       real(DOUBLE),allocatable  ::  xcg(:)
       real(DOUBLE),allocatable  ::  preex(:)
       real(DOUBLE),allocatable  ::  e(:,:)
       real(DOUBLE),allocatable  ::  jc(:,:)
       real(DOUBLE),allocatable  ::  evtu(:)
       real(DOUBLE),allocatable  ::  vexac(:)
       real(DOUBLE),allocatable  ::  err(:)
       real(DOUBLE) ::  rel
       real(DOUBLE) ::  xnorexac
       real(DOUBLE) ::  xnorerr
       real(DOUBLE), dimension (ndri) ::  areafi
       integer, dimension(3,4)   :: indc
       integer, dimension(2,6)   :: inda

      end module electros3D
      
      
 module auxiliar_cargas
 
  integer, allocatable ::  ncaras(:)
  integer, allocatable ::  nodc1(:,:), nodc2(:,:), nodc3(:,:)
  integer, allocatable ::  naristas(:)
  integer, allocatable ::  nod1(:,:), nod2(:,:)
    
 end module auxiliar_cargas


! -----------------------------------------------------------------------
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
! -----------------------------------------------------------------------

      module external_electros3D
      use cp

       real(DOUBLE), external :: fexac
 
      end module external_electros3D
     
! -----------------------------------------------------------------------
! MODULE FICH_ELECTROS3D
!-----------------------------------------------------------------------
! Ficheros
!-----------------------------------------------------------------------
! Se utiliza en:  
!-----------------------------------------------------------------------
! 
!    fichma    : nombre del fichero de la malla
!    fichsol   : nombre del fichero de la solucion
!    fichvexac : nombre del fichero de la solucion exacta
! -----------------------------------------------------------------------

      module fich_electros3D

       character (len=245) fichma
       character (len=245) fichsol
       character (len=245) fichElectricField
       character (len=245) fichvexac
       character (len=245) fichteta
       character (len=245) fichCurrentDensity

      end module fich_electros3D

! -----------------------------------------------------------------------
! MODULE CARGAVOL
!-----------------------------------------------------------------------
! Opciones para las cargas 
!-----------------------------------------------------------------------
! Se utiliza en:  
! -----------------------------------------------------------------------

      module cargavol
      
       use cp
       use derivados3D
 
       CONTAINS

 !    	-----------------------------------
      	real(DOUBLE) FUNCTION fc(nsd,indice)
 !    	----------------------------------- 
      	implicit none
      	integer, INTENT(IN)::nsd
        integer, INTENT(IN)::indice ! indice del array de cargas volumicas

        fc = carvol%valor(indice)
        
      	end	function fc

      end module cargavol


! -----------------------------------------------------------------------
! MODULE CARGASUP
!-----------------------------------------------------------------------
! Opciones para las cargas 
!-----------------------------------------------------------------------
! Se utiliza en:  
! -----------------------------------------------------------------------

 module cargasup
 
  use cp
  use parametros_electros3D
  use derivados3D
   
  implicit none
 
  
  CONTAINS

 !    	-----------------------------------
      	real(DOUBLE) FUNCTION gcst(iref,indice)
 !    	----------------------------------- 
 !      Integral sobre la frontera de la funcion de carga superficial
 !    	Es constante por referencia

      	implicit none
      	integer, INTENT(IN)::iref
        integer, INTENT(IN)::indice ! indice del array de cargas

        gcst = carsup%valor(indice)
        
      	!integer :: i
        !gcst=0.d0
      	!do i=1,carsup%numero
        !	if(iref.eq.carsup%referencias(i)) then		
      	!	gcst = carsup%valor(i) 
        !endif
      	!enddo
      	end	function gcst


! ---------------------------------------------------------------------------------- 
!     Funcion densidad de carga superficial
!     x,y,z: coordenada del punto
!     iref: numero de referencia de la cara
! ---------------------------------------------------------------------------------- 
      
      real(DOUBLE) FUNCTION  gcs(x,y,z,iref,indice)

      implicit none
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN)::iref
      integer, INTENT(IN)::indice ! indice del array de cargas
     
      gcs=0.d0

      if (sup%fun(indice) == 1)     then ! fun_0 == 'Function defined by user'
      		gcs=0.d0
      else
            stop 'Surface source: Function gcs: Function descriptor not found'
      endif

      end	function gcs

       end module cargasup

! -----------------------------------------------------------------------
! MODULE CARGACUR
!-----------------------------------------------------------------------
! Opciones para las cargas 
!-----------------------------------------------------------------------
! Se utiliza en:  
! -----------------------------------------------------------------------

       module cargacur
       use cp
       use parametros_electros3D
       use derivados3D
 
       CONTAINS

 !    	-----------------------------------
      	real(DOUBLE) FUNCTION gcct(iref,indice)
 !    	----------------------------------- 
 !      Integral sobre la frontera de la funcion de carga curvilinea
 !    	Es constante por referencia

      	implicit none
      	integer, INTENT(IN)::iref
      	integer, INTENT(IN)::indice ! indice del array de cargas

        gcct = carcur%valor(indice)
        
      	!integer :: i
        !gcct=0.d0
      	!do i=1,carcur%numero
        !	if(iref.eq.carcur%referencias(i)) then		
      	!	gcct = carcur%valor(i) 
      	!	endif
      	!enddo
      	end	function gcct


! ---------------------------------------------------------------------------------- 
!     Funcion densidad de carga curvilinea
!     x,y,z: coordenadas del punto
!     iref: numero de referencia de la arista
! ---------------------------------------------------------------------------------- 
      
      real(DOUBLE) FUNCTION  gcc(x,y,z,iref,indice)

      implicit none
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN)::iref
      integer, INTENT(IN)::indice ! indice del array de cargas

      gcc = 0.d0

      if (cur%fun(indice) == 1)     then ! fun_0 == 'Function defined by user'
      		gcc=0.d0
      else
            stop 'Line source: Function gcc: Function descriptor not found'
      endif
      

      end	function gcc

 end module cargacur
      
! -----------------------------------------------------------------------
! MODULE CARGAPUN
!-----------------------------------------------------------------------
! Modulo asociado a las cargas puntuales
!-----------------------------------------------------------------------
! Se utiliza en:  
! -----------------------------------------------------------------------

 module cargapun
 
       use cp
       use parametros_electros3D
       use malla_3DP1 , only : nel, mm, z
       
       integer		     ::  ncarpun
       double precision  ::  xcarpun(ndcar)
       double precision  ::  ycarpun(ndcar)
       double precision  ::  zcarpun(ndcar)
       double precision  ::  carpun(ndcar)
       
       CONTAINS

 !    	--------------------------------------------------------
    SUBROUTINE scarpun(xcarpuni,ycarpuni,zcarpuni,itetr,bar1,bar2,bar3,bar4)
 !    	--------------------------------------------------------     	

      	implicit none

        real(DOUBLE), INTENT(IN)   :: xcarpuni, ycarpuni, zcarpuni
        real(DOUBLE), dimension(3) :: xp
        real(DOUBLE), dimension(4) :: bxp
        real(DOUBLE), INTENT(OUT)  :: bar1, bar2, bar3, bar4
        integer :: itetr

      	xp(1)= xcarpuni
        xp(2)= ycarpuni
        xp(3)= zcarpuni

   	    call baric(xp,itetr,bxp,nel,mm,z)
  	  
      	bar1=bxp(1)
      	bar2=bxp(2)
      	bar3=bxp(3)
      	bar4=bxp(4)

      	end	subroutine 	scarpun 

end module cargapun


! -----------------------------------------------------------------------
! MODULE CONDUCTIVIDAD
!-----------------------------------------------------------------------
! Conductividad
!-----------------------------------------------------------------------
! Se utiliza en:  matel.f que es llamada por matriz.f
! -----------------------------------------------------------------------

module conductividad
      
       use cp
       use electros3D,  only: iopej, iopteta,teta
       use derivados3D
       integer		   ::  iopconductividad
       integer		   ::  ndom
       CONTAINS

 !    	--------------------------------------------
      	real(DOUBLE) FUNCTION cond(x,y,z,nsd,nd)
 !    	-------------------------------------------- 
      	implicit none
      	real(DOUBLE), INTENT(IN) :: x, y, z
        integer, INTENT(IN)::nsd,nd
        integer :: i

        cond=1.d0
        


      			   if(nd.eq.1) then	
      			   cond = conduc%valorx(ndom)
                   elseif(nd.eq.2) then
                   cond = conduc%valory(ndom)
                   elseif(nd.eq.3) then
                   cond = conduc%valorz(ndom)
                   endif

!      	endif        opcion_lectura_conductividad
      	
      	

      	end	function cond

        ! retorna conductividade electrica dada por funcion
        real(DOUBLE) function condfun(x,y,z,nsd,nd)
      	implicit none
      	real(DOUBLE), INTENT(IN) :: x, y, z
        integer, INTENT(IN) :: nsd, nd
        
        
!        if (conduc%fun(ndom) == 1) then !  Function defined by user
!            ! edit here
!            condfun = 1.0
!            ! edit here
!                    elseif (conduc%fun(ndom) == 2) then !  ejemplo cuba
!         
!              if (nsd.eq.5) then
!!                                   barra catodica
!             condfun=1./(1.144d-12*750.**2+2.764d-10*750.+1.890d-07)
!              else if (nsd.eq.7) then
!!                                   bloque semigrafitico
!           condfun=1./(-6.911d-14*750.**3+1.31d-10*750.**2-6.69d-08*750.+3.732d-05)
!             else if (nsd.eq.8) then
!!                                   pasta de brascaje
!            condfun=1./(-6.911d-14*750.**3+1.31d-10*750.**2-6.69d-08*750.+3.732d-05)
!!                                               fundicion
!              else if(nsd.eq.27) then
!      
!              condfun=1./((9.954d-10*750.+63.9d-8)/13.d0)
!              
!              
!              endif
        if (trim(conduc%etiqueta(ndom)).eq.'Material name') then
            condfun = 1.0

        else if ((trim(conduc%etiqueta(ndom)).eq.'Test Material 8')) then
            condfun = 1.0
        else
            print*, 'ERROR: Electric conductivity: '//trim(conduc%etiqueta(ndom))//', Function descriptor not found'
            stop 1
        endif
        
        end function condfun

end module conductividad


! -----------------------------------------------------------------------
! MODULE bloqueo
!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
! Se utiliza en:  matel.f que es llamada por matriz.f
! -----------------------------------------------------------------------


      module bloqueo

      use cp
      use parametros_electros3D
      use derivados3D , only: dir
      use electros3D, only: nrd !, lirefd => irefd
      

      ! por constante
      integer	::  nvrebc(ndref)        !numero de vertices donde se bloquea
      integer	::  ivrebc(ndref,ndver)  !numeros de los vertices en los que se bloquea
      ! por funcion
      integer	::  nvrebf(ndref)        !numero de vertices donde se bloquea
      integer	::  ivrebf(ndref,ndver)  !numeros de los vertices en los que se bloquea
      ! innecesario
      !integer   ::  xvrebf(ndref)        !indice en el array de referencias (para pasar a h())
      

       CONTAINS
! ---------------------------------------------------------------------------------- 
!     Funcion de bloqueo para condicion de contorno Dirichlet
! ---------------------------------------------------------------------------------- 
! (x,y,z): coordenadas del punto a bloquear
!  irefd: indice de la referencia Dirichlet

      real(DOUBLE) FUNCTION  h(x,y,z,irefd,indice)

      implicit double precision (a-h,o-z)
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN):: irefd
      real(DOUBLE) :: permi0
      real(DOUBLE) :: pi
      real(DOUBLE) :: q
      real(DOUBLE) :: a
      real(DOUBLE) :: b
      real(DOUBLE) :: qtotal
      integer, INTENT(IN)::indice ! indice en el array de condiciones Dirichlet por funcion
      
      h=0.d0
      
      
     
       if (dir%fun(indice)  == 1) then ! 'Function defined by user'
      !'User defined: Function defined by user'
         h = 0.d0
!         select case(irefd)
!         case(1)
!           h = 0.
!         case(2)
!           h = 1.
!         case default
!           stop 'Function h: irefd not found for descriptor: User defined'           
!         end case
      else
         stop 'Dirichlet B.C.: Function h: Function descriptor not found'
      endif
      
    
      end function h
      
      

      end module bloqueo


! -----------------------------------------------------------------------
! MODULE neumann
!-----------------------------------------------------------------------
! Condicion de contorno Neumann
!-----------------------------------------------------------------------
! Se utiliza en: semi.f???
! -----------------------------------------------------------------------


      module neumann
      use cp
      use derivados3D , only: neu
!      use electros3D, only: nrn, lirefn => irefn 
      CONTAINS


! ---------------------------------------------------------------------------------- 
!     Funcion G: condicion Neumann 
! ---------------------------------------------------------------------------------- 
! (x,y,z) : coordenadas del punto
! irefn   : número de referencia de la frontera Neumman
      
      real(DOUBLE) FUNCTION  g(x,y,z,irefn,indice)

      implicit none
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN)::irefn
      integer, INTENT(IN)::indice ! indice del array de condiciones Neumann por funcion (irefn)




      if (neu%fun(indice)  == 1) then ! 'Function defined by user'
      !'User defined: Function defined by user'
            g = 0.d0
      else
            stop 'Neumann B.C.: Function g: Function descriptor not found'
      endif
            

      end function g


      end module neumann
      
      
      
! -----------------------------------------------------------------------
! MODULE intensidades
!-----------------------------------------------------------------------
! Condicion de contorno Neumann (intensidad de corriente)
!-----------------------------------------------------------------------
! Se utiliza en: semi.f
! -----------------------------------------------------------------------


      module intensidades
      use cp
      use derivados3D , only: intf

      CONTAINS


! ---------------------------------------------------------------------------------- 
!     Funcion G: condicion Neumann 
! ---------------------------------------------------------------------------------- 
! (x,y,z) : coordenadas del punto
! irefi   : número de referencia de la frontera donde se da la intensidad 
      
      real(DOUBLE) FUNCTION  gint(x,y,z,irefi,indice)

      implicit none
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN)::irefi
      integer, INTENT(IN)::indice ! indice del array de condiciones Neumann por funcion (irefn)
     

        gint=0.d0

       if (intf%fun(indice)  == 1) then ! 'Function defined by user'
      !'User defined: Function defined by user'
            gint = 0.d0
      else
            stop 'Neumann B.C. intensity: Function gint: Function descriptor not found'
      endif
            

      end function gint


      end module intensidades
      
! ---------------------------------------------------------------------------------- 
     
       module resolucion_sistema_lineal
       
       ! camorf: matriz camor factorizada      
       use cp
       
       real(DOUBLE),allocatable  ::  camorf(:)
       real(DOUBLE),allocatable  ::  work(:)
    
      end module resolucion_sistema_lineal
