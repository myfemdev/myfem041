!-----------------------------------------------------------------------
!     MODULE COMPILER_DEPENDANT
!-----------------------------------------------------------------------
!      module cp
!      integer, parameter :: DOUBLE = selected_real_kind(15, 307)
!      end module cp

!*-----------------------------------------------------------------------
!     MODULE PARAMETROS_ELECTROS
!------------------------------------------------------------------------
!    ndrd   : maximum number of Dirichlet boundaries
!    ndfn   : maximum number of Neumann boundaries
!    ndcar  : maximum number of charges
!    ndar   : maximum number of edges
!*-----------------------------------------------------------------------

  module parametros_electros

     integer, parameter :: ndrd   =      50
     integer, parameter :: ndrn   =      50
     integer, parameter :: ndri   =      50
     integer, parameter :: ndcar  =     100
     integer, parameter :: ndar	  =   30000
     integer  :: ierror


  end module parametros_electros

!*-----------------------------------------------------------------------
!     MODULE DERIVADOS
!------------------------------------------------------------------------
!    blofron  : strong imposition of Dirichlet conditions by constants
!               (referencias==boundary numbers)
!    blopun   : strong imposition of Dirichlet conditions by points
!               (referencias==point numbers)
!    neuman   : strong imposition of Neumann conditions by constants
!               (referencias==boundary numbers       )
!    carvol   : volumic charge entry by array
!               (referencias==subdomain numbers)
!    carcur   : curvilinear charge entry by array
!               (referencias==subdomain numbers)
!    inten    : current intensity by constants
!    conduc   : relative electric conductivity
!*-----------------------------------------------------------------------

  module derivados

     type interfaz
       integer                         :: numero
       integer,dimension(100)          :: referencias
       double precision,dimension(100) :: valor
       double precision,dimension(100) :: thickness
     end type interfaz


     type  interfazxy
       integer                             :: numero
       integer,dimension(100)              :: referencias
       integer,dimension(100)              :: iopcond
       integer,dimension(100)              :: ntab
       double precision,dimension(100,100) :: teta
       double precision,dimension(100,100) :: valtabx
       double precision,dimension(100,100) :: valtaby
       double precision,dimension(100) :: valorx
       double precision,dimension(100) :: valory
       character(len=120),dimension(100) :: etiqueta
     end type interfazxy

     type(interfaz)   :: blofron,blopun,carvol,carcur,neuman,inten
     type(interfazxy) :: conduc

  end module derivados

!*----------------------------------------------------------------------
! MODULE MALLA_2DP1
!-----------------------------------------------------------------------
! Variables associated to a 2D mesh with P1 elements
!-----------------------------------------------------------------------
!    nel   : number of elements of the mesh
!    nver  : number of vertices of the mesh
!    mm    : connectivity matrix
!    nra   : array with the numbers of the edges
!    nrv   : array with the references of the vertices
!    nsd   : domains array
!    z     : array of coordinates
!*----------------------------------------------------------------------

  module malla_2DP1

     integer                      ::  nel
     integer                      ::  nver
     integer,allocatable          ::  mm(:,:)
     integer,allocatable          ::  nra(:,:)
     integer,allocatable          ::  nrv(:,:)
     integer,allocatable          ::  nsd(:)
     double precision,allocatable ::  z(:,:)

  end module malla_2DP1


!*----------------------------------------------------------------------
! MODULE ELECTROS_2D
!-----------------------------------------------------------------------
! Variables used in the computations in electrostatica 2D
!-----------------------------------------------------------------------
! Se utiliza en:
!-----------------------------------------------------------------------
!    iopblo     : strong imposition of Dirichlet
!                 conditions option (1 Yes 0 No)
!    iopblo1    : input via function (1 Yes 0 No)
!    iopblo2    : input via constants in the boundaries (1 Yes 0 No)
!    iopblo3    : input via punctual blocking (1 Yes 0 No)
!
!    nrd        : number of Dirichlet references, input via function
!    irefd      : array of Dirichlet references, input via function
!    DirBC_Func : Dirichlet function label
!
!    iopneu     : Neumann references option (1 Yes 0 No)
!    iopinneu1  : input via function (1 Yes 0 No)
!    iopinneu2  : input via constants in the boundaries (1 Yes 0 No)
!    nrn        : number of Neumann references, input via function
!    irefn      : array of Neumann references, input via function
!    NeuBC_Func : Neumann function label
!
!    iop      : quadrature formula for matrix and second member option
!               (1 baricentres 2 vertices ELSE midpoints)
!    iopf     : quadrature formula for the boundary terms
!               (1 Gauss 2 trapezoidal rule)
!
!    iopint   : current intensity option (1 Yes 0 No)
!    iopint2  : input via constants in the boundaries (1 Yes 0 No)
!
!    iopteta  : temperature file option (1 Yes 0 No)
!    teta     : temperature in the vertices
!
!    mua      : pointer to the profile stored matrix
!    c        : system matrix
!    b        : second member vector
!    sol      : solution vector
!*----------------------------------------------------------------------

  module dcurrent_2D

     use parametros_electros

     integer           ::  nrd
     integer           ::  nrdc
     integer           ::  npd
     integer           ::  nrn
     integer           ::  irefd(ndrd)
     integer           ::  irefn(ndrn)
     integer		   ::  iop
     integer           ::  iopf

     character(len=255) :: DirBC_Func, NeuBC_Func

     integer           ::  iopblo
     integer           ::  iopblo1
     integer           ::  iopblo2
     integer           ::  iopblo3
     integer           ::  iopneu
     integer           ::  iopinneu1
     integer           ::  iopinneu2
     integer           ::  ichneu
     integer		     ::  iopej

     integer           :: iopteta

     integer           :: nri
     integer           :: iopint
     integer           :: iopint2
     integer           ::  irefi(ndri)

     double precision  ::  areafi(ndri)
     double precision,allocatable  ::  teta(:)

     integer,allocatable           ::  mua(:)
     double precision,allocatable  ::  c(:)
     double precision,allocatable  ::  b(:)
     double precision,allocatable  ::  sol(:)
     double precision,allocatable  ::  vexac(:)
     double precision,allocatable  ::  err(:)
     double precision,allocatable  ::  e(:,:)
     double precision,allocatable  ::  jc(:,:)
     double precision,allocatable  ::  evtu(:)

  end module dcurrent_2D

!*----------------------------------------------------------------------
! MODULE EXTERNAL_ELECTROS
!-----------------------------------------------------------------------
! External functions
!*----------------------------------------------------------------------

  module external_electros

     double precision, external :: fexac

  end module external_electros

!*----------------------------------------------------------------------
! MODULE FICH_ELECTROS
!-----------------------------------------------------------------------
! Ficheros
!-----------------------------------------------------------------------
! Se utiliza en:
!-----------------------------------------------------------------------
!    fichma             : mesh file
!    fichsol            : solution file
!    fichteta           : temperature file
!    fichElectricField  : electric field file
!    fichCurrentDensity : current density file
!*----------------------------------------------------------------------

  module fich_electros

     character (len=245) fichma
     character (len=245) fichsol
     character (len=245) fichteta
     character (len=245) fichElectricField
     character (len=245) fichCurrentDensity

  end module fich_electros

!*----------------------------------------------------------------------
! MODULE CONDUCTIVIDAD
!-----------------------------------------------------------------------
! Electric conductivity
!*----------------------------------------------------------------------

  module conductividad

     use derivados

  CONTAINS

     double precision function condfun(x,y,nsd,nd)

     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::nsd,nd

     condfun= 1.d0

     iopcond=0
 	 do i=1,conduc%numero
      	if (nsd.eq.conduc%referencias(i)) then
           iopcond=conduc%iopcond(i)
           ndom=i
      	endif
     enddo

     if(iopcond.eq.0) stop 'dominio sin asignar conductividad'


     opcion_lectura_conductividad: if (iopcond.eq.1) then

!!!!!!!!!!!Edit here
           if (trim(conduc%etiqueta(ndom)).eq.'Material name') then
                       condfun=1.d0
           else
               print*, 'ERROR: Electric conductivity: '//trim(conduc%etiqueta(ndom))//', Function descriptor not found'
               stop 1
           endif
!!!!!!!!!!!Edit here

     elseif (iopcond.eq.2) then

      	if (nd.eq.1) then
      	   condfun= conduc%valorx(ndom)
        elseif(nd.eq.2) then
           condfun = conduc%valory(ndom)
        endif


      	endif opcion_lectura_conductividad

     end function condfun

  end module conductividad

!*----------------------------------------------------------------------
! MODULE bloqueo
!-----------------------------------------------------------------------
! Function for the strong imposition of the boundary condition
!*----------------------------------------------------------------------

  module bloqueo

     use dcurrent_2D,  only: DirBC_Func

  contains

     double precision function  h(x,y,irefd)

     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::irefd
     character(len=255) :: message

     h=0d0

     if (trim(adjustl(DirBC_Func)) == 'User_Defined') then
!       Dirichlet Boundary Condition defined as a function by User
!       Please modify the code below in case you want to provide your own
!       function to compute Dirichlet boundary condition

        h = 10.

     elseif (trim(adjustl(DirBC_Func)) == 'Example_3') then

        if(irefd.eq.4)then
           h=3.*x**2+10.
        endif
     elseif (trim(adjustl(DirBC_Func)) == 'Example_1') then

        if(irefd.eq.1.or.irefd.eq.2)then
           h=3.*x**2-y**2+10.
        endif

     elseif (trim(adjustl(DirBC_Func)) == 'Example_2') then


        if(irefd.eq.3.or.irefd.eq.4)then
           h=dlog(6.*x+4.*y+2.)
        endif

     else

        print*,'Dirichlet BC as a function not recognized:'//trim(adjustl(DirBC_Func))
        stop 1

     endif

     end function h

  end module bloqueo


!*----------------------------------------------------------------------
! MODULE neumann
!-----------------------------------------------------------------------
! Neumann boundary condition
!*----------------------------------------------------------------------

  module neumann

     use derivados

     use dcurrent_2D, only: NeuBC_Func


  contains

     double precision function g(x,y,irefn)

     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::irefn

     g=0.
     if (trim(adjustl(NeuBC_Func)) == 'User_Defined') then
!       Neumann Boundary Condition defined as a function by User
!       Please modify the code below in case you want to provide your own
!       function to compute Neumann boundary condition

        g = 0.

     elseif (trim(adjustl(NeuBC_Func)) == 'Example_1') then

        if(irefn.eq.4) then
           g=-12.*x
        elseif(irefn.eq.3) then
           g=12.*x
        endif

     elseif (trim(adjustl(NeuBC_Func)) == 'Example_2') then

        if(irefn.eq.1) then
           g=4.
        elseif(irefn.eq.2) then
           g=-4.
        endif

     else

        print*, 'Neumann BC as a function not recognized:'//trim(adjustl(NeuBC_Func))
        stop 1

     endif
     end function g

  end module neumann
