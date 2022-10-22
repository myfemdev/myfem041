!*----------------------------------------------------------------------
!     MODULE PARAMETROS_ELECTROS
!-----------------------------------------------------------------------
!    ndrd   : maximum number of Dirichlet boundaries 
!    ndfn   : maximum number of Neumann boundaries       
!    ndcar  : maximum number of charges
!    ndar   : maximum number of edges                            
!    ierror 
!*----------------------------------------------------------------------

  module parametros_electros

   integer, parameter :: ndrd   =      50
   integer, parameter :: ndrn   =      50
   integer, parameter :: ndcar  =     100
   integer, parameter :: ndar   =   30000
   integer  :: ierror
 
  end module parametros_electros

!*----------------------------------------------------------------------
!     MODULE DERIVADOS
!-----------------------------------------------------------------------
!    blofron  : strong imposition of Dirichlet conditions by constants
!               (referencias==boundary numbers)
!    blopun   : strong imposition of Dirichlet conditions by points
!               (referencias==point numbers)
!    neuman   : strong imposition of Neumann conditions by constants
!               (referencias==boundary numbers)
!    carvol   : volumic charge entry by array
!               (referencias==subdomain numbers)
!    carcur   : curvilinear charge entry by array
!               (referencias==subdomain numbers)
!    permirel : relative permitivity entry per subdomain
!               (numero==numbers assigned to the different subdomains
!                referencias==subdomain numbers
!                iopermir==relative permitivity option
!                  (1 function 2 constant per subdomain
!                  3 array depending on temperature)
!                valorx,valory==rel perm,iopermir.eq.2
!                ntab,valtabx,valtaby,teta==rel perm array)
!*----------------------------------------------------------------------

  module derivados

   type interfaz
     integer                         :: numero
     integer,dimension(100)          :: referencias
     double precision,dimension(100) :: valor
   end type interfaz

   type  interfazxy
     integer                             :: numero
     integer,dimension(100)              :: referencias
     integer,dimension(100)              :: iopermir
     integer,dimension(100)              :: ntab
     double precision,dimension(100,100) :: teta
     double precision,dimension(100,100) :: valtabx
     double precision,dimension(100,100) :: valtaby
     double precision,dimension(100)     :: valorx
     double precision,dimension(100)     :: valory
     character(len=120),dimension(100)   :: etiqueta
   end type interfazxy
 
   type(interfaz)    :: blofron,blopun,neuman,carvol,carcur
   type(interfazxy)  :: permirel

  end module derivados

!*----------------------------------------------------------------------
!     MODULE MALLA_2DP1
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
!     MODULE ELECTROS_2D
!-----------------------------------------------------------------------
! Variables used in the computations in electrostatica 2D
!-----------------------------------------------------------------------
!    iopblo     : strong imposition of Dirichlet 
!                 conditions option (1 Yes 0 No)
!    iopblo1    : input via function (1 Yes 0 No)
!    iopblo2    : input via constants in the boundaries (1 Yes 0 No)
!    iopblo3    : input via punctual blocking (1 Yes 0 No) 
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
!
!    iopvol   : volumic charge option (1 Yes 0 No) 
!    iopinvol : input option for the volumic charge 
!               (1 Via function 2 Via array 3 Via file)
!    iopcur   : curvilinear charge option (1 Yes 0 No) 
!    iopincur : input option for the curvilinear charge 
!               (1 Via funciton 2 Via array 3 Via file)
!    ioppun   : punctual charge option (1 Yes 0 No) 
!
!    iopteta  : temperature file option (1 Yes 0 No)
!    teta     : temperature in the vertices
!
!    mua      : pointer to the profile stored matrix 
!    c        : system matrix                          
!    b        : second member vector
!    sol      : solution vector 
!*----------------------------------------------------------------------

  module electros_2D

   use parametros_electros

   integer            ::  nrd
   integer            ::  nrdc
   integer            ::  npd
   integer            ::  nrn
   integer            ::  irefd(ndrd)
   integer            ::  irefn(ndrn)
   integer            ::  iop
   integer            ::  iopf
   integer            ::  iopvol
   integer            ::  iopcur
   integer            ::  ioppun
   integer            ::  iopinvol
   integer            ::  iopincur
   character(len=255) :: DirBC_Func, NeuBC_Func
   integer            ::  iopblo
   integer            ::  iopblo1
   integer            ::  iopblo2
   integer            ::  iopblo3
   integer            ::  iopneu
   integer            ::  iopinneu1
   integer            ::  iopinneu2
   integer            ::  ichneu
   integer            ::  iopej

   integer                       :: iopteta
   double precision,allocatable  ::  teta(:)

   integer,allocatable           ::  mua(:)
   double precision,allocatable  ::  c(:)
   double precision,allocatable  ::  b(:)
   double precision,allocatable  ::  sol(:)
   double precision,allocatable  ::  vexac(:)
   double precision,allocatable  ::  err(:)
   double precision,allocatable  ::  e(:,:)
   double precision,allocatable  ::  evtu(:)

  end module electros_2D

!*----------------------------------------------------------------------
!     MODULE EXTERNAL_ELECTROS
!-----------------------------------------------------------------------
! External functions 
!-----------------------------------------------------------------------
!    fexac
!*----------------------------------------------------------------------

  module external_electros

   double precision, external :: fexac

  end module external_electros
     
!*----------------------------------------------------------------------
!     MODULE FICH_ELECTROS
!-----------------------------------------------------------------------
! Files
!-----------------------------------------------------------------------
!    fichma            : mesh file 
!    fichsol           : solution file 
!    fichteta          : temperature file 
!    fichElectricField : electric field file 
!*----------------------------------------------------------------------

  module fich_electros

   character (len=245) fichma
   character (len=245) fichsol
   character (len=245) fichteta
   character (len=245) fichElectricField

  end module fich_electros

!*----------------------------------------------------------------------
!     MODULE CARGAVOL
!-----------------------------------------------------------------------
! Options for the volumic charges 
!*----------------------------------------------------------------------

  module cargavol

   use derivados
 
   contains

    double precision function fc(nsd)
     implicit double precision (a-h,o-z)
     integer, intent(in)::nsd

     fc=0.d0
     do i=1,carvol%numero
       if(nsd.eq.carvol%referencias(i)) then 
         fc = carvol%valor(i)
       endif
     enddo
    end function fc

  end module cargavol

!*-------------------------------------------------------------------------
!     MODULE CARGACUR
!--------------------------------------------------------------------------
! Options for the curvilinear charges
!--------------------------------------------------------------------------
!    gcct : integral over the boundary of the curvilinear charge function. 
!           Is constant over each reference 
!    gcc  : density of curvilinear charge function 
!*-------------------------------------------------------------------------

  module cargacur

   use parametros_electros
   use derivados

   dimension  nrinterior(ndcar)
 
   contains

    double precision function gcct(iref)
     implicit double precision (a-h,o-z)
     integer, intent(in)::iref

     gcct=0.d0
     do i=1,carcur%numero
       if(iref.eq.carcur%referencias(i)) then 
         gcct = carcur%valor(i) / nrinterior(i)
       endif
     enddo
    end function gcct

    double precision function  gcc(x,y,iref) 
     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::iref

     gcc = 0.d0

    end function gcc

  end module cargacur

!*---------------------------------------------------------------------------
!     MODULE CARGAPUN
!----------------------------------------------------------------------------
! Module associated to the punctual charges
!----------------------------------------------------------------------------
!    ncarpun : number of punctual charges
!    xcarpun : x-coordinate of the points associated to the punctual charges 
!    ycarpun : y-coordinate of the points associated to the punctual charges
!    ncarpun : values of the punctual charges
!    scarpun
!*---------------------------------------------------------------------------

  module cargapun

   use parametros_electros

   integer           ::  ncarpun
   double precision  ::  xcarpun(ndcar)
   double precision  ::  ycarpun(ndcar)
   double precision  ::  carpun(ndcar)
 
   contains

    subroutine scarpun(xcarpuni,ycarpuni,itria,bar1,bar2,bar3)
     use malla_2DP1
     implicit double precision (a-h,o-z) 
     dimension xp(2), bxp(3)

     xp(1)=xcarpuni
     xp(2)=ycarpuni

     call baric(xp,itria,bxp,nel,mm,z)
     
     bar1=bxp(1)
     bar2=bxp(2)
     bar3=bxp(3)

    end subroutine scarpun

  end module cargapun

!*----------------------------------------------------------------------
!     MODULE PERMITIVIDAD
!-----------------------------------------------------------------------
! Permitivity
!-----------------------------------------------------------------------
! Used in:  matel.f 
!-----------------------------------------------------------------------
!    permir
!*----------------------------------------------------------------------

  module permitividad

   use electros_2D, only: iopej
   use derivados
 
   contains

    double precision function permir(x,y,nsd,nd)
     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::nsd,nd

     permir= 1.d0
     
     iopermir=0
     do i=1,permirel%numero
       if(nsd.eq.permirel%referencias(i)) then 
         iopermir=permirel%iopermir(i)
         ndom=i
       endif
     enddo
     if(iopermir.eq.0) stop 'Dominio sin asignar permitividad'

     opcion_lectura_permitividad: if (iopermir.eq.1) then
!       if(iopej.eq.4) then
!         permi0= 8.854e-12
!         if(nd.eq.1) then
!           permir = dsqrt(x**2+y**2)/permi0
!         elseif(nd.eq.2) then
!           permir = dsqrt(x**2+y**2)/permi0
!         endif
!       elseif(iopej.eq.1.or.iopej.eq.2.or.iopej.eq.3&
!         .or.iopej.eq.5) then
!         permir= 1.d0
!       endif
!!!!!!!!!!!Edit here
           if (trim(permirel%etiqueta(ndom)).eq.'Material name') then
               permir = 1.d0
           else
               print*, 'ERROR: Relative permittivity: '//trim(permirel%etiqueta(ndom))//', Function descriptor not found'
               stop 1
           endif
!!!!!!!!!!!Edit here
     elseif (iopermir.eq.2) then
       if(nd.eq.1) then 
         permir = permirel%valorx(ndom)
       elseif(nd.eq.2) then
         permir = permirel%valory(ndom)
       endif
     endif opcion_lectura_permitividad

    end function permir


    double precision function permi(x,y,nsd,nd)
     implicit double precision (a-h,o-z)
     double precision, intent(in) :: x, y
     integer, intent(in)::nsd,nd
     permi0= 8.854e-12
     permi = permi0 * permir(x,y,nsd,nd) 
    end function permi

  end module permitividad

!*----------------------------------------------------------------------
!     MODULE BLOQUEO
!-----------------------------------------------------------------------
!    h : function for the strong imposition of Dirichlet conditions 
!*----------------------------------------------------------------------

  module bloqueo

  use electros_2D,  only: DirBC_Func

  contains

   double precision function  h(x,y,irefd)
   implicit double precision (a-h,o-z)
   double precision, intent(in) :: x, y
   integer, intent(in)::irefd

   h=0.d0
!       Dirichlet Boundary Condition defined as a function by User
!       Please modify the code below in case you want to provide your own
!       function to compute Dirichlet boundary condition
   if (trim(adjustl(DirBC_Func)) == 'User_Defined') then
     h = 0.d0
   endif

!    Dirichlet Boundary Condition function example 1 (point source) 
!    Potential generated by a charge at origin.
   if (trim(adjustl(DirBC_Func)) == 'Example_1') then
     ql= 1.e-10
     pi=dacos(-1.d0)
     permi0= 8.854e-12
     h = ql/(2*pi*permi0)*dlog(1.d0/dsqrt(x**2+y**2))
   endif

!    Dirchlet Boundary Condition  function example 2 	(line sources)
!    Potential generated by a uniform distribution of charges at a circumference of radius a.
   if (trim(adjustl(DirBC_Func)) == 'Example_2') then
     ql    = 1.e-10
     epsi0 = 8.854e-12
     a=0.5d0
     if (dsqrt(x**2+y**2).ge.a) then
       h = ql*a/epsi0 * dlog(1.d0/dsqrt(x**2+y**2))
     else 
       h = ql*a/epsi0 * dlog(1.d0/a)
     endif
   endif
  
!    Exact function example 3 	(surface sources)
!    Potential generated by a uniform distribution of charges in a circle of radius rint
   if (trim(adjustl(DirBC_Func)) == 'Example_3') then
     h=0.d0
   endif

   end function h

  end module bloqueo

!*----------------------------------------------------------------------
!     MODULE NEUMANN
!-----------------------------------------------------------------------
! Neumann boundary condition  
!*----------------------------------------------------------------------

  module neumann

  use derivados
  use electros_2D, only: NeuBC_Func

  contains

   double precision function  g(x,y,irefn)
   implicit double precision (a-h,o-z)
   double precision, intent(in) :: x, y
   integer, intent(in)::irefn

   g=0.d0

!    Neumann Boundary Condition defined as a function by User
!    Please modify the code below in case you want to provide your own
!    function to compute Neumann boundary condition
   if (trim(adjustl(NeuBC_Func)) == 'User_Defined') then
     g = 0.
   endif 

!    Neumann Boundary Condition function example 1 (point source) 
!    Potential generated by a charge at origin.
   if (trim(adjustl(NeuBC_Func)) == 'Example_1') then
     g =-7.957747154594768e-012 
   endif
  
!    Neumann Boundary Condition  function example 2 	(line sources)
!    Potential generated by a uniform distribution of charges at a circumference of radius a.
   if (trim(adjustl(NeuBC_Func)) == 'Example_2') then
     ql    = 1.e-10
     epsi0 = 8.854e-12
     a=0.5d0
     g= - ql*a/dsqrt(x**2+y**2)
   endif

   end function g

  end module neumann
