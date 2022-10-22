!-----------------------------------------------------------------------
!     MODULE COMPILER_DEPENDANT
!-----------------------------------------------------------------------  
      module cp
       integer, parameter :: DOUBLE = selected_real_kind(15, 307)
      end module cp

! -----------------------------------------------------------------------
!     MODULE PARAMETROS_ELECTROS3D
!-----------------------------------------------------------------------                                                                    
!     ndrd    : maximum number of Dirichlet boundaries 
!     ndrn    : maximum number of Neumann boundaries
!     ndf     : maximum number of function descriptors 
!     funlen  : maximum lenght for a function name    
!     ndcar   : maximum number of charges  
!     ndar    : maximum number of edges  
!     ndcaras : maximum number of faces
!     ndver   : maximum number of vertices
!     ndref   : maximum number of references                          
! -----------------------------------------------------------------------

      module parametros_electros3D

       integer, parameter :: ndrd    =      50 
       integer, parameter :: ndrn    =      50 
       integer, parameter :: ndf     =     100 
       integer, parameter :: funlen  =     120 
       integer, parameter :: ndcar   =     100
       integer, parameter :: ndar    =   30000
       integer, parameter :: ndcaras =   30000
       integer, parameter :: ndver   =   120000
       integer, parameter :: ndref   =   200
       integer  :: ierror

      end module parametros_electros3D
      
!-----------------------------------------------------------------------
!     MODULE DERIVADOS
!-----------------------------------------------------------------------                                             
!    blofron  : strong imposition of Dirichlet conditions by constants
!               (referencias==boundary numbers)
!    blopun   : strong imposition of Dirichlet conditions by points
!               (referencias==point numbers)
!    neuman   : strong imposition of Neumann conditions by constants
!               (referencias==boundary numbers)
!               (valor==normal derivative value)
!----------------------------------------------------------------------- 

      module derivados3D
      
  	   use cp
       use parametros_electros3D

       type interfaz
        integer                         :: numero
        integer,      dimension(100)    :: referencias
        real(DOUBLE), dimension(100)    :: valor
       end type interfaz

       type interfaz2
        integer                         :: numero
        integer,      dimension(100)    :: referencias
        real(DOUBLE), dimension(100)    :: valor
        logical,      dimension(100)    :: constante ! true => constant ; false => function (uses{vol,sup,cur}%fun)
       end type interfaz2

       type  interfazxy
        integer                          :: numero
        integer,      dimension(100)     :: referencias
        integer,      dimension(100)     :: iopermir
        integer,      dimension(100)     :: fun ! index of function: 1=user_defined,..,n
        integer,      dimension(100)     :: ntab
        real(DOUBLE), dimension(100,100) :: teta
        real(DOUBLE), dimension(100,100) :: valtabx
        real(DOUBLE), dimension(100,100) :: valtaby
        real(DOUBLE), dimension(100,100) :: valtabz
        real(DOUBLE), dimension(100)     :: valorx
        real(DOUBLE), dimension(100)     :: valory
        real(DOUBLE), dimension(100)     :: valorz
        character(len=120), dimension(100) :: etiqueta
       end type interfazxy

       type userfun !function descriptors
        integer, dimension(ndf) :: fun
! If all functions are the same, funs is it's number. Otherwise, funs=-1. It is computed in function_number  
        integer :: funs
       end type userfun

       type(interfaz)    :: blofron,blopun,neuman
       type(interfaz2)   :: carvol,carcur,carsup
       type(interfazxy)  :: permirel
       type(userfun)     :: dir,neu,vol,sup,cur
      
! function names
      character(len=funlen), dimension(7), parameter :: functions = &
        ['Function defined by user                   ', &  ! 1
         'Example 1: Volumic charged sphere          ', &  ! 2
         'Example 2: One uniformly charged sphere    ', &  ! 3
         'Example 3: Two uniformly charged spheres   ', &  ! 4
         'Example 4: One uniformly charged segment   ', &  ! 5
         'Example 5: One charged point               ', &  ! 6
         'Example 6: Two charged points              ']    ! 7
       
! function names for permitivity
      character(len=funlen), dimension(3), parameter :: functions_perm = &
        ['Function defined by user   ',  & ! 1
          'log(1+r^2)                 ', & ! 2
          'log(e+r^2)                 ']   ! 3

       contains
       
! obtains the index of the function_name in the array. 0 if not found
        integer function function_number(function_name, array)
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
! Variables associated to a 3D mesh with P1 elements
!-----------------------------------------------------------------------
!    nel   : number of elements of the mesh                          
!    nver  : number of vertices of the mesh                           
!    nra   : array with the numbers of the edges                   
!    nrv   : array with the references of the vertices                                         
!    nsd   : domains array     
!    z     : array of coordinates  
!    det   : determinant of the matrix associated to Fk
!           (Fk transforms the tetrahedron k in the reference tetrahedron)
!    binv  : inverse of the matrix associated to Fk
!    mm    : connectivity matrix 
!    nrvg  : references of the vertices in the global numbering
!    nrc   : references of the faces
!    nrv   : references of the vertices
!    nead  : array with the neighbour elements to each tetrahedron
!    itin  : tetrahedron where the vertices are initially (characteristic method)
!    nelem : number of elements for the index of carvol
!    ensd  : elements for each index of carvol                                               
! -----------------------------------------------------------------------
          
      module malla_3DP1
      
   	   use cp

       integer                      ::  nel
       integer                      ::  nemm
       integer                      ::  nver
       integer,allocatable          ::  nead(:,:)
       integer,allocatable          ::  mm(:,:)
       integer,allocatable          ::  nra(:,:)
       integer,allocatable          ::  nrv(:,:)
       integer,allocatable          ::  nrvg(:)
       integer,allocatable          ::  nrc(:,:)
       integer,allocatable          ::  nsd(:)
       integer,allocatable          ::  it(:)
       double precision,allocatable ::  z(:,:)
       double precision,allocatable ::  det (:)
       double precision,allocatable ::  binv (:,:,:)
          
       integer,allocatable :: nelem(:) 
       integer,allocatable :: ensd(:,:)
           
      end module malla_3DP1

!-----------------------------------------------------------------------
! MODULE ELECTROS3D
!-----------------------------------------------------------------------
!    iopblo     : strong imposition of Dirichlet conditions option 
!                 (1 Yes 0 No)
!    iopblo1    : input via function (1 Yes 0 No)
!    iopblo2    : input via constants in the boundaries (1 Yes 0 No)
!    iopblo3    : input via punctual blocking (1 Yes 0 No) 
!    nrd        : number of Dirichlet references, input via function
!    irefd      : array of Dirichlet references, input via function 
!    iopneu     : Neumann references option (1 Yes 0 No)
!    iopinneu1  : input via function (1 Yes 0 No) 
!    iopinneu2  : input via constants in the boundaries (1 Yes 0 No) 
!    nrn        : number of Neumann references, input via function
!    irefn      : array of Neumann references, input via function 
!    iop        : quadrature formula for matrix and second member option
!    iopf       : quadrature formula for the boundary terms 
!    iopvol     : volumic charge option (1 Yes 0 No)
!    iopsup     : superficial charge option (1 Yes 0 No)  
!    iopcur     : curvilinear charge option (1 Yes 0 No)
!    ioppun     : punctual charge option (1 Yes 0 No)
!    mua        : pointer to the profile stored matrix 
!    c          : system matrix                          
!    b          : second member vector
!    sol        : solution vector  
!    ib         : morse pointer for row storage
!    jb         : morse pointer for column storage   
!    iopsl      : option for the linear system resolution 
!    epscg      : epsilon for the test in the conjugate gradient method 
!    nitcg      : maximum number of iterations in conjugate gradient 
!    indc(1,i)  : array wich stores the 3 numbers (of 4 possibilities) 
!                 wich correspond to the vertices that are in the face i  
!    inda(1,i)  : array wich stores the 2 numbers (of 4 possibilities)
!                 wich correspond to the vertices that are in the edge i
! -----------------------------------------------------------------------

      module electros3D
	  
       use cp
       use parametros_electros3D
       
       integer         ::  nrd
       integer         ::  nrn
       integer         ::  irefd(ndrd)
       integer         ::  irefn(ndrn)
       integer		   ::  iop
       integer		   ::  iopf
       integer		   ::  iopvol
       integer		   ::  iopsup
       integer		   ::  iopcur
       integer		   ::  ioppun
       integer         ::  iopblo
       integer         ::  iopblo1
       integer         ::  iopblo2
       integer         ::  iopblo3
       integer         ::  iopneu
       integer         ::  iopneu1
       integer         ::  iopneu2
       integer		   ::  iopej
       integer		   ::  iopsl
       real(DOUBLE)    ::  epscg
       integer         ::  nitcg
	   
       integer                       :: iopteta
       double precision,allocatable  ::  teta(:)

      ! CALCULUS
       integer,     allocatable  ::  ib(:)
       integer,     allocatable  ::  jb(:)
       integer,     allocatable  ::  mua(:)
       integer,     allocatable  ::  id(:)
       real(DOUBLE),allocatable  ::  camor(:)
       real(DOUBLE),allocatable  ::  b(:)
       real(DOUBLE),allocatable  ::  sol(:)
       real(DOUBLE),allocatable  ::  xcg(:)
       real(DOUBLE),allocatable  ::  preex(:)
       real(DOUBLE),allocatable  ::  e(:,:)
       real(DOUBLE),allocatable  ::  evtu(:)
       real(DOUBLE),allocatable  ::  vexac(:)
       real(DOUBLE),allocatable  ::  err(:)
       real(DOUBLE)              ::  rel
       real(DOUBLE)              ::  xnorexac
       real(DOUBLE)              ::  xnorerr
       integer, dimension(3,4)   ::  indc
       integer, dimension(2,6)   ::  inda

      end module electros3D
      
!-----------------------------------------------------------------------
! MODULE AUXILIAR_CARGAS
!-----------------------------------------------------------------------     
      module auxiliar_cargas
         
        integer, allocatable ::  ncaras(:)
        integer, allocatable ::  nodc1(:,:), nodc2(:,:), nodc3(:,:)
        integer, allocatable ::  naristas(:)
        integer, allocatable ::  nod1(:,:), nod2(:,:)
            
      end module auxiliar_cargas

! -----------------------------------------------------------------------
! MODULE EXTERNAL_ELECTROS
!-----------------------------------------------------------------------
! External functions
!-----------------------------------------------------------------------

      module external_electros3D
      use cp

       real(DOUBLE), external :: fexac
 
      end module external_electros3D
     
! -----------------------------------------------------------------------
! MODULE FICH_ELECTROS3D
!-----------------------------------------------------------------------
! Ficheros
!-----------------------------------------------------------------------
!    fichma      : mesh file 
!    fichsol     : solution file
!    fichgradsol : solution gradient file 
!    fichvexac   : exact solution file
!    fichteta    : temperature file
! -----------------------------------------------------------------------

      module fich_electros3D

       character (len=245) fichma
       character (len=245) fichsol
       character (len=245) fichgradsol
       character (len=245) fichvexac
       character (len=245) fichteta

      end module fich_electros3D

! -----------------------------------------------------------------------
! MODULE CARGAVOL
!-----------------------------------------------------------------------
! Options for the volumic charges
!-----------------------------------------------------------------------

      module cargavol
      
       use cp
       use derivados3D
 
       contains

      	real(DOUBLE) function fc(nsd,indice)
 
      	implicit none
      	integer, intent(in)::nsd
        integer, intent(in)::indice ! array of indices for the volumic charges

        fc = carvol%valor(indice)
        
      	end	function fc

      end module cargavol

! -----------------------------------------------------------------------
! MODULE CARGASUP
!-----------------------------------------------------------------------
! Options for the superficial charges
!-----------------------------------------------------------------------

      module cargasup
         
       use cp
       use parametros_electros3D
       use derivados3D
           
       implicit none
          
       contains

       real(DOUBLE) function gcst(iref,indice)
 ! Integral over the boundary of the superficial charge function
 ! Is constant on each reference
 
      	implicit none
      	integer, intent(in)::iref
        integer, intent(in)::indice ! array of indices for the superficial charges

        gcst = carsup%valor(indice)
		
       end function gcst

! ---------------------------------------------------------------------------------- 
!     Function for the density of superficial charge 
!     x,y,z: point coordinates
!     iref: face number reference
! ---------------------------------------------------------------------------------- 
      
       real(DOUBLE) function  gcs(x,y,z,iref,indice)

        implicit none
        real(DOUBLE), intent(in) :: x, y, z
        integer,      intent(in) :: iref
        integer,      intent(in) :: indice ! array with the indices of the charges
     
        gcs=0.d0

        if (sup%fun(indice) == 3)    then  ! fun_2 == 'Example 2'...
           if (iref.eq.5.or.iref.eq.6.or.iref.eq.7.or.   &
               iref.eq.8.or.iref.eq.11.or.iref.eq.12.or. &
               iref.eq.14.or.iref.eq.15) then
      		      gcs= 1.d-10
      	   endif
        elseif (sup%fun(indice) == 4)    then   ! fun_3 == 'Example 3'...
           if (iref.eq.9.or.iref.eq.10.or.iref.eq.11.or.  &
               iref.eq.12.or.iref.eq.17.or.iref.eq.18.or. &
               iref.eq.21.or.iref.eq.22) then
       		    gcs= 1.d-10
           endif  
      	   if (iref.eq.5.or.iref.eq.6.or.iref.eq.7.or.   &
               iref.eq.8.or.iref.eq.15.or.iref.eq.16.or. &
               iref.eq.20.or.iref.eq.23) then
      	 	    gcs= -2.5d-11
      	   endif        
        elseif (sup%fun(indice) == 1)     then ! fun_0 == 'Function defined by user'
           gcs=0.d0
        else
          stop 'Surface source: Function gcs: Function descriptor not found'
        endif

       end function gcs

      end module cargasup

! -----------------------------------------------------------------------
! MODULE CARGACUR
!-----------------------------------------------------------------------
! Options for the curvilinear charges
!-----------------------------------------------------------------------

      module cargacur
       use cp
       use parametros_electros3D
       use derivados3D
 
       contains

      	real(DOUBLE) FUNCTION gcct(iref,indice)
 ! Integral over the boundary of the curvilinear charge function
 ! Is constant on each reference

      	implicit none
      	integer, INTENT(IN)::iref
      	integer, INTENT(IN)::indice ! array with the indices of the charges

        gcct = carcur%valor(indice)

      end function gcct


! ---------------------------------------------------------------------------------- 
!     Function for the density of curvilinear charge 
!     x,y,z: point coordinates
!     iref: face number reference
! ---------------------------------------------------------------------------------- 
      
      real(DOUBLE) FUNCTION  gcc(x,y,z,iref,indice)

      implicit none
      real(DOUBLE), INTENT(IN) :: x, y, z
      integer, INTENT(IN)::iref
      integer, INTENT(IN)::indice ! array with the indices of the charges

      gcc = 0.d0

      if (cur%fun(indice) == 5)    then  ! fun_4 == 'Example 4'...
            if (iref.eq.9) then
      		    gcc = 1.d-10
      		endif
      elseif (cur%fun(indice) == 1)     then ! fun_0 == 'Function defined by user'
      		gcc=0.d0
      else
         stop 'Line source: Function gcc: Function descriptor not found'
      endif
      

      end	function gcc

     end module cargacur
      
! ----------------------------------------------------------------------
! MODULE CARGAPUN
!-----------------------------------------------------------------------
! Module associated to the punctual charges
!-----------------------------------------------------------------------

      module cargapun
 
       use cp
       use parametros_electros3D
       use malla_3DP1 , only : nel, mm, z
       
       integer		     ::  ncarpun
       double precision  ::  xcarpun(ndcar)
       double precision  ::  ycarpun(ndcar)
       double precision  ::  zcarpun(ndcar)
       double precision  ::  carpun(ndcar)
       
       contains

      subroutine scarpun(xcarpuni,ycarpuni,zcarpuni,itetr,bar1,bar2,bar3,bar4)	

      	implicit none

        real(DOUBLE), intent(in)   :: xcarpuni, ycarpuni, zcarpuni
        real(DOUBLE), dimension(3) :: xp
        real(DOUBLE), dimension(4) :: bxp
        real(DOUBLE), intent(out)  :: bar1, bar2, bar3, bar4
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
! MODULE PERMITIVIDAD
!-----------------------------------------------------------------------
! Permitivity
!-----------------------------------------------------------------------

      module permitividad
      
       use cp
       use electros3D,  only: iopej, iopteta,teta
       use derivados3D
       integer		   ::  iopermir
       integer		   ::  ndom
       CONTAINS

      	real(DOUBLE) function permir(x,y,z,nsd,nd)

      	implicit none
      	real(DOUBLE), intent(in) :: x, y, z
        integer, intent(in)::nsd,nd
        integer :: i
        real(DOUBLE) :: permi0

        permir=1.d0

      	if(nd.eq.1) then	
      	   permir = permirel%valorx(ndom)
        elseif(nd.eq.2) then
           permir = permirel%valory(ndom)
        elseif(nd.eq.3) then
           permir = permirel%valorz(ndom)
        endif

      	end	function permir
      	

      	real(DOUBLE) function permi(x,y,z,nsd,nd)

      	implicit none
      	real(DOUBLE), intent(in) :: x, y, z
        integer, intent(in)::nsd,nd
        real(DOUBLE) :: permi0     
      	permi0= 8.854d-12
      	permi = permi0 * permir(x,y,z,nsd,nd)
      	end function permi


        real(DOUBLE) function permirfun(x,y,z,nsd,nd)
      	implicit none
      	real(DOUBLE), intent(in) :: x, y, z
        integer, intent(in) :: nsd, nd
        
        permirfun = 1.0
        
!        if (permirel%fun(ndom) == 3) then !  log(1+r^2)
!            permirfun = dlog(1.0+x*x+y*y+z*z)
!        elseif (permirel%fun(ndom) == 2) then !  log(e+r^2)
!            permirfun = dlog(2.71828+x*x+y*y+z*z)
!        elseif (permirel%fun(ndom) == 1) then !  Function defined by user
!            ! edit here
!            permirfun = 1.0
!            ! edit here
!        else
!           stop 'Relative permittivity: Function permirfun: Function descript or not found'
!        endif
!!!!!!!!!!!Edit here
           if (trim(permirel%etiqueta(ndom)).eq.'Material name') then
               permirfun = 1.d0
           else
               print*, 'ERROR: Relative permittivity: '//trim(permirel%etiqueta(ndom))//', Function descriptor not found'
               stop 1
           endif
!!!!!!!!!!!Edit here

        
        end function permirfun



      	real(DOUBLE) function permifun(x,y,z,nsd,nd)
      	implicit none
      	real(DOUBLE), intent(in) :: x, y, z
        integer, intent(in)::nsd,nd
        real(DOUBLE) :: permi0     
      	permi0 = 8.854d-12
      	permifun = permi0 * permirfun(x,y,z,nsd,nd)
      	end function permifun
  
      end module permitividad

! ----------------------------------------------------------------------------------
! MODULE bloqueo
!-----------------------------------------------------------------------------------
!  nvrebc  : number of vertices  for Dirichlet BC
!  iverbc  : numbers of the vertices in nvrebc
!  nvrebf  : number of vertices for Dirichlet BC given by a function
!  iverbf  : numbers of the vertices in nvrebf
!-----------------------------------------------------------------------------------

      module bloqueo

      use cp
      use parametros_electros3D
      use derivados3D , only: dir
      use electros3D, only: nrd !, lirefd => irefd
      
      ! Constant
      integer	::  nvrebc(ndref)       
      integer	::  ivrebc(ndref,ndver)
      ! Function
      integer	::  nvrebf(ndref)       
      integer	::  ivrebf(ndref,ndver)  

       contains
!     Strong imposition of the Dirichlet conditions
      real(DOUBLE) function  h(x,y,z,irefd,indice)

      implicit double precision (a-h,o-z)
      real(DOUBLE), intent(in) :: x, y, z
      integer, intent(in)      :: irefd
      real(DOUBLE)             :: permi0
      real(DOUBLE)             :: pi
      real(DOUBLE)             :: q
      real(DOUBLE)             :: a
      real(DOUBLE)             :: b
      real(DOUBLE)             :: qtotal
      integer, intent(in)      ::indice
      
      h=0.d0
      permi0= 8.854d-12
      pi=dacos(-1.d0)
      q=1.d-10
      qtotal=q*4.*pi
      
      if (dir%fun(indice) == 2)    then  ! 'Example 1'...
      !'Example 1: Volumic charged sphere'
           
        h=qtotal/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2))
             
      elseif (dir%fun(indice) == 3)    then  ! 'Example 2'...
      ! 'Example 2: One uniformly charged sphere (Test_cs1.dat)'
      
         h=qtotal/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2))
                
      elseif (dir%fun(indice) == 4)    then ! 'Example 3'...
      ! 'Example 3: Two uniformly charged sphere (Test_cs2.dat)'
      
         h=qtotal/(4*pi*permi0*2.)
              
      elseif (dir%fun(indice) == 5)    then ! 'Example 4'...
      ! 'Example 4: One uniformly charged segment (Test_cc1.dat)'
      
            a=sqrt(x**2+y**2+(1-z)**2)+(1-z)
            b=sqrt(x**2+y**2+(-1-z)**2)+(-1-z)
            h=q/(4*pi*permi0)*dlog(a/b)  
                 
      elseif (dir%fun(indice) == 6)    then  !  'Example 5'...
      ! 'Example 5: One charged point (Test_cp1.dat)'
      
            h=q/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2))
              
      elseif (dir%fun(indice) == 7)    then !  'Example 6'...
      !  'Example 6: Two charged point (Test_cp2p.dat)'
       
      h=q/(4*pi*permi0)*(1./sqrt((2-x)**2+y**2+z**2)-1./sqrt((2+x)**2+y*&
     &*2+z**2))   
            
      elseif (dir%fun(indice)  == 1) then ! 'Function defined by user'
      !'User defined: Function defined by user'
         h = 0.d0

      else
        stop 'Dirichlet B.C.: Function h: Function descriptor not found'
      endif
      
    
      end function h

      end module bloqueo


! -----------------------------------------------------------------------
! MODULE neumann
!-----------------------------------------------------------------------
! Neumann boundary conditions
!-----------------------------------------------------------------------

      module neumann
      use cp
      use derivados3D , only: neu

      contains


!     Function G: Neumann condition 
      real(DOUBLE) function  g(x,y,z,irefn,indice)

      implicit none
      real(DOUBLE), intent(in) :: x, y, z
      integer, intent(in) :: irefn
      integer, intent(in) :: indice
      real(DOUBLE)        :: pi
      real(DOUBLE)        :: q
      real(DOUBLE)        :: a
      real(DOUBLE)        :: b
      real(DOUBLE)        :: qtotal

        pi=dacos(-1.d0)
        q=1.d-10
        qtotal=q*4.*pi
        g=0.d0

      if (neu%fun(indice) == 3) then ! 'Example 2'
      ! 'Example 2: One uniformly charged sphere (Test_cs1.dat)'
            g = - qtotal/(4.*pi)*(1./(x**2+y**2+z**2))  
      elseif (neu%fun(indice)  == 1) then ! 'Function defined by user'
      !'User defined: Function defined by user'
            g = 0.d0
      else
         stop 'Neumann B.C.: Function g: Function descriptor not found'
      endif
            
      end function g

      end module neumann
      
! ---------------------------------------------------------------------------------- 
     
       module resolucion_sistema_lineal
             
       use cp
       
       real(DOUBLE),allocatable  ::  camorf(:)
       real(DOUBLE),allocatable  ::  work(:)
    
      end module resolucion_sistema_lineal
