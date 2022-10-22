!------------------------------------------------------------------------
!      Density of volumic charge function (second member) 
!  x,y,z : point coordinates
!  nsd   : number of the subdomain of the element
!------------------------------------------------------------------------
 
double precision function f(x,y,z,nsd,indice)

  use derivados3D

  implicit none 
  integer :: indice,nsd
  double precision :: x,y,z 

  f=0.d0

  if (vol%fun(indice) == 2) then ! 'Example 1'...
     f=3.d-10
  elseif (vol%fun(indice) == 1) then ! 'Function defined by user'
     f=0.d0
  else
     stop 'Volumic source: Function f: Function descriptor not found'
  endif

  return
end

!------------------------------------------------------------------------
!      Exact solution function of the examples
!------------------------------------------------------------------------
 
double precision function fexac(x,y,z)

  use derivados3D

  implicit none
  double precision            :: x,y,z
  double precision, parameter :: permi0 = 8.854d-12, pi =dacos(-1d0)
  double precision, parameter :: q =1.d-10, qtotal = q*4.*pi
  double precision            :: a,b

  fexac = 0.d0
  
! 'Example 1: Volumic charged sphere'    
  if ((dir%funs == 2).or.(vol%funs == 2)) then
     if (sqrt(x**2+y**2+z**2).gt.1) then
        fexac=qtotal/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2)) 
     else
        fexac=3.*qtotal/(2*4*pi*permi0)-3.d-10*(x**2+y**2+z**2)/(6.*permi0)
     endif
! 'Example 2: One uniformly charged sphere (Test_cs1.dat)'
  elseif ((dir%funs == 3).or.(sup%funs == 3)) then
     if (sqrt(x**2+y**2+z**2).gt.1) then
        fexac=qtotal/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2)) 
     else
        fexac=qtotal/(4*pi*permi0)
     endif 
! 'Example 3: Two uniformly charged sphere (Test_cs2.dat)'               
  elseif ((dir%funs == 4).or.(sup%funs == 4)) then
     if (sqrt(x**2+y**2+z**2).le.1) then
        fexac=qtotal/(4*pi*permi0)
     else if (sqrt(x**2+y**2+z**2).le.2) then
        fexac=qtotal/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2)) 
     else
        fexac=qtotal/(4*pi*permi0*2.)
     endif 
! 'Example 4: One uniformly charged segment (Test_cc1.dat)'
  elseif ( (dir%funs == 5).or.(cur%funs == 5)) then
     a=sqrt(x**2+y**2+(1-z)**2)+(1-z)
     b=sqrt(x**2+y**2+(-1-z)**2)+(-1-z)
     if(abs(a) <= epsilon(a) .or. abs(b) <= epsilon(b)) then
       fexac=4.
     else
       fexac=q/(4*pi*permi0)*dlog(a/b)  
     endif  
! 'Example 5: One charged point (Test_cp1.dat)'              
  elseif ( dir%funs == 6) then 
     if(sqrt(x**2+y**2+z**2).gt.0) then
        fexac=q/(4*pi*permi0)*(1./sqrt(x**2+y**2+z**2))
     endif  
! 'Example 6: Two charged point (Test_cp2p.dat)'            
  elseif (dir%funs == 7) then
     if (sqrt((2.-x)**2+y**2+z**2).gt.0.and.&
        sqrt((2.+x)**2+y**2+z**2).gt.0) then
        fexac=q/(4*pi*permi0)*(1./sqrt((2.-x)**2+y**2+z**2)&
              -1./sqrt((2.+x)**2+y**2+z**2))
     endif 
! 'User defined: Function defined by user'
  else  
     fexac = 0.d0     
  endif
 
  return
end function fexac   
        
!------------------------------------------------------------------------
!    fuction that evaluates the array (xt,yt) that has nn elements 
!	                    in the point x 
!------------------------------------------------------------------------
      
double precision function evalta(nn,xt,yt,x)

  implicit none 
  integer          :: nn
  double precision :: x,y
  double precision :: xt(*),yt(*)
  integer          :: ic  

  if (nn.lt.2) stop 'The array is not valid'
! EXTRAPOLATION TO THE LEFT
  if (x.le.xt(1)) then
     call punto1(xt(1),yt(1),xt(2),yt(2),x,y)
     evalta = y
     return
  endif
! INTERPOLATION
  do ic=2,nn
     if (x.le.xt(ic)) then
        call punto1(xt(ic-1),yt(ic-1),xt(ic),yt(ic),x,y)
        evalta = y
        return
    endif
  end do
! EXTRAPOLATION TO THE RIGHT
  call punto1(xt(nn-1),yt(nn-1),xt(nn),yt(nn),x,y)
  evalta = y
  
  return
end function evalta
