!---------------------------------------------------------------------------
!     Volumic charge density function (second member)
!---------------------------------------------------------------------------
 
double precision function f(x,y,nsd)

  use dcurrent_2D,  only: DirBC_Func

  implicit none
  integer, intent(in)          :: nsd
  double precision, intent(in) :: x,y

  f=0.d0
      
  if (trim(adjustl(DirBC_Func)) == 'Example_3') then
     if (nsd.eq.1) then
        f=-12.
     endif    
  endif

  return
end

!---------------------------------------------------------------------------
!     Exact solution for the examples
!---------------------------------------------------------------------------
 
double precision function fexac(x,y)

  use dcurrent_2D,  only: DirBC_Func

  implicit none
  double precision, intent(in) :: x,y

  if (trim(adjustl(DirBC_Func)) == 'Example_3') then
     fexac=3*x**2+10  
  elseif (trim(adjustl(DirBC_Func)) == 'Example_1') then 
     fexac=3.*x**2-y**2+10.  
  elseif (trim(adjustl(DirBC_Func)) == 'Example_2') then 
     fexac=dlog(6.*x+4.*y+2.)
  endif
  
  return
end












