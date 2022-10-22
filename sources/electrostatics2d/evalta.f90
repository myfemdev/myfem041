!------------------------------------------------------------------------
!    fuction that evaluates the array (xt,yt) for the material im and
!	the kind of data id in 'x' 
!
!     materials(10): 1: graphite, 2: paste, 3: nipple, 4: ferrule, 
!                    5: plates, 6: pasta liquida, 7: briquettes
!     data(15): 1: termal conductivity, 2:electrical resistivity,
!               3: specific heat, 4: mass density, 
!               5: er, 6: ez, 7:nur, 8: nuz, 9: gz, 10: alfr, 11: alfz
!               12: enthalpy 
!------------------------------------------------------------------------
      
double precision function evalta(nn,xt,yt,x)

  integer          :: nn
  double precision :: xt(*),yt(*)
  double precision :: x
  double precision :: y
  integer          :: ic

  if (nn .lt. 2) stop 'The array is not valid'
! EXTRAPOLATION TO THE LEFT
  if (x .le. xt(1)) then
     call punto1(xt(1),yt(1),xt(2),yt(2),x,y)
     evalta = y
     return
  endif
! INTERPOLATION
  do ic=2,nn
     if (x .le. xt(ic)) then
       call punto1(xt(ic-1),yt(ic-1),xt(ic),yt(ic),x,y)
       evalta = y
       return
    endif
  enddo 
! EXTRAPOLATION TO THE RIGHT
  call punto1(xt(nn-1),yt(nn-1),xt(nn),yt(nn),x,y)
  evalta = y

  return
end
