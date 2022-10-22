!-----------------------------------------------------------------------
!   PUNTO1 SUBROUTINE                                                
!   Computation, by interpolation (or extrapolation), of the value "y" 
!   when (x,y) is in the line through (x1,y1) and (x2,y2) 
!-----------------------------------------------------------------------

subroutine punto1(x1,y1,x2,y2,x,y)
      
  implicit none
  double precision, intent(in)  :: x1,y1,x2,y2,x
  double precision, intent(out) :: y 

  if (dabs(x2-x1) .le. 1.d-7) then 
     y = (y1+y2)*0.5d0
  else
     y = (y2-y1)*(x-x1)/(x2-x1)+y1
  end if

  return
end
