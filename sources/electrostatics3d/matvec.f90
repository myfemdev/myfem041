!---------------------------------------------------------------------------
! Computation of the product:
!                          y=beta*y+alpha*xmat*x
!---------------------------------------------------------------------------

subroutine matvec(alpha,x,beta,y,xmat,n,mua1,mua2)

  implicit none 
  double precision, intent(in)    :: alpha,beta
  double precision, intent(in)    :: x(*),xmat(*)
  double precision, intent(inout) :: y(*)
  integer,          intent(in)    :: n
  integer,          intent(in)    :: mua1(*),mua2(*)
  integer                         :: i

  if (beta.ne.1.d0) then
     if (beta.ne.0.d0) then
        do i=1,n
           y(i)=beta*y(i)
        enddo  
     else
        do i=1,n
           y(i)=0.d0
        enddo
     endif
  endif

  if (alpha.ne.0.d0) then
     if (alpha.eq.1.d0) then
        call procb1(n,xmat,mua1,mua2,x,y)
     else   
        call procb2(n,xmat,mua1,mua2,alpha,x,y)
     endif
  endif

  return
end
