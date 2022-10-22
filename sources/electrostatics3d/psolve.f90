subroutine psolve(x,b,xmat,n,mua1,mua2)

  implicit none
  double precision, intent(in)    :: xmat(*),b(*) 
  integer,          intent(in)    :: mua1(*),mua2(*)
  integer,          intent(in)    :: n
  double precision, intent(inout) :: x(*)

  call reslud(1,n,mua1,mua2,xmat,b,x)
  call reslud(2,n,mua1,mua2,xmat,x,x)

  return
end
