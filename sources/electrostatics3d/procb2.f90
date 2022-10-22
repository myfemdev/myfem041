!---------------------------------------------------------------------------
! GOAL:      p:=p+alpha*A*q
!---------------------------------------------------------------------------

subroutine procb2(m,bmat,ib,jb,alpha,q,p)

  implicit none
  integer,          intent(in)    :: m
  double precision, intent(in)    :: alpha
  double precision, intent(in)    :: bmat(*),q(*)
  integer,          intent(in)    :: ib(*),jb(*)
  double precision, intent(inout) :: p(*) 
  double precision                :: suma
  integer                         :: i,j

  do i=1,m
     suma=0.d0
     do j=ib(i)+1,ib(i+1)
        suma=suma+bmat(j)*q(jb(j))
     enddo  
     p(i)=p(i)+alpha*suma
  enddo       

  return
end
