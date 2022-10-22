subroutine blomamor(xmat,nrefb,irefb,sm)

  use bloqueo
  use malla_3DP1
  use electros3D
     
  implicit none
  integer,          intent(in)    :: irefb(*)
  integer,          intent(in)    :: nrefb
  double precision, intent(inout) :: sm(*)
  double precision, intent(out)   :: xmat(*)
  integer                         :: iv,icol,nr
  integer                         :: i,j,l,i1

  do i=1,nrefb
     do j=1,nvrebf(irefb(i))
        iv=ivrebf(irefb(i),j)
        do l=ib(iv)+1,ib(iv+1)-1
           xmat(l)=0d0
        enddo     
        xmat(ib(iv+1))=1d0
     enddo     
  enddo

  do i=1,nver
     bucle1: do i1=ib(i)+1,ib(i+1)-1
        icol=jb(i1)
        nr=nrvg(icol)
        do j=1,nrefb
           if (nr.eq.irefb(j)) then
              sm(i)=sm(i)-xmat(i1)*h(z(1,icol),z(2,icol),z(3,icol),nr,j)
              xmat(i1)=0d0
              cycle bucle1
           endif
        enddo     
     enddo bucle1
  enddo       

  return
end
