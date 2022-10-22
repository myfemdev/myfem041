subroutine blomamorp(xmat,nrefb,irefb,tablo,sm)

  use bloqueo
  use malla_3DP1
  use electros3D
     
  implicit none 
  integer,          intent(in)    :: irefb(*)
  double precision, intent(in)    :: tablo(*)
  integer,          intent(in)    :: nrefb
  double precision, intent(inout) :: sm(*)
  double precision, intent(out)   :: xmat(*)
  integer                         :: iv,icol,nr
  integer                         :: i,j,l,i1

  do i=1,nrefb
     iv=irefb(i)
     do l=ib(iv)+1,ib(iv+1)-1
        xmat(l)=0d0
     enddo        
     xmat(ib(iv+1))=1d0
  enddo       

  do i=1,nver
     bucle1: do i1=ib(i)+1,ib(i+1)-1
        icol=jb(i1)
        do j=1,nrefb
           if (icol.eq.irefb(j)) then
              sm(i)=sm(i)-xmat(i1)*tablo(j)
              xmat(i1)=0d0
              cycle bucle1
           endif
        enddo     
     enddo bucle1
  enddo       

  return
end
