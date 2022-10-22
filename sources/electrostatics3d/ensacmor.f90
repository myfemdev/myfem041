!------------------------------------------------------------------
!                      ASSEMBLY OF THE MATRIX      
!------------------------------------------------------------------
! GOAL: assembly of the matrix C with sky-line storage
!       (pointers ib and jb) 
!------------------------------------------------------------------
! IN:    cmat --> matrix 
!        mm ----> connectivity matrix
!        jb ----> column pointer
!        ib ----> row pointer
!
! OUT:   xmor --> morse assembly
!------------------------------------------------------------------

subroutine ensacmor(cmat,ib,jb,mm,xmor)

  implicit none
  double precision, intent(in)  :: cmat(4,*)
  integer,          intent(in)  :: mm(*),ib(*),jb(*) 
  double precision, intent(out) :: xmor(*)
  integer                       :: n1,n2
  integer                       :: i,j,l

  do i=1,4
     n1=mm(i)
     do j=1,4
        n2=mm(j)
        do l=ib(n1)+1,ib(n1+1)
           if (n2.eq.jb(l)) then
              xmor(l)=xmor(l)+cmat(i,j)
           endif
        enddo
     enddo       
  enddo       

  return
end
