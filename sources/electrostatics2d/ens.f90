!------------------------------------------------------------------
!                      ASSEMBLY OF THE MATRIX      
!------------------------------------------------------------------

subroutine ens(mua,e,n,c)

  implicit none
  double precision, intent(in)    :: e(3,3)
  integer,          intent(in)    :: mua(*),n(3)
  double precision, intent(inout) :: c(*)
  integer                         :: i,j,l

  do i=1,3
     do j=1,3
        if(n(j).gt.n(i)) cycle 
        l=mua(n(i)+1)-n(i)+n(j)
        c(l)=c(l)+e(i,j)
     enddo
  enddo

  return
end
