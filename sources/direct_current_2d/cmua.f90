subroutine cmua(mua,mm,nel,nver)

  implicit none
  integer, intent(in)  :: mm(3,1)
  integer, intent(in)  :: nel,nver
  integer, intent(out) :: mua(1)
  integer              :: mi
  integer              :: i,j,k

  do i=2,nver+1
     mua(i)=i-1
  enddo
  do k=1,nel
     mi=min(mm(1,k),mm(2,k),mm(3,k))
     do j=1,3
        mua(mm(j,k)+1)=min(mua(mm(j,k)+1),mi)
     enddo
  enddo
  mua(1)=0
  do i=3,nver+1
     mua(i)=mua(i-1)+i-mua(i)
  enddo

  return
end
