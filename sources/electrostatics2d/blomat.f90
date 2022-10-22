subroutine blomat(mua,ca,mm,nra,nel,iref)

  implicit none
  integer,          intent(in)  :: mua(*),mm(3,*),nra(3,*)
  integer,          intent(in)  :: nel,iref
  double precision, intent(out) :: ca(*)
  integer                       :: k,j

  do k=1,nel
     do j=1,3
        if(nra(j,k).eq.iref) then
           ca(mua(mm(j,k)+1))          = 1.d50
           ca(mua(mm(mod(j,3)+1,k)+1)) = 1.d50
        end if
     enddo
  enddo

  return
end
