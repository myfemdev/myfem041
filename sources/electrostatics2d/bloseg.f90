subroutine bloseg(b,mm,z,nra,nel,iref)

  use bloqueo
  implicit none
  integer,          intent(in)  :: mm(3,*),nra(3,*)
  integer,          intent(in)  :: nel,iref
  double precision, intent(in)  :: z(2,*)
  double precision, intent(out) :: b(*)
  integer                       :: k,j,jj

  do k=1,nel
     do j=1,3
        if (nra(j,k).eq.iref) then
           b(mm(j,k))=1.d50 * h(z(1,mm(j,k)),z(2,mm(j,k)),iref)
           jj = 1+mod(j,3)
           b(mm(jj,k))=1.d50 * h(z(1,mm(jj,k)),z(2,mm(jj,k)),iref)
        end if
     enddo
  enddo

  return
end
