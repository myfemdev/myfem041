subroutine blosegc(b,mm,nra,nel,iref,valor)
  
  implicit none
  integer,          intent(in)  :: mm(3,*),nra(3,*)
  double precision, intent(in)  :: valor
  integer,          intent(in)  :: nel,iref
  double precision, intent(out) :: b(*)
  integer                       :: k,j

  do k=1,nel
     do j=1,3
        if (nra(j,k).eq.iref) then
           b(mm(j,k))         =1.d50 * valor
           b(mm(mod(j,3)+1,k))=1.d50 * valor
        end if
     enddo
  enddo

  return
end
