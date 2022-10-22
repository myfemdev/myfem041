!-----------------------------------------------------------------------------
! This subroutine calculates an array nrinterior which size is equal to the
! number of curvilinear charges. Each charge has a reference associated.
! nrinterior(i) indicates whether the charge i is in the interior of the
! domain (in this case nrinterior(i)=2) or in the boundary (in this case
! nrinterior(i)=1).
! output variable : nrinterior in the cargacur module
!-----------------------------------------------------------------------------

subroutine arint ()

  use  malla_2DP1,          only : nel, mm, nra
  use  cargacur,            only : carcur, nrinterior
  use  parametros_electros, only : ndar

  implicit none
  integer :: naristas(carcur%numero), nod1(ndar), nod2(ndar)
  integer :: nref,nov1,nov2
  integer :: i,j,k,ii

  if (carcur%numero.gt.0) then
     nod1(1)=0
     nod2(1)=0
     donumcar: do i= 1,carcur%numero
        nrinterior(i) = 1
        naristas(i)   = 1
        do k=1,nel;do j=1,3
           nref=nra(j,k)
           nov1=mm(j,k)
           nov2=mm(mod(j,3)+1,k)
           if (nref.eq.carcur%referencias(i)) then
              do ii=1, naristas(i)
                 if ((nod1(ii).eq.nov1.and.nod2(ii).eq.nov2)&
                 .or.(nod1(ii).eq.nov2.and.nod2(ii).eq.nov1)) then
! IN THIS CASE THE EDGE IS ALREDY COUNTED
                    nrinterior(i)=2
                    cycle donumcar
                 endif
              enddo 
              naristas(i) = naristas(i) + 1
              nod1(naristas(i)) = nov1
              nod2(naristas(i)) = nov2
           endif
        enddo;enddo
     enddo donumcar
  endif

end
