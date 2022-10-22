!------------------------------------------------------------------
! IN:      detk -----> determinant of the matrix Bk ( =6*vol(k) )
!          cbar -----> array with the partial derivatives of the 
!                      shape functions
!          binv -----> inverse of the matrix Bk
!          coefm ----> mass coeficient
!          coefk(i) ----> coefficient i of the diagonal in the matrix 
!
! OUT:     amat -----> elemental matrix (mass)
!          cmat -----> elemental matrix, follows from integrating a 
!                      term of the form u*v
!          a2mat ----> elemental matrix, follows from integrating a 
!                      term of the form grad(u)*grad(v)
!------------------------------------------------------------------

subroutine matlap_orto(amat,detk,binv,coefm,coefk,cmat)

  implicit none
  double precision, intent(in)  :: coefk(3),binv(3,*)
  double precision, intent(in)  :: detk,coefm
  double precision, intent(out) :: amat(4,*),cmat(4,*)
  double precision              :: cbar(4,3),a2mat(4,4)
  integer                       :: i,j,ii

  call derff(binv,detk,cbar)
  do i=1,4
     do j=1,4
        cmat(i,j)=0d0
        a2mat(i,j)=0d0
        do ii=1,3
           a2mat(i,j)=a2mat(i,j)+cbar(i,ii)*cbar(j,ii)*coefk(ii)
        enddo     
        a2mat(i,j)=a2mat(i,j)/detk/6d0
        if (i.eq.j) then
           cmat(i,j)=detk/24d0
        endif
        amat(i,j)=coefm*cmat(i,j)+a2mat(i,j)
     enddo     
  enddo       

  return
end
