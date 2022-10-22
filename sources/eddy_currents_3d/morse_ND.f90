module morse_nd_mod

use sparse_class

implicit none

contains

! Nedelec - dielectric
subroutine morse_nd(A,nn,nnod,neld,dofdiel)

implicit none
integer, parameter :: lnn = 6 ! numero de nodos (Nedelec) por elemento
integer, intent(IN)     :: nnod ! nnod da malla total (aristas)
integer, intent(IN)     :: neld ! nel da malla do dielectrico
integer, intent(IN)     :: nn(:,:) ! nn da malla total ! lnn*nel
integer, dimension(:), intent(IN) :: dofdiel ! dofdiel(elemento do diel) = elemento da malla total
type(sparse_real), intent(INOUT) :: A

INTEGER,ALLOCATABLE    :: vecino(:,:)
INTEGER                :: i,j,k,jj,temp

 call sparse_free(A)

 call sparse_build1(A, nnod, 3*neld) ! sets size(rows x cols) and allocates colptr
!A%colptr = 0
!vecino  = 0

allocate (vecino(neld,lnn))

do k = 1, neld !nel do dielectrico
   vecino(k,:) = nn(:, dofdiel(k)) !forall (i=1:6) vecino(k,i) = nn(i, ddk)
enddo

!ordenacion dos nodos vecinhos, de menor a maior
do i = 1, neld
  do j = 1, lnn - 1
    do jj = j + 1, lnn
      if (vecino(i,j) > vecino(i,jj)) THEN
         temp         = vecino(i,jj)
         vecino(i,jj) = vecino(i,j)
         vecino(i,j)  = temp
      endif
    enddo
  enddo
enddo

! creacion do punteiro colptr
forall (k = 1:3*neld+1) A%colptr(k) = lnn*(k-1) + 1
!A%colptr = [(3*(k-1)+1, k =  1, 3*neld+1)]

! creacion do punteiro row
 call sparse_build2(A, A%colptr(3*neld+1) - 1) ! sets nnz and allocates rowind and nzval
 A%nzval = 0. ! sparse_add needs 0
!A%rowind = 0

do k = 1, neld
  do i = 1, 3
     A%rowind(A%colptr(3*(k-1)+i):A%colptr(3*(k-1)+i)+lnn-1) = vecino(k,:)
  enddo
enddo

deallocate(vecino)

end subroutine

end module morse_nd_mod

