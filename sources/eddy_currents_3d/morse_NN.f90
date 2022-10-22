module morse_nn_mod

use sparse_class

implicit none

contains

! Nedelec - Nedelec
subroutine morse_nn(A, nver, nel, nn)

implicit none
integer, parameter :: lnn = 6 ! numero de nodos (Nedelec) por elemento
integer,intent(IN)     :: nn(lnn,nel) ! nn da malla total
integer,intent(IN)     :: nver ! nnod malla total
integer,intent(IN)     :: nel ! nel malla total
TYPE(sparse_real),intent(INOUT) :: A

INTEGER,ALLOCATABLE    :: nvecino(:),vecino(:,:)
INTEGER                :: i,j,k,jj,temp,aux(lnn)

 call sparse_free(A)

 call sparse_build1(A, nver, nver) ! sets size(rows x cols) and allocates colptr
!A%colptr = 0
allocate (nvecino(nver),vecino(nver,100))
nvecino = 0
!vecino  = 0

do k = 1,nel
  aux = nn(:,k)
  do i = 1, lnn !bucle nos nodos do elemento dos que imos buscar vecinhos
  inner: do j = 1, lnn !bucle nos nodos do elemento que poden ser vecinhos do i-esimo
!      if (i == j) cycle !un nodo non é vecinho de si mesmo
      if (nvecino(aux(i)) == 0) then !non hai vecinhos cos que comparar 
        nvecino(aux(i)) = nvecino(aux(i)) + 1
        vecino(aux(i), nvecino(aux(i))) = aux(j) 
      else
        do jj = 1, nvecino(aux(i)) !bucle nos vecinhos gardados para nn(i,k)
          if (vecino(aux(i),jj) == aux(j)) cycle inner
        enddo
        nvecino(aux(i)) = nvecino(aux(i)) + 1
        vecino(aux(i), nvecino(aux(i))) = aux(j) 
       endif
     enddo inner
   enddo
enddo

!ordenacion dos nodos vecinhos, de menor a maior
do i = 1, nver
  do j = 1, nvecino(i) - 1
    do jj = j + 1, nvecino(i)
      if (vecino(i,j) > vecino(i,jj)) THEN
         temp         = vecino(i,jj)
         vecino(i,jj) = vecino(i,j)
         vecino(i,j)  = temp
      endif
    enddo
  enddo
enddo

! creacion do punteiro colptr
A%colptr(1) = 1
do i = 1, nver
  A%colptr(i+1) = A%colptr(i) + nvecino(i)
enddo

! creacion do punteiro row
 call sparse_build2(A, A%colptr(nver+1) - 1) ! sets nnz and allocates rowind and nzval
 A%nzval = 0. ! sparse_add needs 0
!A%rowind = 0

do i = 1, nver
   do j = 1, nvecino(i)
     A%rowind(A%colptr(i) + j - 1) = vecino(i,j)
   enddo
enddo

deallocate(nvecino, vecino)

end subroutine

end module morse_nn_mod

