module morse_crd_mod

use sparse_class

implicit none

contains

! engadido tot_diel a ver se funciona. revisar.
! k -> tot_diel(k)
! k -> k

subroutine morse_crd(A,nnodd,neld,nnd, tot_diel)

implicit none
integer, parameter     :: lnn = 4 !numero de nodos (Cruzeix-Raviart) por elemento
integer,intent(IN)     :: nnd(lnn,neld) ! nn do dielectrico
integer,intent(IN)     :: nnodd ! numero de nodos (caras) da malla do dielectrico
integer,intent(IN)     :: neld ! numero de elementos da malla do dielectrico
TYPE(sparse_real),intent(INOUT) :: A
integer,intent(IN)     :: tot_diel(neld) ! array: elemento_do_dielectrico -> elemento_do_dielectrico [¿significado?]


INTEGER,ALLOCATABLE    :: nvecino(:),vecino(:,:)
INTEGER                :: i,j,k,jj,temp,aux(lnn)

 call sparse_free(A)

 call sparse_build1(A, 3*neld, nnodd) ! sets size(rows x cols) and allocates colptr
!A%colptr = 0
allocate (nvecino(nnodd), vecino(nnodd, 2))
nvecino = 0
!vecino  = 0

do k = 1,neld !calculamos o tetraedros vecinhos dunha cara
  aux = nnd(:,tot_diel(k)) ! engadido tot_diel
  do i = 1, lnn !bucle nos nodos (caras) do elemento dos que imos buscar vecinhos
        nvecino(aux(i)) = nvecino(aux(i)) + 1 !unha cara pode ter un ou dous elementos vecinhos
        vecino(aux(i), nvecino(aux(i))) = k ! non engadido tot_diel
  enddo
enddo

!ordenacion dos nodos vecinhos, de menor a maior
do i = 1, nnodd
  if (nvecino(i) == 2) then
     if (vecino(i,1) > vecino(i,2)) then
         temp        = vecino(i,2)
         vecino(i,2) = vecino(i,1)
         vecino(i,1) = temp
     endif
  endif
enddo

!creacion do punteiro colptr
A%colptr(1) = 1
do i = 1, nnodd
  A%colptr(i+1) = A%colptr(i) + 3*nvecino(i)
enddo

! creacion do punteiro row
 call sparse_build2(A, A%colptr(nnodd+1) - 1) ! sets nnz and allocates rowind and nzval
 A%nzval = 0. ! sparse_add needs 0
!A%rowind = 0

do i = 1, nnodd !bucle en nodos (caras) da malla do dielectrico
   do j = 1, nvecino(i) !bucle en tereaedros vecinhos desa cara
     do jj = 1, 3 !bucle nas tres componhentes da incognita
       A%rowind(A%colptr(i) + 3*(j-1) + jj-1) =  3*(vecino(i,j)-1) + jj
     enddo
   enddo
enddo

deallocate(nvecino, vecino)

end subroutine

end module morse_crd_mod

