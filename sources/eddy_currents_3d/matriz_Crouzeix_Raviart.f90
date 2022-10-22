module matriz_Crouzeix_Raviart_mod

use globales
use sparse_class
use module_MATH
use morse_crd_mod

implicit none

contains

subroutine matriz_Crouzeix_Raviart()
implicit none
integer :: i,j,k
real(real64) :: tol
real(real64), dimension(3,4) :: coord
real(real64), dimension(4,3), parameter :: temp1 = reshape([0,1,0,0, 0,0,1,0, 0,0,0,1],[4,3]) ! [0 0 0;eye(3)]
!real(real64), dimension(4,4) :: temp2
real(real64), dimension(4,4) :: temp2inv
real(real64), dimension(3,4) :: GG
real(real64), dimension(3,meshdiel%nel) :: baric_t, baric_d
real(real64), dimension(1,meshdiel%nel) :: ones
integer, dimension(meshdiel%nel) :: tot_diel
!real(real64), dimension(meshdiel%nel) :: tempA
integer, dimension(1) :: tempB ! tamanho == size(shape(tempA))
real(real64), dimension(:), allocatable :: values
real(real64), dimension(:,:), allocatable :: col
type(sparse_real) :: sparsecol
!allocate(values(1))
 ones = 1

!%******************************************************************************
!% Identificacion tetraedro Total vs Dielectrico
!%******************************************************************************

 do k = 1, meshdiel%nel
    coord = meshcond%z(:,meshcond%mm(:,dofdiel(k)))
    baric_t(:,k) = sum(coord,2)/4
    coord = meshdiel%z(:,meshdiel%mm(:,k))
    baric_d(:,k) = sum(coord,2)/4
 enddo

 ! abs -> dabs ?
 tol = 1.e-6*maxval(abs(meshdiel%z))

 print *, 'matriz_Crouzeix_Raviart: loop1: before'
 do k = 1, meshdiel%nel
    tempB(1) = find_1l1(&
        maxval(abs(matmul(reshape(baric_t(:,k),[3,1]), ones) - baric_d), 1) < tol)

!    tempA = maxval(abs(matmul(reshape(baric_t(:,k),[3,1]), ones) - baric_d), 1)
!    tempB = maxloc(tempA, tempA < tol)

    if (tempB(1) == 0) then
        write (error_unit,*), 'Error: matriz_Crouzeix_Raviart: index not found'
        stop 1
    endif
    tot_diel(k) = tempB(1)
 enddo
 print *, 'matriz_Crouzeix_Raviart: loop1: after'

!%******************************************************************************
!% Definicion de la matriz como sparse
!%******************************************************************************

! call build(P,3*meshdiel%nel,meshdiel%nnod)
 ! construe perfil de nonceros
 call morse_crd(P, meshdiel%nnod, meshdiel%nel, meshdiel%nn, tot_diel) ! 3*meshdiel%nel , meshdiel%nnod

!%******************************************************************************
!% Bucle en elementos para el calculo de la matriz
!%******************************************************************************

 do k = 1, meshdiel%nel
    coord = meshcond%z(:,meshcond%mm(:,dofdiel(k)))
!    temp2 = reshape([1.0d0,coord(:,1),1.0d0,coord(:,2),1.0d0,coord(:,3),1.0d0,coord(:,4)],[4,4])

!    if (.not. inverse4x4(temp2, temp2inv)) &
!        call sperror('Unable to find matrix inverse in "matriz_Crouczeix_Raviart.f90(...)" subroutine')

    if (.not. inverse3x4_1(coord, temp2inv)) &
        call sperror('Unable to find matrix inverse in "matriz_Crouczeix_Raviart.f90(...)" subroutine')

    GG = transpose(matmul(temp2inv, temp1)) ! GG = (inv([1 1 1 1;coord])*[0 0 0;eye(3)])';
    GG = -3 * volT(dofdiel(k)) * GG (:,[4,2,3,1])

    call sparse_add(P, GG, 3*(k-1)+[1,2,3], meshdiel%nn(:,tot_diel(k)))
 enddo

 if (size(nod_comp_con, 1)>1) then

     if (allocated(col)) deallocate(col)
     if (allocated(values)) deallocate(values)
     allocate(col(P%nr,1))

     col = 0

     do i = 2, size(nod_comp_con, 1)
        do j = 1, size(nod_comp_con(i)%diel_refs,1)
            call sparse_get_col(P, nod_comp_con(i)%diel_refs(j), values)
            col(:,1) = col(:,1) + values(:) !dense column
        enddo
     enddo

     call sparse_from_dense(sparsecol, col)
     !call sparse_sum()
     call sparse_add_last_column(P,sparsecol)

     call sparse_free(sparsecol)
 endif
! 2 -> 3 s.

 call sparse_remove_cols(P, nod_frontera_total)
 !call sparse_collapse(P)

 if (allocated(col)) deallocate(col)
 if (allocated(values)) deallocate(values)
 
end subroutine matriz_Crouzeix_Raviart

end module matriz_Crouzeix_Raviart_mod

