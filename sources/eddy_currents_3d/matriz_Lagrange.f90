module matriz_Lagrange_mod

use globales
use sparse_class
use module_MATH
use morse_nd_mod

implicit none

contains


subroutine matriz_Lagrange()
implicit none
integer :: i, k
real(real64), dimension(4,3), parameter :: temp1 = reshape([0,1,0,0, 0,0,1,0, 0,0,0,1],[4,3]) ! [0 0 0;eye(3)]
real(real64), dimension(3,4) :: coord ! 3 * 4 ! meshcond%dim,meshcond%lnv
!real(real64), dimension(4,4) :: temp2
real(real64), dimension(4,4) :: temp2inv
real(real64), dimension(6,6) :: temp4 = 0 ! so diagonal ! vale aqui porque so se modifica diagonal
real(real64), dimension(3,4) :: GG
real(real64), dimension(3,6) :: MMM
real(real64), dimension(3,6) :: curlBase

 ! call build(N,meshcond%nnod,3*meshdiel%nel)
 ! construe perfil de nonceros
 call morse_nd(N, meshcond%nn, meshcond%nnod, meshdiel%nel, dofdiel) ! meshcond%nnod , 3*meshdiel%nel

 do k = 1, size(dofdiel,1)
    coord = meshcond%z(:,meshcond%mm(:,dofdiel(k)))

    ! [1 1 1 1;coord]
!    temp2 = reshape([1.0d0,coord(:,1),1.0d0,coord(:,2),1.0d0,coord(:,3),1.0d0,coord(:,4)],[4,4])

!    if (.not. inverse4x4(temp2, temp2inv)) &
!        call sperror('Unable to find matrix inverse in "matriz_Lagrange.f90(...)" subroutine')

    if (.not. inverse3x4_1(coord, temp2inv)) &
        call sperror('Unable to find matrix inverse in "matriz_Lagrange.f90(...)" subroutine')

    GG = transpose(matmul(temp2inv, temp1)) ! GG = (inv([1 1 1 1;coord])*[0 0 0;eye(3)])';

    MMM = reshape([ cross3(GG(:,1),GG(:,2)), cross3(GG(:,2),GG(:,3)), cross3(GG(:,3),GG(:,1)), &
        & cross3(GG(:,1),GG(:,4)), cross3(GG(:,2),GG(:,4)), cross3(GG(:,3),GG(:,4)) ], [3,6])

    do i = 1, 6 ! temp4 = diag(sga(:,dofdiel(k))) ! sga: 6 * nel
        temp4(i,i) = sga(i,dofdiel(k))
    enddo

    curlBase = 2 * volT(dofdiel(k)) * matmul(MMM, temp4)

    call sparse_add(N, transpose(curlBase), meshcond%nn(:,dofdiel(k)), 3*(k-1)+[1,2,3])
 enddo

end subroutine matriz_Lagrange

end module matriz_Lagrange_mod

