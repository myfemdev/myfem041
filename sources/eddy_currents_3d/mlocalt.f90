module mlocalt_mod

use globales
use module_MATH

implicit none

contains

subroutine mlocalt(kk, MT, KT)
implicit none
integer, intent(IN) :: kk ! numero de elemento
real(real64), dimension(6,6), intent(OUT) :: MT
real(real64), dimension(6,6), intent(OUT) :: KT
integer :: i, j
real(real64), dimension(3,4) :: coord ! 3 * 4 ! meshcond%dim,meshcond%lnv
real(real64), dimension(4,3), parameter :: temp1 = reshape([0,1,0,0, 0,0,1,0, 0,0,0,1],[4,3]) ! [0 0 0;eye(3)]
!real(real64), dimension(4,4) :: temp2
real(real64), dimension(4,4) :: temp2inv
real(real64), dimension(6,6) :: temp3 = 0 ! so diagonal ! vale aqui porque so se modifica diagonal
real(real64), dimension(6,6) :: temp4 = 0 ! so diagonal ! vale aqui porque so se modifica diagonal
real(real64), dimension(4,3) :: G1
real(real64), dimension(4,4) :: G

 coord = meshcond%z(:,meshcond%mm(:,kk)) ! z: dim * nver
 volT(kk) = det(coord(:,[2,3,4])-coord(:,[1,1,1]))/6 ! determinante
! temp2 = reshape([1.0d0,coord(:,1),1.0d0,coord(:,2),1.0d0,coord(:,3),1.0d0,coord(:,4)],[4,4]) ! [1 1 1 1;coord]

! if (.not. inverse4x4(temp2, temp2inv)) &
!    call sperror('Unable to find matrix inverse in "mlocalt(...)" subroutine')

 if (.not. inverse3x4_1(coord, temp2inv)) &
    call sperror('Unable to find matrix inverse in "mlocalt(...)" subroutine')

 G1 = matmul(temp2inv, temp1) ! G1 = inv([1 1 1 1;coord])*[0 0 0;eye(3)];
 G = matmul(G1,transpose(G1))

 KT = 0.0d0
 do i = 1, 6
    do j = i, 6
        KT(i,j) = (G(e(1,i),e(1,j))*G(e(2,i),e(2,j)))-(G(e(1,i),e(2,j))*G(e(2,i),e(1,j)))
    enddo
 enddo

 do i = 1, 6 ! temp3 = diag(diag(KT))
    temp3(i,i) = KT(i,i)
 enddo
 KT = 4 * volT(kk) * (KT+transpose(KT)-temp3)
 do i = 1, 6 ! temp4 = diag(sga(:,kk)) ! sga: 6 * nel
    temp4(i,i) = sga(i,kk)
 enddo
 KT = matmul(temp4,matmul(KT,temp4))


 MT = 0.0d0
 do i = 1, 6
    MT(i,i) = 0.1*(G(e(1,i),e(1,i))+G(e(2,i),e(2,i))-G(e(1,i),e(2,i)))
 enddo

 do i = 1, 5
    do j = i+1, 6
        if (e(2,i)==e(1,j)) then
            MT(i,j) = 0.05*(G(e(2,i),e(2,j))-G(e(2,i),e(1,j))-2*G(e(1,i),e(2,j))+G(e(1,i),e(1,j)))
        elseif (e(1,i)==e(2,j)) then
            MT(i,j) = 0.05*(G(e(2,j),e(2,i))-G(e(2,j),e(1,i))-2*G(e(1,j),e(2,i))+G(e(1,j),e(1,i)))
        elseif (e(2,i)==e(2,j)) then
            MT(i,j) = 0.05*(G(e(2,i),e(2,j))-G(e(2,i),e(1,j))-G(e(1,i),e(2,j))+2*G(e(1,i),e(1,j)))
        elseif (e(1,i)==e(1,j)) then
            MT(i,j) = 0.05*(2*G(e(2,i),e(2,j))-G(e(2,i),e(1,j))-G(e(1,i),e(2,j))+G(e(1,i),e(1,j)))
        else
            MT(i,j) = 0.05*(G(e(2,i),e(2,j))-G(e(2,i),e(1,j))-G(e(1,i),e(2,j))+G(e(1,i),e(1,j)))
        endif
    enddo
 enddo

 !temp3 = 0 ! non necesario (so modificou a diagonal)
 do i = 1, 6 ! temp3 = diag(diag(MT))
    temp3(i,i) = MT(i,i)
 enddo
 MT = volT(kk) * (MT+transpose(MT)-temp3)
 MT = matmul(temp4,matmul(MT,temp4))

end subroutine mlocalt


end module mlocalt_mod

