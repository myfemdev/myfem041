module matriz_Nedelec_mod

use globales
use sparse_class
use signo_mod
use mlocalt_mod
use morse_nn_mod

implicit none

contains


subroutine matriz_Nedelec()
implicit none
integer :: kk, t
real(real64), dimension(6,6) :: MT
real(real64), dimension(6,6) :: KT
real(real64) :: mu

!%******************************************************************************
!% Definicion de la matriz como sparse
!%******************************************************************************

! call build(M,meshcond%nnod,meshcond%nnod)
! call build(Kc,meshcond%nnod,meshcond%nnod)
 ! construe perfil de nonceros
 call morse_nn(M, meshcond%nnod, meshcond%nel, meshcond%nn) ! meshcond%nnod , meshcond%nnod
 call morse_nn(Kc, meshcond%nnod, meshcond%nel, meshcond%nn) ! meshcond%nnod , meshcond%nnod

!%******************************************************************************
!% Bucle en elementos para el calculo de la matriz
!%******************************************************************************

 ! para que esté listo en mlocalt, donde se va rellenando
 if (allocated(volT) .and. (size(volT,1) /= meshcond%nel) ) deallocate(volT)
 if (.not. allocated(volT)) allocate(volT(meshcond%nel))
 volT = 0

 do kk=1, meshcond%nel

    call mlocalt(kk,MT,KT)

    mu = mu0 ! por defecto
    do t = 1, all_num
        if (meshcond%nsd(kk) == all_refs(t)) then
            mu = all_mus(t) ! xa e absoluta
            exit ! exit en vez de cycle por si hay valores duplicados en all_refs
        endif
    enddo
    call sparse_add(M, mu*omega1*MT, meshcond%nn(:,kk), meshcond%nn(:,kk));

    do t = 1, conductor_num
        if (meshcond%nsd(kk) == conductor_refs(t)) then
            call sparse_add(Kc, (1.0d0/conductor_sigmas(t))*KT, meshcond%nn(:,kk), meshcond%nn(:,kk));
            exit ! exit en vez de cycle por si hay valores duplicados en conductor_refs
        endif
    enddo

 enddo


end subroutine matriz_Nedelec

end module matriz_Nedelec_mod
