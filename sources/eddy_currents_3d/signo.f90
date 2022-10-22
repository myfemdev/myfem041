module signo_mod

use globales

implicit none

contains

subroutine signo()
implicit none
integer :: k, j

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% SIGNO.M                             
!%                                       
!% Calculo de los vectores sga y exa que indican la orientacion de las aristas en el conductor   
!% sga (6*nel)    : indican la orientacion de las aristas
!% exa (2*nnod)   : extremos de la arista
!% ref_aristas (1*nnod) : referencia de las aristas en un solo vector
!% 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (allocated(sga) .and. (size(sga,1) /= 6 .or. size(sga,2) /= meshcond%nel) ) deallocate(sga)
if (.not. allocated(sga)) allocate(sga(6,meshcond%nel))
sga = 0

if (allocated(exa) .and. (size(exa,1) /= 2 .or. size(exa,2) /= meshcond%nnod) ) deallocate(exa)
if (.not. allocated(exa)) allocate(exa(2,meshcond%nnod))
exa = 0

if (allocated(ref_aristas) .and. size(ref_aristas,1) /= meshcond%nnod) deallocate(ref_aristas)
if (.not. allocated(ref_aristas)) allocate(ref_aristas(meshcond%nnod))
ref_aristas = 0

if (allocated(iflag) .and. size(iflag,1) /= meshcond%nnod) deallocate(iflag)
if (.not. allocated(iflag)) allocate(iflag(meshcond%nnod))
iflag = 0

!%*****************************************************************************%
!% Calculo del tablero de signos (sga) que fija la orientacion de las aristas  %
!%*****************************************************************************%

b1: do k = 1, meshcond%nel
b2:     do j = 1, 6
        if (iflag(meshcond%nn(j,k)) == 0) then
            sga(j,k) = 1
            iflag(meshcond%nn(j,k)) = 1
            ref_aristas(meshcond%nn(j,k)) = meshcond%nra(j,k)
            exa(1,meshcond%nn(j,k)) = meshcond%mm(e(1,j),k)
            exa(2,meshcond%nn(j,k)) = meshcond%mm(e(2,j),k)
        else
            if(meshcond%mm(e(1,j),k) == exa(1,meshcond%nn(j,k)) .and.&
                    &meshcond%mm(e(2,j),k) == exa(2,meshcond%nn(j,k))) then
                sga(j,k) = 1
            elseif (meshcond%mm(e(1,j),k) == exa(2,meshcond%nn(j,k)) .and.&
                    &meshcond%mm(e(2,j),k) == exa(1,meshcond%nn(j,k))) then
                sga(j,k) = -1
            else
                write (error_unit,*), 'Invalid option in "signo()" subroutine'
                stop 1 ! de acordo
            endif
        endif
    enddo b2
enddo b1

deallocate(iflag)

end subroutine signo


subroutine signo_diel()
end subroutine signo_diel


end module signo_mod

