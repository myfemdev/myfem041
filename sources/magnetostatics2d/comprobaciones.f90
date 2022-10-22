
! comprobaciones sobre los datos de entrada

module comprobaciones

use derivados
use electros_2D

implicit none

public :: comprueba

private

integer :: i, j

contains

logical function comprueba()
    comprueba = .FALSE.

    ! Dirichlet por funcion
    !nrd, irefd
    ! Dirichlet por constante
    !blofron%numero, blofron%referencias, blofron%valor

    if (dirichlet_bc%numero .lt. 1) then
        print*, 'ERROR: at least one Dirichlet B.C. reference must exist'
        return
    endif

    do i=1,dirichlet_bc%numero
        do j=1,i-1
            if (dirichlet_bc%referencias(i) == dirichlet_bc%referencias(j)) then
                print*, 'ERROR: duplicated Dirichlet reference ', dirichlet_bc%referencias(i)
                return
            endif
        enddo
    enddo
    
     if (neumann_bc%numero .ge. 2) then
       do i=1,neumann_bc%numero
         do j=1,i-1
            if (neumann_bc%referencias(i) == neumann_bc%referencias(j)) then
                print*, 'ERROR: duplicated Neumann reference ', neumann_bc%referencias(i)
                return
            endif
          enddo
        enddo
      end if
   
    

    ! Dirichlet / Neumann

    do i=1,dirichlet_bc%numero
        do j=1,neumann_bc%numero
            if (dirichlet_bc%referencias(i) == neumann_bc%referencias(j))then           
              print*, 'ERROR: the same reference is used in Neumann and Dirichlet B.C.',dirichlet_bc%referencias(i)
              return
            endif
        enddo
    enddo
   


    ! fuentes volumicas
    ! sourcevol{numero,modo,referencias,valor}

    do i=1,sourcevol%numero
        do j=1,i-1
            if (sourcevol%referencias(i) == sourcevol%referencias(j)) then
                print*, 'ERROR: duplicated reference in current source', sourcevol%referencias(i)
                return
            endif
        enddo
    enddo


    ! relative magnetic permittivity

    do i=1,permagrel%numero
        do j=1,i-1
            if (permagrel%referencias(i) == permagrel%referencias(j)) then
                print*, 'ERROR: duplicated reference in subdomain material assignment', permagrel%referencias(i)
            endif
        enddo
    enddo


    comprueba = .TRUE.
    
    return
    
end function comprueba


end module comprobaciones

