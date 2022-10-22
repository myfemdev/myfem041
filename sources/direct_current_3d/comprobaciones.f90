
! comprobaciones sobre los datos de entrada

module comprobaciones

use mesh

use fich_electros3D
use electros3D
use cargavol
use cargacur
use cargapun
use conductividad
use bloqueo
use derivados3D
use auxiliar_cargas
!use parametros_electros3D

implicit none

public :: comprueba, blocking_node, calculate_funs
private


!tamen en readxml.f90
!para usar sin trim
 character(len=27) :: neu1 = 'Normal current density B.C.'
 character(len=22) :: neu2 = 'Normal current density'
 character(len=14) :: dir1 = 'Potential B.C.'
 character(len=9)  :: dir2 = 'Potential'

integer :: i, j

contains

! supone que nrd y blofron%numero tienen valores correctos (incluso si iopblo == 0)
logical function comprueba()
    comprueba = .FALSE.

    ! Dirichlet por funcion
    !nrd, irefd, dir%fun
    ! Dirichlet por constante
    !blofron%numero, blofron%referencias, blofron%valor

    do i=1,nrd
        if (dir%fun(i) <= 0 .or. dir%fun(i) > size(functions,1)) then
            print*, 'ERROR: unknown function in '//dir1
            return
        endif
    enddo
    
    do i=1,nrd
        do j=1,i-1
            if (irefd(i) == irefd(j)) then
                print*, 'ERROR: duplicated reference in '//dir1//' by function', irefd(i)
                return
            endif
        enddo
        do j=1,blofron%numero
            if (irefd(i) == blofron%referencias(j)) then
                print*, 'ERROR: duplicated reference in '//dir1//' (constant/function)', irefd(i)
                return
            endif
        enddo
    enddo
    do i=1,blofron%numero
        do j=1,i-1
            if (blofron%referencias(i) == blofron%referencias(j)) then
                print*, 'ERROR: duplicated reference in '//dir1//' by constant', blofron%referencias(i)
                return
            endif
        enddo
    enddo
    

    ! Neumann por funcion
    !nrn, irefn, neu%fun
    ! Neumann por constante
    !neuman%numero, neuman%referencias, neuman%valor

    do i=1,nrn
        if (neu%fun(i) <= 0 .or. neu%fun(i) > size(functions,1)) then
            print*, 'ERROR: unknown function in '//neu1
            return
        endif
    enddo
    
    do i=1,nrn
        do j=1,i-1
            if (irefn(i) == irefn(j)) then
                print*, 'ERROR: duplicated reference in '//neu1//' by function', irefn(i)
                return
            endif
        enddo
        do j=1,neuman%numero
            if (irefn(i) == neuman%referencias(j)) then
                print*, 'ERROR: duplicated reference in '//neu1//' (constant/function)', irefn(i)
                return
            endif
        enddo
    enddo
    do i=1,neuman%numero
        do j=1,i-1
            if (neuman%referencias(i) == neuman%referencias(j)) then
                print*, 'ERROR: duplicated reference in '//neu1//' by constant', neuman%referencias(i)
                return
            endif
        enddo
    enddo

    
    ! Intensidad por funcion
    !nri, irefi, intf%fun
    ! Intensidad por constante
    !inten%numero, inten%referencias, inten%valor

    do i=1,nri
        if (intf%fun(i) <= 0 .or. intf%fun(i) > size(functions,1)) then
            print*, 'ERROR: unknown function in Intensity B.C.'
            return
        endif
    enddo
    
    do i=1,nri
        do j=1,i-1
            if (irefi(i) == irefi(j)) then
                print*, 'ERROR: duplicated reference in Intensity B.C. by function', irefi(i)
                return
            endif
        enddo
        do j=1,inten%numero
            if (irefi(i) == inten%referencias(j)) then
                print*, 'ERROR: duplicated reference in Intensity B.C. (constant/function)', irefi(i)
                return
            endif
        enddo
    enddo
    do i=1,inten%numero
        do j=1,i-1
            if (inten%referencias(i) == inten%referencias(j)) then
                print*, 'ERROR: duplicated reference in Intensity B.C. by constant', inten%referencias(i)
                return
            endif
        enddo
    enddo

    

    ! Dirichlet / Neumann
    do i=1,nrd
        do j=1,nrn
            if (irefd(i) == irefn(j)) then
                print*, 'ERROR: the same reference is used in '//neu1//' and '//dir1, irefd(i)
                return
            endif
        enddo
        do j=1,neuman%numero
            if (irefd(i) == neuman%referencias(j)) then
                print*, 'ERROR: the same reference is used in '//neu1//' and '//dir1, irefd(i)
                return
            endif
        enddo
    enddo
    do i=1,blofron%numero
        do j=1,nrn
            if (blofron%referencias(i) == irefn(j)) then
                print*, 'ERROR: the same reference is used in '//neu1//' and '//dir1, blofron%referencias(i)
                return
            endif
        enddo
        do j=1,neuman%numero
            if (blofron%referencias(i) == neuman%referencias(j)) then
                print*, 'ERROR: the same reference is used in '//neu1//' and '//dir1, blofron%referencias(i)
                return
            endif
        enddo
    enddo
    
    !do i=1,inten%numero
    !    print *, i, inten%referencias(i)
    !    print *, i, inten%valor(i)
    !enddo
    !print *, '-'
    !o i=1,nri
    !   print *, i, irefi(i)
    !   print *, i, int%fun(i)
    !enddo
    
    ! cargas volumicas
    ! carvol{numero,referencias,valor,constante} vol%fun
    do i=1,carvol%numero
        if ( (.not. carvol%constante(i)) .and. (vol%fun(i) <= 0 .or. vol%fun(i) > size(functions,1)) ) then
            print*, 'ERROR: unknown function in volume source'
            return
        endif
        do j=1,i-1
            if (carvol%referencias(i) == carvol%referencias(j)) then
                print*, 'ERROR: duplicated reference in volume source', carvol%referencias(i)
                return
            endif
        enddo
    enddo
    
    ! cargas superficiales
    ! carsup{numero,referencias,valor,constante} sup%fun
    do i=1,carsup%numero
        if ( (.not. carsup%constante(i)) .and. (sup%fun(i) <= 0 .or. sup%fun(i) > size(functions,1)) ) then
            print*, 'ERROR: unknown function in surface source'
            return
        endif
        do j=1,i-1
            if (carsup%referencias(i) == carsup%referencias(j)) then
                print*, 'ERROR: duplicated reference in surface source', carsup%referencias(i)
                return
            endif
        enddo
    enddo
    
    ! cargas curvilineas
    ! carcur{numero,referencias,valor,constante} cur%fun
    do i=1,carcur%numero
        if ( (.not. carcur%constante(i)) .and. (cur%fun(i) <= 0 .or. cur%fun(i) > size(functions,1)) ) then
            print*, 'ERROR: unknown function in line source'
            return
        endif
        do j=1,i-1
            if (carcur%referencias(i) == carcur%referencias(j)) then
                print*, 'ERROR: duplicated reference in line source', carcur%referencias(i)
                return
            endif
        enddo
    enddo
    
    ! Conductividad electrica
    do i=1,conduc%numero
        if (conduc%iopcond(i) == 1) then
            if (conduc%fun(i)<1 .or. conduc%fun(i)>size(functions_cond,1)) then
                print*, 'ERROR: electrical conductivity function number out of range: ', conduc%fun(i)
                return
            endif
        endif
    enddo
    
    comprueba = .TRUE.
    
    return
end function comprueba



!-1 error 0 not added 1 added
integer function blocking_node()
implicit none
integer, dimension(3) :: datos
integer :: nver
integer :: res

 blocking_node = 0

 if (.not. (nrd.gt.0.or.blofron%numero.gt.0)) then

    print *, 'Neumann problem: assigning 0.0 to last vertex'

    res = mesh_read_array_unformatted(fichma, 3, datos) ! nel, nver, nemm

    nver = datos(2)

    if (res /= 0) then
        print *, 'Neumann problem: ', trim(mesh_error_string(res))
        blocking_node = -1
        return
    endif

    if (nver < 1) then
        print *, 'Neumann problem: wrong number of vertices ', nver
        blocking_node = -1
        return
    endif

    iopblo = 1
    iopblo3 = 1
    blopun%numero = blopun%numero + 1
    blopun%referencias(blopun%numero) = nver
    blopun%valor(blopun%numero) = 0.0d0

    blocking_node = 1

    print *, 'Neumann problem: assigned 0.0 to vertex', nver

 end if

end function blocking_node



        ! calculates the number of the function if there is only one distinct function
        ! -1: mixed functiones
        ! 0: no functions
        ! 1..7: one function
        subroutine calculate_funs()
        implicit none
        integer :: i

            dir%funs = 0
            neu%funs = 0
            vol%funs = 0
            sup%funs = 0
            cur%funs = 0
            intf%funs = 0
            
            ! Dirichlet
            do i=1,nrd
                if (dir%funs==0) then
                    dir%funs = dir%fun(i)
                else
                    if (dir%funs /= dir%fun(i)) then
                        dir%funs = -1
                        exit
                    endif
                endif
            end do

            ! Neumann
            do i=1,nrn
                if (neu%funs==0) then
                    neu%funs = neu%fun(i)
                else
                    if (neu%funs /= neu%fun(i)) then
                        neu%funs = -1
                        exit
                    endif
                endif
            end do
            
            ! Intensity
            do i=1,nri
                if (intf%funs==0) then
                    intf%funs = intf%fun(i)
                else
                    if (intf%funs /= intf%fun(i)) then
                        intf%funs = -1
                        exit
                    endif
                endif
            end do
            
            ! Volume
            do i=1,carvol%numero
                if (.not. carvol%constante(i)) then
                    if (vol%funs==0) then
                        vol%funs = vol%fun(i)
                    else
                        if (vol%funs /= vol%fun(i)) then
                            vol%funs = -1
                            exit
                        endif
                    endif
                endif
            end do
            
            ! Surface
            do i=1,carsup%numero
                if (.not. carsup%constante(i)) then
                    if (sup%funs==0) then
                        sup%funs = sup%fun(i)
                    else
                        if (sup%funs /= sup%fun(i)) then
                            sup%funs = -1
                            exit
                        endif
                    endif
                endif
            end do
            
            ! Line
            do i=1,carcur%numero
                if (.not. carcur%constante(i)) then
                    if (cur%funs==0) then
                        cur%funs = cur%fun(i)
                    else
                        if (cur%funs /= cur%fun(i)) then
                            cur%funs = -1
                            exit
                        endif
                    endif
                endif
            end do

!            print *, 'funs: dir neu vol sup cur int',&
!                dir%funs,neu%funs,vol%funs,sup%funs,cur%funs,int%funs

            
        end subroutine



end module comprobaciones
