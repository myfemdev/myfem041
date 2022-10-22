module condicion_intensidad_general_mod

use globales
use module_MATH
use morse_id_row_mod

implicit none

contains

subroutine condicion_intensidad_general()
implicit none
!integer, dimension(:), allocatable :: ainput1
!integer, dimension(:), allocatable :: tinput1
!integer, dimension(:), allocatable :: ainput
!integer, dimension(:), allocatable :: tinput
integer, dimension(:), allocatable :: refarista
integer, dimension(:), allocatable :: nnaa
integer, dimension(:), allocatable :: pp
integer, dimension(:,:), allocatable :: numglover
integer, dimension(:), allocatable :: results
integer, dimension(:), allocatable :: aux_index
integer, dimension(:), allocatable :: aux_value
integer, dimension(:), allocatable :: aux_zu_index
complex(real64), dimension(:), allocatable :: aux_zu_value
integer :: inputlen, ppost, ppostvalue
logical :: ppostset
integer, dimension(:), allocatable :: pposts
integer :: i, j, k, tempi
complex(real64) :: tempr
integer :: sss1, sss2, sss3
integer :: fsigno
integer :: input
type(rowstruct), dimension(:), allocatable :: rows
type(rowstruct), dimension(:), allocatable :: rows2
integer :: minidx
integer :: minrow, fg2, tg2
integer, dimension(4,3) :: ac = reshape([1,3,1,2, 2,4,4,5, 3,6,5,6],[4,3])
integer, dimension(4,3) :: vc = reshape([1,1,1,2, 3,4,2,3, 2,3,4,4],[4,3])
integer, dimension(3)   :: a1, a2
integer, dimension(:), allocatable :: cc, tt, fg, tg, ag, posg
integer, dimension(:,:), allocatable :: acs
integer, dimension(:), allocatable :: gh, afron

 allocate (pposts(num_inputs))
 allocate (aux_zu_index(num_inputs))
 allocate (aux_zu_value(num_inputs))
 allocate (rows(num_inputs))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! inputs(i)%boundary_references son superficies
! inputs(i)%boundary_references soamente vai ter un valor
! Se busca la frontera de la referencia

 if (allocated(gh)) deallocate(gh)
 allocate(gh(meshcond%nnod))
 if (allocated(sel_refs)) deallocate(sel_refs)
 allocate(sel_refs(meshcond%nnod))
 if (allocated(afron)) deallocate(afron)
 allocate(afron(num_inputs))

 sel_refs = 0

 do i = 1, num_inputs
    gh = 0
    call find_2v(meshcond%nrc, inputs(i)%boundary_references, cc, tt)

    if (allocated(acs)) deallocate(acs)
    allocate(acs(size(cc,1),size(ac,2)))

    acs = ac(cc,:)
    do j = 1, size(tt,1)
            gh(meshcond%nn(acs(j,:),tt(j))) = gh(meshcond%nn(acs(j,:),tt(j))) + 1
    enddo

    call find_1v(gh, [1], afron)
    sel_refs(afron) = inputs(i)%boundary_references(1)

 enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 do input = 1, num_inputs

    ppostset = .false.
    ppostvalue = 0

!   Si la referencia de la condicion es una linea:
!    ! nra  6 x nel
!    call find_2v(meshcond%nra, inputs(input)%boundary_references, ainput1, tinput1)
!
!    allocate(refarista(size(ainput1,1)))
!
!    ! nn  6 x nel
!    do j = 1 , size(ainput1,1)
!       refarista(j) = meshcond%nn(ainput1(j), tinput1(j))
!    enddo

    call find_1v(sel_refs, inputs(input)%boundary_references, refarista)

    call unique2(refarista, nnaa, pp)! nnaa: unico, ordenado ; pp: os indices dos ultimos valores iguais que aparecen

    if (size(nnaa,1)<1) then
       write (error_unit,*), 'Error: condicion_intensidad_noparametrica: empty array: nnaa'
       stop 1
    endif

    ppost = nnaa(1)

    inputlen = size(pp,1)
   ! allocate(ainput(inputlen))
   ! allocate(tinput(inputlen))
   ! ainput = ainput1(pp) ! non usado !
   ! tinput = tinput1(pp) ! non usado !

    allocate(numglover(size(exa,1),size(nnaa,1)))
    numglover = exa(:,nnaa)

    sss1 = 1
    sss2 = numglover(1,1)

    if (allocated(aux_index)) deallocate(aux_index)
    allocate(aux_index(inputlen))
    if (allocated(aux_value)) deallocate(aux_value)
    allocate(aux_value(inputlen))


    do j = 1 , inputlen

       call find_1s(numglover(1,[(i, i=1, sss1-1), (i, i=sss1+1, inputlen)]), sss2, results)
       if (size(results,1) == 0) then
           call find_1s(numglover(2,[(i, i=1, sss1-1), (i, i=sss1+1, inputlen)]), sss2, results)
           fsigno = -1
       else
           fsigno = 1
       endif

       if (size(results,1) /= 1) then
           write (error_unit,*), 'Error: condicion_intensidad_general: array of size not equal to 1: results'
           stop 1
       endif
       sss3 = results(1)
       if (sss3 >= sss1) then
           sss3 = sss3 + 1
       endif

   !    print *, 'D', ppost, sss3, nnaa(sss3), fsigno
       ! non ordenados
       aux_index(j) = nnaa(sss3)
       aux_value(j) = fsigno
       if (aux_index(j) == ppost) then
            ppostset = .true.
            ppostvalue = aux_value(j)
       endif

       if (fsigno == 1) then
           sss2 = numglover(2,sss3)
       else
           sss2 = numglover(1,sss3)
       endif

       sss1 = sss3
    enddo

    ! ordenar aux_index, aux_value, pouco eficientemente (burbulla)
    do i = 1, inputlen - 1
       do j = i, inputlen
           if (aux_index(i) > aux_index(j)) then ! swap

               tempi = aux_index(j)
               aux_index(j) = aux_index(i)
               aux_index(i) = tempi

               tempi = aux_value(j)
               aux_value(j) = aux_value(i)
               aux_value(i) = tempi

           endif
       enddo
    enddo

    allocate(rows(input)%indexes(inputlen))
    allocate(rows(input)%values(inputlen))
    rows(input)%rowsize = inputlen
    rows(input)%rownumber = ppost
    rows(input)%indexes = aux_index
    rows(input)%values = aux_value ! integer -> real

!    if (sparse_get_element(D, ppost, ppost) == 1.0d0) then
    if (.not. ppostset .or. ppostvalue == 1) then
!       call sparse_scale_row(D, ppost, -1.0d0)
       rows(input)%values = -rows(input)%values
       aux_zu_index(input) = ppost
       aux_zu_value(input) = inputs(input)%intensity*dsqrt(2.d0)*(dcos(inputs(input)%phase*pi/180.d0) &
                             + dcmplx(0.d0,1.d0)*dsin(inputs(input)%phase*pi/180.d0))
    else
       aux_zu_index(input) = ppost
       aux_zu_value(input) = -inputs(input)%intensity*dsqrt(2.d0)*(dcos(inputs(input)%phase*pi/180.d0) &
                             + dcmplx(0.d0,1.d0)*dsin(inputs(input)%phase*pi/180.d0))
    endif
    pposts(input) = ppost


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Orientacion

    call find_2v(meshcond%nrc, inputs(input)%boundary_references, fg, tg)
    call find_2s(meshcond%nn(:,tg), ppost, ag, posg)

    if (.not.(size(ag,1)==1) .and. (size(posg,1)==1)) then
       write (error_unit,*), 'Error: condicion_intensidad_general: wrong size array: ag, posg'
       stop 1
    endif

    fg2 = fg(posg(1))
    tg2 = tg(posg(1))

    a1 = meshcond%mm(vc(fg2,:),tg2)
    a2 = exa(:,ppost)

    if (( ((a2(1)==a1(1)) .and. (a2(2)==a1(2))) .or.&
          ((a2(1)==a1(2)) .and. (a2(2)==a1(3))) .or.&
          ((a2(1)==a1(3)) .and. (a2(2)==a1(1))) )) then

        k = find_1i1(aux_zu_index, ppost)
        aux_zu_value(k) = -aux_zu_value(k)

    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! liberar memoria

    if (allocated(refarista)) deallocate(refarista)
    if (allocated(nnaa))      deallocate(nnaa)
    if (allocated(pp))        deallocate(pp)
    if (allocated(numglover)) deallocate(numglover)
    if (allocated(aux_index)) deallocate(aux_index)
    if (allocated(aux_value)) deallocate(aux_value)
    if (allocated(results))   deallocate(results)

 enddo ! input = 1, num_inputs


    ! ordenar aux_zu_index, aux_zu_value, pouco eficientemente (burbulla)
    do i = 1, num_inputs - 1
       do j = i, num_inputs
           if (aux_zu_index(i) > aux_zu_index(j)) then ! swap

               tempi = aux_zu_index(j)
               aux_zu_index(j) = aux_zu_index(i)
               aux_zu_index(i) = tempi

               tempr = aux_zu_value(j)
               aux_zu_value(j) = aux_zu_value(i)
               aux_zu_value(i) = tempr

           endif
       enddo
    enddo

    ! crea unha matriz sparse con valores nunha columna
    call sparse_col(zerosunos, meshcond%nnod, 1, 1, aux_zu_index, aux_zu_value)

    ! ordenar as filas de rows en rows2. complexidade non optima (pouco eficiente)
    allocate(rows2(num_inputs))
    do j = 1, num_inputs
        rows(j)%counted = .false.
    enddo
    do i = 1, num_inputs
        minidx = 0
        minrow = 0
        do j = 1, num_inputs
            if ((.not. rows(j)%counted) .and. (minidx == 0 .or. minrow > rows(j)%rownumber)) then
                minidx = j
                minrow = rows(j)%rownumber
            endif
        enddo
        rows(minidx)%counted = .true.
        allocate(rows2(i)%indexes(rows(minidx)%rowsize))
        allocate(rows2(i)%values(rows(minidx)%rowsize))
        rows2(i)%rowsize = rows(minidx)%rowsize
        rows2(i)%rownumber = rows(minidx)%rownumber
        rows2(i)%indexes = rows(minidx)%indexes(1:rows(minidx)%rowsize)
        rows2(i)%values = rows(minidx)%values(1:rows(minidx)%rowsize)
    enddo

    ! matriz con diagonal e varias filas. rows2 ordenado e ordenado
    call morse_id_rows(D, meshcond%nnod, meshcond%nnod, rows2)

    ! there is no need to sort pposts
    ! eliminar columnas
    call sparse_remove_cols(D, pposts)

 ! liberar memoria
 if (allocated(pposts))              deallocate(pposts)
 if (allocated(aux_zu_index))        deallocate(aux_zu_index)
 if (allocated(aux_zu_value))        deallocate(aux_zu_value)
 do i = 1, num_inputs
    if (allocated(rows(i)%indexes))  deallocate(rows(i)%indexes)
    if (allocated(rows(i)%values))   deallocate(rows(i)%values)
 enddo
 if (allocated(rows))                deallocate(rows)
 do i = 1, num_inputs
    if (allocated(rows2(i)%indexes)) deallocate(rows2(i)%indexes)
    if (allocated(rows2(i)%values))  deallocate(rows2(i)%values)
 enddo
 if (allocated(rows2))               deallocate(rows2)

 if (allocated(gh))                  deallocate(gh)
 if (allocated(sel_refs))            deallocate(sel_refs)
 if (allocated(afron))               deallocate(afron)
 if (allocated(acs))                 deallocate(acs)

end subroutine condicion_intensidad_general

end module condicion_intensidad_general_mod
