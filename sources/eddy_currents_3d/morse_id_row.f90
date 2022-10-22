module morse_id_row_mod

use defines
use sparse_class

implicit none

type rowstruct
logical :: counted
integer :: rownumber
integer :: rowsize
integer, dimension(:), allocatable :: indexes
real(real64), dimension(:), allocatable :: values
end type

contains

! creates an sparse identity matrix with some nonzeros in one row

subroutine morse_id_row(A, nr, nc, row, cols, values)
implicit none
type(sparse_real), intent(inout) :: A
integer, intent(in) :: nr
integer, intent(in) :: nc
integer, intent(in) :: row
integer, dimension(:), intent(in) :: cols ! deben estar ordenadas
real(real64), dimension(:), intent(in) :: values
integer :: colsindex
integer :: num
integer :: n
integer :: col

 ! se existe i tal que (cols(i) == row) => un nonzero menos
 ! nnz = min(nr,nc) + size(cols,1) ! pode ter un exceso de 1

 call sparse_free(A)

 call sparse_build1(A, nr, nc) ! sets size(rows x cols) and allocates colptr

 colsindex = 1
 A%colptr(1) = 1
 do col = 1, nc
    num = 0
    if (colsindex <= size(cols,1)) then
        if (cols(colsindex) == col) then
            if (row /= cols(colsindex)) then ! se cadra na diagonal non o conta por duplicado
                num = num + 1
            endif
            colsindex = colsindex + 1
        endif
    endif
    if (col <= min(nr, nc)) then ! se estamos dentro do cadrado da matrix, conta diagonal
        num = num + 1
    endif
    A%colptr(col + 1) = A%colptr(col) + num
 enddo

! creacion do punteiro row
 call sparse_build2(A, A%colptr(nc+1) - 1) ! sets nnz and allocates rowind and nzval
! A%nzval = 0
! A%rowind = 0

 colsindex = 1
 n = 1
 do col = 1, nc
    num = 0
    if (col <= min(nr, nc)) then ! se estamos dentro do cadrado da matrix, conta diagonal
        num = 1 ! diagonal
    endif
    if (colsindex <= size(cols,1)) then
        if (cols(colsindex) == col) then
            if (row /= col) then ! se non cadra na diagonal:
                if (num == 1) then
                    num = 3 ! diagonal + serie en distinto sitio
                else
                    num = 2 ! serie soa
                endif
            else ! diagonal + serie no mesmo sitio => gardar serie
                num = 2 ! serie na diagonal
            endif
            colsindex = colsindex + 1
        endif
    endif

!    print *, 'col', col, 'num', num, 'ci', colsindex

    if (num == 0) then ! non hai nada
    else if (num == 1) then ! hai diagonal
        A%rowind(n) = col
        A%nzval(n) = 1.0d0
        n = n + 1
    else if (num == 2) then ! hai serie (ou serie e diagonal no mesmo punto)
        A%rowind(n) = row
        A%nzval(n) = values(colsindex - 1)
        n = n + 1
    else if (num == 3) then ! hai serie e diagonal
        if (col > row) then ! se esta por riba da diagonal
            A%rowind(n) = row
            A%nzval(n) = values(colsindex - 1)
            A%rowind(n+1) = col
            A%nzval(n+1) = 1.0d0
        else if (col < row) then
            A%rowind(n) = col
            A%nzval(n) = 1.0d0
            A%rowind(n+1) = row
            A%nzval(n+1) = values(colsindex - 1)
        else
            print *, 'morse_id_row.f90: error =='
            stop 1
        endif
        n = n + 2
    endif
 enddo

 print *, 'tammax', min(nr,nc) + size(cols,1), 'tam', A%nnz, 'n=tam+1', n

end subroutine

! ^v non equivalentes... arriba I=o abaixo I+=o

subroutine morse_id_row_b(A, nr, nc, row, cols, values)
implicit none
type(sparse_real), intent(inout) :: A
integer, intent(in) :: nr
integer, intent(in) :: nc
integer, intent(in) :: row
integer, dimension(:), intent(in) :: cols
real(real64), dimension(:), intent(in) :: values
type(sparse_real) :: I
type(sparse_real) :: R

 call sparse_identity(I, nr, nc, 1.0d0)
 call sparse_row(R, nr, nc, row, cols, values)
 call sparse_sum(I, R, A)

! print *, 'I'
! call sparse_print(I)
! print *, 'R'
! call sparse_print(R)

 ! necesario ?
 call sparse_free(I)
 call sparse_free(R)

end subroutine


subroutine morse_id_rows(A, nr, nc, rows)
implicit none
type(sparse_real), intent(inout) :: A
integer, intent(in) :: nr
integer, intent(in) :: nc
type(rowstruct), dimension(:), allocatable, intent(in) :: rows ! doblemente ordenados
integer :: col
integer :: rowidx
integer :: rowarrays
integer :: tam
integer :: numadded
integer, dimension(:), allocatable :: nextindexes
logical :: diagonal_added
logical :: value_added

 call sparse_free(A)

 call sparse_build1(A, nr, nc) ! sets size(rows x cols) and allocates colptr

 rowarrays = size(rows,1)
 allocate(nextindexes(rowarrays))
 nextindexes = 1

 tam = 0
 do rowidx = 1, rowarrays
    tam = tam + rows(rowidx)%rowsize
 enddo
 tam = tam + min(nr,nc)
 ! tam >= number of nonzeros

 allocate(A%rowind(tam))
 allocate(A%nzval(tam))
 A%nnz = 0

 A%colptr(1) = 1
 do col = 1, nc
    numadded = 0
    diagonal_added = .false.
    value_added = .false.
    do rowidx = 1, rowarrays
        if (nextindexes(rowidx) <= rows(rowidx)%rowsize) then
            if (rows(rowidx)%indexes(nextindexes(rowidx)) == col) then
                if (.not. diagonal_added) then
                    if (col < rows(rowidx)%rownumber) then ! vaise engadir un valor meirande que a diagonal. engadir diagonal
                        A%rowind(A%nnz + 1) = col
                        A%nzval(A%nnz + 1) = 1.0d0
                        A%nnz = A%nnz + 1
                        numadded = numadded + 1
                        diagonal_added = .true.
                    elseif (col == rows(rowidx)%rownumber) then ! valor coincide en diagonal: descartar diagonal
                        diagonal_added = .true.
                    endif
                endif
                if (.not. value_added) then ! para evitar duplicados
                    A%rowind(A%nnz + 1) = rows(rowidx)%rownumber
                    A%nzval(A%nnz + 1) = rows(rowidx)%values(nextindexes(rowidx))
                    A%nnz = A%nnz + 1
                    numadded = numadded + 1
                    value_added = .true.
                else
                    ! converter en erro ?
                    ! warning: multiple rows with the same value
                    print *, 'morse_id_row.f90: warning: multiple rows with the same row value'
                endif
                nextindexes(rowidx) = nextindexes(rowidx) + 1
            endif
        endif
    enddo
    if (.not. diagonal_added .and. col <= nr) then ! chegouse ao final sen engadir diagonal. engadila
        A%rowind(A%nnz + 1) = col
        A%nzval(A%nnz + 1) = 1.0d0
        A%nnz = A%nnz + 1
        numadded = numadded + 1
        diagonal_added = .true.
    endif
    A%colptr(col+1) = A%colptr(col) + numadded
 enddo

 ! posible axustar tamanho

 deallocate(nextindexes)

end subroutine


end module morse_id_row_mod

