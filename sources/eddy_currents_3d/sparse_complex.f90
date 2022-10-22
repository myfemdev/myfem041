module sparse_complex_class

!-----------------------------------------------------------------------
! Module for sparse matrix management
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Francisco Pena, fran.pena@usc.es
! Author: Rodrigo Valiña Gutiérrez, rodrigo.valina@usc.es
! Last update: 16/09/2010
!
! PUBLIC TYPE-BOUND PROCEDURES:
!   build: allocate memory and build morse indices
!   set: set values in matrix
!   add: add values to matrix
!   get: get values from matrix
!-----------------------------------------------------------------------

use sparse_aux
use vector

implicit none

!Constants
!integer, parameter, private :: real64 = selected_real_kind(15, 307) !double precision
real(real64), parameter, private :: minmul = 1.0d-14

!Types
type :: sparse_complex
integer :: nr
integer :: nc
integer :: nnz
complex(real64), dimension(:), allocatable :: nzval ! nnz ! complex values
integer, dimension(:), allocatable :: rowind ! nnz
integer, dimension(:), allocatable :: colptr ! nc + 1
end type

type :: col_complex
complex(real64), dimension(:), allocatable :: values
integer, dimension(:), allocatable :: rows
integer :: num
end type

!Interfaces
!interface build; module procedure build_prv; end interface

!private procedures

contains


!---------------------------
! free memory and clear data
!---------------------------
subroutine sparse_complex_free(this)
implicit none
type(sparse_complex), intent(inout) :: this
!print *, 'free-complex'
this%nr = 0
this%nc = 0
this%nnz = 0
if (allocated(this%nzval)) deallocate(this%nzval)
if (allocated(this%rowind)) deallocate(this%rowind)
if (allocated(this%colptr)) deallocate(this%colptr)
end subroutine


!----------
! copy data
!----------
subroutine sparse_complex_copy(this, other)
implicit none
type(sparse_complex), intent(in) :: this
type(sparse_complex), intent(inout) :: other
 call sparse_complex_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = this%nzval(1:this%nnz)
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


!-----------
! print data
!-----------
subroutine sparse_complex_print(this)
implicit none
type(sparse_complex), intent(in) :: this
print *, 'nr', this%nr, 'nc', this%nc, 'nnz', this%nnz
print *, 'nzval'
if (this%nnz > 0) print *, this%nzval(1:this%nnz)
print *, 'rowind'
if (this%nnz > 0) print *, this%rowind(1:this%nnz)
print *, 'colptr'
if (this%nc >= 0) print *, this%colptr(1:this%nc + 1)
end subroutine


!----------------
! print some data
!----------------
subroutine sparse_complex_print_s(this)
implicit none
type(sparse_complex), intent(in) :: this
integer a,b,c
logical al, bl, cl
print *, 'nr', this%nr, 'nc', this%nc, 'nnz', this%nnz
a = 0
b = 0
c = 0
al = allocated(this%nzval)
bl = allocated(this%rowind)
cl = allocated(this%colptr)
if (al) a = size(this%nzval,1)
if (bl) b = size(this%rowind,1)
if (cl) c = size(this%colptr,1)
print *, 'nzval', al, a, 'rowind', bl, b, 'colptr', cl, c
end subroutine


!-------------------------------------
! compare two matrices. not exhaustive
!-------------------------------------
logical function sparse_complex_equal(A, B)
implicit none
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
integer :: i

 sparse_complex_equal = .false.

 if (A%nr /= B%nr) return
 if (A%nc /= B%nc) return
 if (A%nnz /= B%nnz) return
 if (A%nnz > 0) then
    if (.not. allocated(A%nzval) .or. .not. allocated(B%nzval)) return
    if (.not. allocated(A%rowind) .or. .not. allocated(B%rowind)) return
    do i = 1, A%nnz
        if (A%nzval(i) /= B%nzval(i)) return
        if (A%rowind(i) /= B%rowind(i)) return
    enddo
 endif
 if (.not. allocated(A%colptr) .or. .not. allocated(B%colptr)) return
 do i = 1, A%nc + 1
     if (A%colptr(i) /= B%colptr(i)) return
 enddo

 sparse_complex_equal = .true.

end function


!----------------
! allocate memory
!----------------
subroutine sparse_complex_build(this, rows, cols, nnz)
implicit none
type(sparse_complex), intent(inout) :: this !this matrix
integer, intent(in) :: rows
integer, intent(in) :: cols
integer, intent(in) :: nnz
 print *, 'sparse_complex_build rows', rows, 'cols', cols, 'nnz', nnz
 call sparse_complex_free(this)
 this%nr = rows
 this%nc = cols
 this%nnz = nnz
 allocate(this%nzval(this%nnz))
 allocate(this%rowind(this%nnz))
 allocate(this%colptr(this%nc+1))
end subroutine


!----------------------
! allocate memory (1/2)
!----------------------
subroutine sparse_complex_build1(this, rows, cols)
implicit none
type(sparse_complex), intent(inout) :: this !this matrix
integer, intent(in) :: rows
integer, intent(in) :: cols
 print *, 'sparse_complex_build1 rows', rows, 'cols', cols
 if (allocated(this%colptr)) deallocate(this%colptr)
 this%nr = rows
 this%nc = cols
 allocate(this%colptr(this%nc+1))
end subroutine


!----------------------
! allocate memory (2/2)
!----------------------
subroutine sparse_complex_build2(this, nnz)
implicit none
type(sparse_complex), intent(inout) :: this !this matrix
integer, intent(in) :: nnz
 print *, 'sparse_complex_build2 nnz', nnz
 if (allocated(this%nzval)) deallocate(this%nzval)
 if (allocated(this%rowind)) deallocate(this%rowind)
 this%nnz = nnz
 allocate(this%nzval(this%nnz))
 allocate(this%rowind(this%nnz))
end subroutine


!--------------------------------
! collapse arrays to minimum size
!--------------------------------
subroutine sparse_complex_collapse(this)
implicit none
type(sparse_complex), intent(inout) :: this
complex(real64), dimension(:), allocatable :: temp1
integer, dimension(:), allocatable :: temp2
 if (allocated(this%nzval)) then
    if (size(this%nzval)>this%nnz) then
        allocate(temp1(this%nnz))
        temp1 = this%nzval(1:this%nnz)
        deallocate(this%nzval)
        call move_alloc(temp1, this%nzval)
    endif
 endif
 if (allocated(this%rowind)) then
    if (size(this%rowind)>this%nnz) then
        allocate(temp2(this%nnz))
        temp2 = this%rowind(1:this%nnz)
        deallocate(this%rowind)
        call move_alloc(temp2, this%rowind)
    endif
 endif
 if (allocated(this%colptr)) then
    if (size(this%colptr)>this%nc+1) then
        allocate(temp2(this%nc+1))
        temp2 = this%colptr(1:this%nc+1)
        deallocate(this%colptr)
        call move_alloc(temp2, this%colptr)
    endif
 endif
end subroutine


!-------------------
! convert from dense
!-------------------
subroutine sparse_complex_from_dense(this, dense)
implicit none
type(sparse_complex), intent(inout) :: this
complex(real64), dimension(:,:), intent(in) :: dense
integer :: counter, row, col

 call sparse_complex_free(this)
 call sparse_complex_build1(this, size(dense,1), size(dense,2))
 this%nnz = 0
 this%colptr(1) = 1
 do col = 1, size(dense,2)
    counter = 0
    do row = 1, size(dense,1)
        if (dense(row,col) /= (0.0d0,0.0d0)) then
            call vector_append(this%nnz, this%nzval, dense(row,col))
            call vector_append(this%nnz, this%rowind, row)
            this%nnz = this%nnz + 1
            counter = counter + 1
        endif
    enddo
    this%colptr(col+1) = this%colptr(col) + counter
 enddo

end subroutine


!-----------------
! convert to dense
!-----------------
subroutine sparse_complex_to_dense(this, dense)
implicit none
type(sparse_complex), intent(in) :: this
complex(real64), dimension(:,:), intent(inout) :: dense
integer :: i, col, ini, fin

 if (size(dense,1) /= this%nr .or. size(dense,2) /= this%nc)&
    call sperror('sparse_complex_to_dense: sizes do not match')

 dense = (0.0d0,0.0d0)
 do col = 1, this%nc
    ini = this%colptr(col)
    fin = this%colptr(col+1) - 1
    do i = ini, fin
        dense(this%rowind(i), col) = this%nzval(i)
    enddo
 enddo

end subroutine


!---------------------------
! remove 1 col from a matrix
!---------------------------
subroutine sparse_complex_remove_col(this, col)
implicit none
type(sparse_complex),   intent(inout) :: this !this matrix
integer,        intent(in)    :: col !col to remove
integer :: a
integer :: b
integer :: num
integer :: i

 a = this%colptr(col)
 b = this%colptr(col+1)
 num = b - a

 ! adjust colptr
 do i = col, this%nc
    this%colptr(i) = this%colptr(i+1) - num
 enddo

 ! displace rowind and nzval
 if (num > 0) then ! if there were numbers in that column
     this%rowind(a:this%nnz-num) = this%rowind(b:this%nnz)
     this%nzval(a:this%nnz-num) = this%nzval(b:this%nnz)
 endif

 this%nnz = this%nnz - num
 this%nc = this%nc - 1

 ! call sparse_complex_collapse(this) ! optionally call to free unused memory

end subroutine


!----------------------------------------------
! remove cols from a matrix. cols sorted or not
!----------------------------------------------
subroutine sparse_complex_remove_cols(this, cols)
implicit none
type(sparse_complex),         intent(inout) :: this !this matrix
integer, dimension(:),        intent(in)    :: cols !cols to remove
integer, dimension(size(cols,1)) :: colscopy
integer :: i
integer :: j
integer :: col
colscopy = cols

print *, 'sparse_complex_remove_cols', size(cols,1), 'of', this%nc

 do i = size(colscopy,1), 1, -1
    col = colscopy(i)
    call sparse_complex_remove_col(this, col)
    do j = 1, i - 1
        if (colscopy(j) > col) then
            colscopy(j) = colscopy(j) - 1
        else if (colscopy(j) == col) then
            call sperror('sparse_complex_remove_cols: duplicated column')
        endif
    enddo
 enddo

print *, 'sparse_complex_remove_cols', ' now', this%nc

end subroutine


!--------------------------
! remove rows from a matrix
!--------------------------
subroutine sparse_complex_remove_rows(this, rows)
implicit none
type(sparse_complex),         intent(inout) :: this !this matrix
integer, dimension(:),        intent(in)    :: rows !rows to remove

print *, 'sparse_complex_remove_rows: no code !', this%nnz, size(rows)

end subroutine


!-----------------------------------
! remove rows and cols from a matrix
!-----------------------------------
subroutine sparse_complex_remove(this, rows, cols)
implicit none
type(sparse_complex),         intent(inout) :: this !this matrix
integer, dimension(:),        intent(in)    :: rows !rows to remove
integer, dimension(:),        intent(in)    :: cols !cols to remove

print *, 'sparse_complex_remove: no code !', this%nnz, size(rows), size(cols)

end subroutine


!---------------------------------
! multiply all values by a complex
!---------------------------------
subroutine sparse_complex_scale(this, value)
implicit none
type(sparse_complex),            intent(inout) :: this !this matrix
complex(real64),                 intent(in)    :: value !factor
this%nzval(1:this%nnz) = this%nzval(1:this%nnz) * value
end subroutine



!------------------------------------------
! multiply all values in a row by a complex
!------------------------------------------
subroutine sparse_complex_scale_row(this, row, value)
implicit none
type(sparse_complex),         intent(inout) :: this !this matrix
integer,                      intent(in)    :: row !row to scale
complex(real64),              intent(in)    :: value !factor
integer :: x
 do x = 1, this%nnz
    if (this%rowind(x) == row) then
        this%nzval(x) = this%nzval(x) * value
    endif
 enddo
end subroutine



!----------------------------------------------------------
! set: set values in matrix. cols unordered. rows unordered
!----------------------------------------------------------
subroutine sparse_complex_set(this, m, rows, cols)
implicit none
type(sparse_complex),            intent(inout) :: this !this matrix
complex(real64), dimension(:,:), intent(in)    :: m    !matrix to store
integer, dimension(:), intent(in)    :: rows !rows where to store m
integer, dimension(:), intent(in)    :: cols !cols where to store m
integer :: ri, ci
integer :: ini, fin
integer :: row, col
integer :: res

 if (size(m,1) /= size(rows,1) .or. size(m,2) /= size(cols,1)) &
    call sperror('sparse_complex_set: sizes do not match')

 ! optimizable? sort rows and do a linear search?

 do ci = 1, size(cols,1)
    col = cols(ci)
    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    do ri = 1, size(rows,1)
        row = rows(ri)
        res = BinarySearch(this%rowind, row, ini, fin)
        if (res > 0) then
            this%nzval(res) = m(ri,ci)
        else
            write(0,*) 'element:', ri, ',', ci, 'of', size(rows), ',', size(cols), '->', row, ',', col
            call sperror('sparse_complex_set: element not found')
        endif
    enddo
 enddo

end subroutine


!----------------------------------------------------------
! add: add values to matrix. cols unordered. rows unordered
!----------------------------------------------------------
subroutine sparse_complex_add(this, m, rows, cols)
implicit none
type(sparse_complex),            intent(inout) :: this !this matrix
complex(real64), dimension(:,:), intent(in)    :: m    !matrix to store
integer, dimension(:), intent(in)    :: rows !rows where to store m
integer, dimension(:), intent(in)    :: cols !cols where to store m
integer :: ri, ci
integer :: ini, fin
integer :: row, col
integer :: res

 if (size(m,1) /= size(rows,1) .or. size(m,2) /= size(cols,1)) &
    call sperror('sparse_complex_add: sizes do not match')

 ! optimizable? sort rows and do a linear search?

 do ci = 1, size(cols,1)
    col = cols(ci)
    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    do ri = 1, size(rows,1)
        row = rows(ri)
        res = BinarySearch(this%rowind, row, ini, fin)
        if (res > 0) then
            this%nzval(res) = this%nzval(res) + m(ri,ci)
        else
            write(0,*) 'element:', ri, ',', ci, 'of', size(rows), ',', size(cols), '->', row, ',', col
            call sperror('sparse_complex_add: element not found')
        endif
    enddo
 enddo

end subroutine



!------------------------------------------------
! get matrix data. cols unordered. rows unordered
!------------------------------------------------
function sparse_complex_get(this, rows, cols) result(r)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, dimension(:), intent(in) :: rows !rows from to get m
integer, dimension(:), intent(in) :: cols !cols from to get m
complex(real64), dimension(:,:), allocatable :: r !matrix to get
integer :: ios
integer :: ri, ci
integer :: ini, fin
integer :: row, col
integer :: res

 if (allocated(r) .and. (size(r,1) /= size(rows,1) .or. size(r,2) /= size(cols,1))) deallocate(r)
 if (.not. allocated(r)) then
    allocate(r(size(rows,1), size(cols,1)), stat = ios)
    if (ios /= 0) call sperror('sparse_complex_get, unable to allocate variable')
 endif

 ! optimizable? sort rows and do a linear search?

 do ci = 1, size(cols,1)
    col = cols(ci)
    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    do ri = 1, size(rows,1)
        row = rows(ri)
        res = BinarySearch(this%rowind, row, ini, fin)
        if (res > 0) then
            r(ri,ci) = this%nzval(res)
        else
            r(ri,ci) = (0.0d0,0.0d0)
        endif
    enddo
 enddo

end function


!-----------------
! get element data
!-----------------
function sparse_complex_get_element(this, row, col) result(r)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, intent(in) :: row !row from to get m
integer, intent(in) :: col !col from to get m
complex(real64) :: r !element to get
integer :: res

    res = BinarySearch(this%rowind, row, this%colptr(col), this%colptr(col + 1) - 1)
    if (res > 0) then
        r = this%nzval(res)
    else
        r = (0.0d0,0.0d0)
    endif

end function


!-------------------------
! get column data (sparse)
!-------------------------
subroutine sparse_complex_get_col_sparse(this, col, rows, values)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, intent(in) :: col !col from to get
integer, dimension(:), allocatable, intent(out) :: rows
complex(real64), dimension(:), allocatable, intent(out) :: values
integer :: ini, fin

    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    if (fin<ini) then
        allocate(rows(0))
        allocate(values(0))
        return
    endif
    allocate(rows(fin-ini+1))
    allocate(values(fin-ini+1))
    rows = this%rowind(ini:fin)
    values = this%nzval(ini:fin)

end subroutine


!------------------------
! get column data (dense)
!------------------------
subroutine sparse_complex_get_col(this, col, values)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, intent(in) :: col !col from to get
complex(real64), dimension(:), allocatable, intent(out) :: values
integer :: ini, fin, i, ios

 if (allocated(values) .and. (size(values,1) /= this%nr)) deallocate(values)
 if (.not. allocated(values)) then
    allocate(values(this%nr), stat = ios)
    if (ios /= 0) call sperror('sparse_complex_get_col, unable to allocate variable')
 endif

 values = (0.0d0,0.0d0)

 ini = this%colptr(col)
 fin = this%colptr(col + 1) - 1
 do i = ini, fin
    values(this%rowind(i)) = this%nzval(i)
 enddo

end subroutine


!----------------------
! get row data (sparse)
!----------------------
subroutine sparse_complex_get_row_sparse(this, row, cols, values)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, intent(in) :: row !rows from to get m
integer, dimension(:), allocatable, intent(out) :: cols !vector to get
complex(real64), dimension(:), allocatable, intent(out) :: values !vector to get
integer :: nz
integer :: ini, fin
integer :: col
integer :: res

 nz = 0

 do col = 1, this%nc
    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    res = BinarySearch(this%rowind, row, ini, fin)
    if (res > 0) then
        call vector_append(nz, cols, col)
        call vector_append(nz, values, this%nzval(res))
        nz = nz + 1
    endif
 enddo

 call vector_adjust(nz, cols) ! adjusts size to fit data
 call vector_adjust(nz, values) ! adjusts size to fit data

end subroutine


!---------------------
! get row data (dense)
!---------------------
subroutine sparse_complex_get_row(this, row, values)
implicit none
type(sparse_complex), intent(in) :: this !this matrix
integer, intent(in) :: row !rows from to get m
complex(real64), dimension(:), allocatable, intent(out) :: values !vector to get
integer :: ios
integer :: ini, fin
integer :: col
integer :: res

 if (allocated(values) .and. (size(values,1) /= this%nc)) deallocate(values)
 if (.not. allocated(values)) then
    allocate(values(this%nc), stat = ios)
    if (ios /= 0) call sperror('sparse_complex_get_row, unable to allocate variable')
 endif

 do col = 1, this%nc
    ini = this%colptr(col)
    fin = this%colptr(col + 1) - 1
    res = BinarySearch(this%rowind, row, ini, fin)
    if (res > 0) then
        values(col) = this%nzval(res)
    else
        values(col) = (0.0d0,0.0d0)
    endif
 enddo

end subroutine


!-----------------------------
! add two matrices: C = A + B
!-----------------------------
! needs ordered %rowind
subroutine sparse_complex_sum(A,B,C)
implicit none
type(sparse_complex),                 intent(in)    :: A
type(sparse_complex),                 intent(in)    :: B
type(sparse_complex),                 intent(inout) :: C
integer :: i, nnzmax, added
integer :: sa, sb, ea, eb
integer :: act, row
complex(real64) :: value
 if (A%nr /= B%nr .or. A%nc /= B%nc) call sperror('sparse_complex_sum: sizes do not match')
 call sparse_complex_free(C)
 C%nr = A%nr
 C%nc = A%nc
 C%nnz = 0
 nnzmax = A%nnz + B%nnz ! maximo posible
 allocate(C%nzval(nnzmax),C%rowind(nnzmax)) ! maximo posible
 allocate(C%colptr(C%nc+1))
 C%colptr(1) = 1
 do i = 1, C%nc
    sa = A%colptr(i)
    ea = A%colptr(i+1)
    sb = B%colptr(i)
    eb = B%colptr(i+1)
    added = 0
    do while (sa < ea .or. sb < eb)

        ! which pointers are valid?
        if (sb >= eb) then
            act = 1 ! only A valid
        else if (sa >= ea) then
            act = 2 ! only B valid
        else
            act = 0 ! both valid
        endif

        ! if both are valid, which one points to the first value?
        if (act == 0) then ! second phase for both valid
            if (A%rowind(sa) == B%rowind(sb)) then
                act = 0 ! sum
            else if (A%rowind(sa) < B%rowind(sb)) then
                act = 1 ! A points to less
            else if (A%rowind(sa) > B%rowind(sb)) then
                act = 2 ! B points to less
            endif
        endif

        if (act == 0) then ! both => add
            value = A%nzval(sa) + B%nzval(sb)
            row = A%rowind(sa)
            sa = sa + 1
            sb = sb + 1
        else if (act == 1) then ! A => store
            value = A%nzval(sa)
            row = A%rowind(sa)
            sa = sa + 1
        else if (act == 2) then ! B => store
            value = B%nzval(sb)
            row = B%rowind(sb)
            sb = sb + 1
        else ! impossible
            call sperror('sparse_complex_sum: unreachable code reached')
        endif

        C%nnz = C%nnz + 1
        C%nzval(C%nnz) = value
        C%rowind(C%nnz) = row
        added = added + 1
    enddo
    C%colptr(i+1) = C%colptr(i) + added
 enddo

 print *, 'sparse_complex_sum:' , A%nnz, '+', B%nnz, '->', C%nnz

 ! adjust C%nzval and C%rowind vectors to minimum size C%nzz
 ! call sparse_complex_collapse(C)

end subroutine


!-------------------
! multiply C = A * B
!-------------------
subroutine sparse_complex_multiply_b(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
complex(real64) :: val
integer :: colA
integer :: colB
integer :: rowA
!integer :: rowB
integer :: i
integer :: res
integer :: iniA
integer :: finA
integer :: iniB
integer :: finB
integer :: numrows

 if (A%nc /= B%nr) call sperror('sparse_complex_multiply: sizes do not match')

 call sparse_complex_free(C)
 call sparse_complex_build1(C, A%nr, B%nc)
 C%nnz = 0
 C%colptr(1) = 1

 do colB = 1, B%nc
    iniB = B%colptr(colB)
    finB = B%colptr(colB + 1) - 1
    numrows = 0
    do rowA = 1, A%nr
        ! A[rowA,k] * B[k,colB]
        val = (0.0d0,0.0d0)
        do i = iniB, finB
            colA = B%rowind(i) ! == rowB
            iniA = A%colptr(colA)
            finA = A%colptr(colA + 1) - 1
            res = BinarySearch(A%rowind, rowA, iniA, finA)
            if (res > 0) then
                val = val + A%nzval(res) * B%nzval(i)
            endif
        enddo
        ! no direct abs to avoid square roots
        if (abs(dreal(val)) > minmul .or. abs(dimag(val)) > minmul) then ! ou sempre ou != 0
            call vector_append(C%nnz, C%nzval, val)
            call vector_append(C%nnz, C%rowind, rowA)
            C%nnz = C%nnz + 1
            numrows = numrows + 1
        endif
    enddo
    C%colptr(colB + 1) = C%colptr(colB) + numrows
 enddo

end subroutine


!--------------------
! multiply C = A' * B
!--------------------
subroutine sparse_complex_multiply_at(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
complex(real64) :: val
integer :: colA
integer :: colB
integer :: iniA
integer :: finA
integer :: iniB
integer :: finB
integer :: pA
integer :: pB
integer :: numrows

 if (A%nr /= B%nr) call sperror('sparse_complex_multiply_t: sizes do not match')

 call sparse_complex_free(C)
 call sparse_complex_build1(C, A%nc, B%nc)
 C%nnz = 0
 C%colptr(1) = 1

 do colB = 1, B%nc
    iniB = B%colptr(colB)
    finB = B%colptr(colB + 1) - 1
    numrows = 0
    do colA = 1, A%nc
        iniA = A%colptr(colA)
        finA = A%colptr(colA + 1) - 1

        pA = iniA
        pB = iniB
        val = (0.0d0,0.0d0)

        do while (pA <= finA .and. pB <= finB) ! only A or only B => 0.0
            if (A%rowind(pA) < B%rowind(pB)) then
                pA = pA + 1
            else if (A%rowind(pA) > B%rowind(pB)) then
                pB = pB + 1
            else
                val = val + A%nzval(pA) * B%nzval(pB)
                pA = pA + 1
                pB = pB + 1
            endif
        enddo

        ! no direct abs to avoid square roots
        if (abs(dreal(val)) > minmul .or. abs(dimag(val)) > minmul) then ! ou sempre ou != 0
            call vector_append(C%nnz, C%nzval, val)
            call vector_append(C%nnz, C%rowind, colA)
            C%nnz = C%nnz + 1
            numrows = numrows + 1
        endif
    enddo
    C%colptr(colB + 1) = C%colptr(colB) + numrows
 enddo

end subroutine


!--------------------------
! multiply columns * column
!--------------------------
pure function sparse_complex_multiply_col(colB, A, B)
type(col_complex) :: sparse_complex_multiply_col
integer, intent(in) :: colB
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
integer :: iniA, iniB, finA, finB, pA, pB, colA
complex(real64) :: val

    sparse_complex_multiply_col%num = 0

    iniB = B%colptr(colB)
    finB = B%colptr(colB + 1) - 1
    do colA = 1, A%nc
        iniA = A%colptr(colA)
        finA = A%colptr(colA + 1) - 1

        pA = iniA
        pB = iniB
        val = (0.0d0,0.0d0)

        do while (pA <= finA .and. pB <= finB) ! only A or only B => 0.0
            if (A%rowind(pA) < B%rowind(pB)) then
                pA = pA + 1
            else if (A%rowind(pA) > B%rowind(pB)) then
                pB = pB + 1
            else
                val = val + A%nzval(pA) * B%nzval(pB)
                pA = pA + 1
                pB = pB + 1
            endif
        enddo

        ! no direct abs to avoid square roots
        if (abs(dreal(val)) > minmul .or. abs(dimag(val)) > minmul) then ! ou sempre ou != 0
            call pvector_append(sparse_complex_multiply_col%num, sparse_complex_multiply_col%values, val)
            call pvector_append(sparse_complex_multiply_col%num, sparse_complex_multiply_col%rows, colA)
            sparse_complex_multiply_col%num = sparse_complex_multiply_col%num + 1
        endif
    enddo

end function


!--------------------
! multiply C = A' * B
! tries to do it in parallel
!--------------------
subroutine sparse_complex_multiply_ct(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
integer :: colB
type(col_complex), dimension(:), allocatable :: cols

 if (A%nr /= B%nr) call sperror('sparse_complex_multiply_ct: sizes do not match')

 call sparse_complex_free(C)
 call sparse_complex_build1(C, A%nc, B%nc)
 C%nnz = 0
 C%colptr(1) = 1

 allocate(cols(B%nc))

! forall (colB=1:B%nc) cols(colB) = sparse_complex_multiply_col(colB, A, B)

!$omp parallel do
  do colB=1,B%nc
    cols(colB) = sparse_complex_multiply_col(colB, A, B)
  enddo
!$omp end parallel do

 do colB = 1, B%nc
    call vector_append_vector(C%nnz, C%nzval, cols(colB)%num, cols(colB)%values)
    call vector_append_vector(C%nnz, C%rowind, cols(colB)%num, cols(colB)%rows)
    C%nnz = C%nnz + cols(colB)%num
    C%colptr(colB + 1) = C%colptr(colB) + cols(colB)%num
    if (allocated(cols(colB)%values)) deallocate(cols(colB)%values)
    if (allocated(cols(colB)%rows)) deallocate(cols(colB)%rows)
 enddo

 deallocate(cols)

end subroutine


!---------------------
! multiply_a C = A * B
!---------------------
subroutine sparse_complex_multiply_a(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
type(sparse_complex) :: At

 if (A%nc /= B%nr) call sperror('sparse_complex_multiply_a: sizes do not match')

 call sparse_complex_transpose(A, At)
 call sparse_complex_multiply_at(At, B, C)
 call sparse_complex_free(At)

end subroutine


!-----------------------
! multiply_bt C = A' * B
!-----------------------
subroutine sparse_complex_multiply_bt(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
type(sparse_complex) :: At

 if (A%nr /= B%nr) call sperror('sparse_complex_multiply_bt: sizes do not match')

 call sparse_complex_transpose(A, At)
 call sparse_complex_multiply_b(At, B, C)
 call sparse_complex_free(At)

end subroutine


!---------------------
! multiply_c C = A * B
!---------------------
subroutine sparse_complex_multiply_c(A, B, C)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(in) :: B
type(sparse_complex), intent(inout) :: C
type(sparse_complex) :: At

 if (A%nc /= B%nr) call sperror('sparse_complex_multiply_c: sizes do not match')

 call sparse_complex_transpose(A, At)
 call sparse_complex_multiply_ct(At, B, C)
 call sparse_complex_free(At)

end subroutine


!------------------
! transpose: B = A'
!------------------
subroutine sparse_complex_transpose(A, B)
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(inout) :: B
integer, dimension(:), allocatable :: marker ! marker(i): numero de elementos nz en fila i de A
integer :: n
integer :: m
integer :: i
integer :: j
integer :: col
integer :: relpos

    ! A%nr <- n
    ! A%nc <- m ???
    ! B%nzval <- *at
    ! B%rowind <- *rowind
    ! B%colptr <- *colptr
    ! A%nzval <- *a
    ! A%rowind <- *colind
    ! A%colptr <- *rowptr

 call sparse_complex_build(B, A%nc, A%nr, A%nnz)

 n = A%nr
 m = A%nc

 allocate(marker(n))
 marker = 0

 ! /* Get counts of each column of A, and set up column pointers */

 do i = 1, m
    do j = A%colptr(i), A%colptr(i+1) - 1
        marker(A%rowind(j)) = marker(A%rowind(j)) + 1
    enddo
 enddo

 B%colptr(1) = 1
 
 do j = 1, n
    B%colptr(j+1) = B%colptr(j) + marker(j)
    marker(j) = B%colptr(j) ! marker acumula
 enddo

 ! /* Transfer the matrix into the compressed column storage. */

 do i = 1, m
    do j = A%colptr(i), A%colptr(i+1) - 1
        col = A%rowind(j)
        relpos = marker(col)
        B%rowind(relpos) = i
        B%nzval(relpos) = A%nzval(j)
        marker(col) = marker(col) + 1
    enddo
 enddo

 deallocate(marker)

end subroutine


!-------------------------------------
! build an identity sparse matrix (*v)
!-------------------------------------
subroutine sparse_complex_identity(A, rows, cols, v)
type(sparse_complex), intent(inout) :: A
integer, intent(in) :: rows
integer, intent(in) :: cols
complex(real64), intent(in) :: v
integer :: col
integer :: i

 call sparse_complex_build(A, rows, cols, min(rows,cols))

 A%colptr(1) = 1
 do col = 1, min(rows,cols)
    A%colptr(col + 1) = A%colptr(col) + 1
 enddo
 do col = min(rows,cols) + 1, cols
    A%colptr(col + 1) = A%colptr(col)
 enddo

 do i = 1, A%nnz
    A%rowind(i) = i
 enddo

 A%nzval = v

end subroutine


!---------------------------------------------------
! build a matrix with a row of nonzeros. ids ordered
!---------------------------------------------------
subroutine sparse_complex_row(A, rows, cols, row, ids, vals)
type(sparse_complex), intent(inout) :: A
integer, intent(in) :: rows
integer, intent(in) :: cols
integer, intent(in) :: row
integer, dimension(:), intent(in) :: ids
complex(real64), dimension(:), intent(in) :: vals
integer :: col
integer :: i

 if (cols<size(ids,1)) call sperror ('sparse_complex_row: ids array too big')
 if (size(ids,1)/=size(vals,1)) call sperror ('sparse_complex_row: sizes do not match')

 call sparse_complex_build(A, rows, cols, size(ids,1))

 i = 1
 A%colptr(1) = 1
 do col = 1, cols
    if (ids(i) == col) then
        A%nzval(i) = vals(i)
        i = i + 1
        A%colptr(col + 1) = A%colptr(col) + 1
    else
        A%colptr(col + 1) = A%colptr(col)
    endif
 enddo

 A%rowind = row

end subroutine


!------------------------------------------------------
! build a matrix with a column of nonzeros. ids ordered
!------------------------------------------------------

subroutine sparse_complex_col(A, rows, cols, col, ids, vals)
type(sparse_complex), intent(inout) :: A
integer, intent(in) :: rows
integer, intent(in) :: cols
integer, intent(in) :: col
integer, dimension(:), intent(in) :: ids
complex(real64), dimension(:), intent(in) :: vals
integer :: c

 if (rows<size(ids,1)) call sperror ('sparse_complex_col: ids array too big')
 if (size(ids,1)/=size(vals,1)) call sperror ('sparse_complex_col: sizes do not match')

 call sparse_complex_build(A, rows, cols, size(ids,1))

 A%colptr(1) = 1
 do c = 1, cols
    if (col == c) then
        A%rowind = ids
        A%nzval = vals
        A%colptr(c + 1) = A%colptr(c) + size(ids,1)
    else
        A%colptr(c + 1) = A%colptr(c)
    endif
 enddo

end subroutine


!------------------------------------------
! creates an sparse matrix with one nonzero
!------------------------------------------

subroutine sparse_complex_one(A, nr, nc, row, col, value)
implicit none
type(sparse_complex), intent(inout) :: A
integer, intent(in) :: nr
integer, intent(in) :: nc
integer, intent(in) :: row
integer, intent(in) :: col
complex(real64), intent(in) :: value

 ! sets size(nrx nc), allocates colptr, sets nnz, allocates rowind and nzval
 call sparse_complex_build(A, nr, nc, 1)

 A%nzval = value
 A%rowind = row
 A%colptr(1:col) = 1
 A%colptr(col+1:nc+1) = 2

end subroutine


!----------------------
! extra: prints to file
!----------------------

subroutine sparse_complex_print_to_file(A, file_name, unit_num, opt)
implicit none
type(sparse_complex), intent(in) :: A
character(len=*), intent(in) :: file_name
integer, intent(in) :: unit_num
integer, intent(in) :: opt
integer :: col
integer :: i

 open  (unit=unit_num, file=file_name, form='formatted', position='rewind')
 do col = 1 , A%nc
    do i = A%colptr(col), A%colptr(col+1) - 1
        if (opt == 0 .or. A%nzval(i) /= 0.0) then
            write (unit=unit_num, fmt=*) A%rowind(i), col, dreal(A%nzval(i)), dimag(A%nzval(i))
        endif
    enddo
 enddo
 close(unit_num)

end subroutine


!------------------------------------------------------------------
! convert to upper triangular with explicit diagonal (even if zero)
!------------------------------------------------------------------

subroutine sparse_complex_upper_triangular_diagonal(A, B)
implicit none
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(inout) :: B
integer :: nnz
integer :: colz ! numero de nonzeros da columna
integer :: ini
integer :: fin
integer :: pos
integer :: row
integer :: col
complex(real64) :: val
logical :: diag

 call sparse_complex_free(B)
 call sparse_complex_build(B, A%nr, A%nc, A%nnz + min(A%nr, A%nc))

 nnz = 0
 B%colptr(1) = 1

 do col = 1, A%nc
   colz = 0
   diag = .false.
   ini = A%colptr(col)
   fin = A%colptr(col + 1) - 1
   do pos = ini, fin
     row = A%rowind(pos)
     val = A%nzval(pos)
     if (row <= col) then
       if (row == col) then
           diag = .true.
       endif
       B%nzval(nnz + 1) = val
       B%rowind(nnz + 1) = row
       nnz = nnz + 1
       colz = colz + 1
     endif
   enddo
   if (.not. diag .and. col<=A%nr) then ! se non houbo diagnonal, engadea
       B%nzval(nnz + 1) = (0.0d0,0.0d0)
       B%rowind(nnz + 1) = col
       nnz = nnz + 1
       colz = colz + 1
       diag = .true.
   endif
   B%colptr(col + 1) = B%colptr(col) + colz
 enddo

 B%nnz = nnz ! probablemente queda espazo sen usar ao fin de B%nzval e B%rowind

end subroutine


!------------------------------------------------
! convert to lower triangular. no forced diagonal
!------------------------------------------------

subroutine sparse_complex_lower_triangular(A, B)
implicit none
type(sparse_complex), intent(in) :: A
type(sparse_complex), intent(inout) :: B
integer :: nnz
integer :: colz ! numero de nonzeros da columna
integer :: ini
integer :: fin
integer :: pos
integer :: row
integer :: col
complex(real64) :: val

 call sparse_complex_free(B)
 call sparse_complex_build(B, A%nr, A%nc, A%nnz)

 nnz = 0
 B%colptr(1) = 1

 do col = 1, A%nc
   colz = 0
   ini = A%colptr(col)
   fin = A%colptr(col + 1) - 1
   do pos = ini, fin
     row = A%rowind(pos)
     val = A%nzval(pos)
     if (row >= col) then
       B%nzval(nnz + 1) = val
       B%rowind(nnz + 1) = row
       nnz = nnz + 1
       colz = colz + 1
     endif
   enddo
   B%colptr(col + 1) = B%colptr(col) + colz
 enddo

 B%nnz = nnz ! probablemente queda espazo sen usar ao fin de B%nzval e B%rowind

end subroutine


!-----------------------------------------------
! test whether the matrix conforms to some rules
!-----------------------------------------------

logical function sparse_complex_test(A)
implicit none
type(sparse_complex), intent(in) :: A
integer :: col, ini, fin, pos

 sparse_complex_test = .false.

 if (A%colptr(1) /= 1) return
 if (A%colptr(A%nc+1) - 1 /= A%nnz) return

 do col = 1, A%nc
    ini = A%colptr(col)
    fin = A%colptr(col + 1) - 1
    if (ini > fin + 1) return
    do pos = ini, fin
        if (A%rowind(pos) < 1 .or. A%rowind(pos) > A%nr) return ! fora de limites
        if (pos /= ini) then
            if (A%rowind(pos-1) >= A%rowind(pos)) return ! non incremental
        endif
    enddo
 enddo

 sparse_complex_test = .true.

end function


!--------------------------------------------------------------------------------------
! returns the 2-norm of the difference of the two parts of the matrix (upper and lower)
!--------------------------------------------------------------------------------------

real(real64) function sparse_complex_symmetric_error(A)
implicit none
type(sparse_complex), intent(in) :: A
complex(real64) :: w, diff
integer :: col, ini, fin, pos

 sparse_complex_symmetric_error = 0.0d0

 do col = 1, A%nc
    ini = A%colptr(col)
    fin = A%colptr(col + 1) - 1
    do pos = ini, fin
        w = sparse_complex_get_element(A, col, A%rowind(pos))
        diff = A%nzval(pos) - w
        sparse_complex_symmetric_error = sparse_complex_symmetric_error +&
            real(real(diff))*real(real(diff)) + real(aimag(diff))*real(aimag(diff))
    enddo
 enddo

 ! / 2.0 porque se recorreu toda a matriz e non a metade
 ! pero non é exacto en caso de patrón de nonzeros asimétrico
 sparse_complex_symmetric_error = sqrt ( sparse_complex_symmetric_error / 2.0 )

end function


!------------------------------------------
! from 1-based indexing to 0-based indexing
!------------------------------------------

subroutine sparse_complex_1_to_0(A)
implicit none
type(sparse_complex), intent(inout) :: A
integer :: col, i
 do col = 1, A%nc + 1
    A%colptr(col) = A%colptr(col) - 1
 enddo
 do i = 1, A%nnz
    A%rowind(i) = A%rowind(i) - 1
 enddo
end subroutine


!------------------------------------------
! from 0-based indexing to 1-based indexing
!------------------------------------------

subroutine sparse_complex_0_to_1(A)
implicit none
type(sparse_complex), intent(inout) :: A
integer :: col, i
 do col = 1, A%nc + 1
    A%colptr(col) = A%colptr(col) + 1
 enddo
 do i = 1, A%nnz
    A%rowind(i) = A%rowind(i) + 1
 enddo
end subroutine


end module sparse_complex_class

