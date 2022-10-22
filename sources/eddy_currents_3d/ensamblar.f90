module ensamblar_mod

use sparse_class
use globales

implicit none

contains


subroutine ensamblar(A, m11, m12, m21, m23, m32, tamatotal)
implicit none
type(sparse_complex), intent(inout) :: A
type(sparse_complex), intent(in) :: m11
type(sparse_real), intent(in) :: m12
type(sparse_real), intent(in) :: m21
type(sparse_real), intent(in) :: m23
type(sparse_real), intent(in) :: m32
integer, intent(in) :: tamatotal
integer :: cols1, cols2, cols3
integer :: rows1, rows2, rows3
integer :: col, ini, fin, i, tam

 print *, 'ensamblar ini'

 print*,  m11%nc , m12%nc , m23%nc , tamatotal
 print*,  m11%nr , m21%nr , m32%nr , tamatotal

 if (m11%nr /= m12%nr) call sperror('ensamblar: sizes do not match (1)')
 if (m21%nr /= m23%nr) call sperror('ensamblar: sizes do not match (2)')
 if (m11%nc /= m21%nc) call sperror('ensamblar: sizes do not match (3)') !
 if (m12%nc /= m32%nc) call sperror('ensamblar: sizes do not match (4)')
 if (m11%nc + m12%nc + m23%nc /= tamatotal) call sperror('ensamblar: sizes do not match (5)')
 if (m11%nr + m21%nr + m32%nr /= tamatotal) call sperror('ensamblar: sizes do not match (6)')

 cols1 = m11%nc
 cols2 = m12%nc
 cols3 = m23%nc
 rows1 = m11%nr
 rows2 = m21%nr
 rows3 = m32%nr

 call sparse_free(A)
 call sparse_build1(A, tamatotal, tamatotal)
 A%nnz = 0
 A%colptr(1) = 1

 print *, 'ensamblar 1'

 print *, 'rows', rows1, rows2, rows3, 'cols', cols1, cols2, cols3

 do col = 1, cols1
    tam = 0
    ini = m11%colptr(col)
    fin = m11%colptr(col + 1) - 1
    do i = ini, fin
        call vector_append(A%nnz, A%rowind, m11%rowind(i))
        call vector_append(A%nnz, A%nzval, m11%nzval(i))
        A%nnz = A%nnz + 1
        tam = tam + 1
    enddo
    ini = m21%colptr(col)
    fin = m21%colptr(col + 1) - 1
    do i = ini, fin
        call vector_append(A%nnz, A%rowind, rows1+m21%rowind(i))
        call vector_append(A%nnz, A%nzval, cmplx(m21%nzval(i),0.0d0,real64))
        A%nnz = A%nnz + 1
        tam = tam + 1
    enddo
    A%colptr(col + 1) = A%colptr(col) + tam
 enddo

 print *, 'ensamblar 2'

 do col = 1, cols2
    tam = 0
    ini = m12%colptr(col)
    fin = m12%colptr(col + 1) - 1
    do i = ini, fin
        call vector_append(A%nnz, A%rowind, m12%rowind(i))
        call vector_append(A%nnz, A%nzval, cmplx(m12%nzval(i),0.0d0,real64))
        A%nnz = A%nnz + 1
        tam = tam + 1
    enddo
    ini = m32%colptr(col)
    fin = m32%colptr(col + 1) - 1
    do i = ini, fin
        call vector_append(A%nnz, A%rowind, rows1+rows2+m32%rowind(i))
        call vector_append(A%nnz, A%nzval, cmplx(m32%nzval(i),0.0d0,real64))
        A%nnz = A%nnz + 1
        tam = tam + 1
    enddo
    A%colptr(cols1 + col + 1) = A%colptr(cols1 + col) + tam
 enddo

 print *, 'ensamblar 3'

 do col = 1, cols3
    tam = 0
    ini = m23%colptr(col)
    fin = m23%colptr(col + 1) - 1
    do i = ini, fin
        call vector_append(A%nnz, A%rowind, rows1+m23%rowind(i))
        call vector_append(A%nnz, A%nzval, cmplx(m23%nzval(i),0.0d0,real64))
        A%nnz = A%nnz + 1
        tam = tam + 1
    enddo
    A%colptr(cols1 + cols2 + col + 1) = A%colptr(cols1 + cols2 + col) + tam
 enddo

 print *, 'ensamblar fin'

end subroutine


subroutine ensamblar_b(b, m1, m2, tamatotal)
implicit none
complex(real64), dimension(:), allocatable, intent(inout) :: b
type(sparse_complex), intent(in) :: m1
type(sparse_complex), intent(in) :: m2
integer, intent(in) :: tamatotal
integer :: i, ini, fin

 if (m1%nc /= 1 .or. m2%nc /= 1 .or. m1%nr + m2%nr > tamatotal) call sperror('ensamblar_b: incorrect size')

 allocate(b(tamatotal))
 b = (0.0d0,0.0d0)

 ini = m1%colptr(1)
 fin = m1%colptr(2) - 1
 do i = ini, fin
    b(m1%rowind(i)) = m1%nzval(i)
 enddo

 ini = m2%colptr(1)
 fin = m2%colptr(2) - 1
 do i = ini, fin
    b(m1%nr + m2%rowind(i)) = m2%nzval(i)
 enddo

! debug
 print *, 'b size', m1%nr , '+', m2%nr, '+', tamatotal - m2%nr - m1%nr
 print *, 'b idx', 0 , '+', m1%nr, '+', m1%nr + m2%nr

end subroutine


end module
