module norma_axb_mod

use sparse_class

implicit none

contains

subroutine norma_axb(A, X, B)
type(sparse_complex), intent(in) :: A
complex(real64), dimension(A%nr), intent(in) :: X
complex(real64), dimension(A%nr), intent(in) :: B
type(sparse_complex) :: Xs
type(sparse_complex) :: Bs
type(sparse_complex) :: t1
type(sparse_complex) :: t2
complex(real64), dimension(A%nr,1) :: t3
real(real64) :: s, u, v, w
integer :: n, i

 n = A%nr
 call sparse_from_dense(Xs, reshape(X,[n,1]))
 call sparse_from_dense(Bs, reshape(B,[n,1]))
 call sparse_multiply(A, Xs, t1)
 call sparse_scale(t1, (-1.0d0,0.0d0))
 call sparse_sum(Bs, t1, t2)
 call sparse_to_dense(t2, t3)

 w = 0.0d0
 do i = 1, n
    u = real(real(t3(i,1)))
    v = real(aimag(t3(i,1)))
    w = w + u*u + v*v
 enddo
 w = sqrt(w)

 s = 0.0d0
 do i = 1, n
    u = real(real(X(i)))
    v = real(aimag(X(i)))
    s = s + u*u + v*v
 enddo
 s = sqrt(s)
 print *, 'norma2(B-AX)', w
 print *, 'norma2(X)', s
 print *, 'norma2(B-AX) / norma2(X)', w / s

end subroutine

end module
