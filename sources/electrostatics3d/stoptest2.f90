!---------------------------------------------------------------------------
!  Purpose
!  =======
!---------------------------------------------------------------------------
!  Computes the stopping criterion 2.
!---------------------------------------------------------------------------
!  Arguments
!  =========
!
!  N       (input) INTEGER.
!          On entry, the dimension of the matrix.
!          Unchanged on exit.
!
!  INFO    (output) INTEGER
!          On exit, 1/0 depending on whether stopping criterion
!          was met or not.
!
!  BNRM2   (input/output) DOUBLE PRECISION.
!          On first time entry, will be -1.0.
!          On first time exit will contain norm2(B)
!          On all subsequent entry/exit's unchanged.
!
!  RESID   (output) DOUBLE PRECISION.
!          On exit, the computed stopping measure.
!
!  TOL     (input) DOUBLE PRECISION.
!          On input, the allowable convergence measure.
!
!  R       (input) DOUBLE PRECISION array, dimension N.
!          On entry, the residual.
!          Unchanged on exit.
!
!  B       (input) DOUBLE PRECISION array, dimension N.
!          On entry, right hand side vector B.
!          Unchanged on exit.
!
!  BLAS CALLS:   DNRM2
!---------------------------------------------------------------------------

subroutine stoptest2(n,r,b,bnrm2,resid,tol,info)

  integer,          intent(in)    :: n
  double precision, intent(in)    :: tol
  double precision, intent(in)    :: r(*),b(*)
  double precision, intent(inout) :: bnrm2
  double precision, intent(out)   :: resid
  integer,          intent(out)   :: info
  double precision, parameter     :: zero=0.0d0,one=1.0d0
  double precision, external      :: dnrm2

  if (info.eq.-1) then
     bnrm2 = dnrm2(n,b,1)
     if (bnrm2.eq.zero) bnrm2 = one
  endif

  resid = dnrm2(n,r,1)/bnrm2

  info = 0
  if(resid.le.tol) info = 1

  return
end
