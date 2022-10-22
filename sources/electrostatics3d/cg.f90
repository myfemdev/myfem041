!---------------------------------------------------------------------------
!  Purpose
!  =======
!
!  CG solves the linear system Ax = b using the
!  Conjugate Gradient iterative method with preconditioning.
!
!  Convergence test: ( norm( b - A*x ) / norm( b ) ) < TOL.
!  For other measures, see the above reference.
!  --Done in CGREVCOM.
!---------------------------------------------------------------------------
!  Arguments
!  =========
!
!  N       (input) INTEGER.
!          On entry, the dimension of the matrix.
!          Unchanged on exit.
!
!  B       (input) DOUBLE PRECISION array, dimension N.
!          On entry, right hand side vector B.
!          Unchanged on exit.
!
!  X       (input/output) DOUBLE PRECISION array, dimension N.
!          On input, the initial guess. This is commonly set to
!          the zero vector.
!          On exit, if INFO = 0, the iterated approximate solution.
!          Set by CGREVCOM.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension ( * ).
!          Workspace for residual, direction vector, etc.
!
!  LDW     (input) INTEGER
!          The leading dimension of the array WORK. LDW >= max(1,N).
!
!  ITER    (input/output) INTEGER
!          On input, the maximum iterations to be performed.
!          On output, actual number of iterations performed.
!          Set by CGREVCOM.
!
!  RESID   (input/output) DOUBLE PRECISION
!          On input, the allowable convergence measure for
!          norm( b - A*x ) / norm( b ).
!          On output, the final value of this measure.
!          Set by CGREVCOM.
!
!  matvec  (external subroutine)
!          The user must provide a subroutine to perform the
!          matrix-vector product
!
!               y := alpha*A*x + beta*y,
!
!          where alpha and beta are scalars, x and y are vectors,
!          and A is a matrix. Vector x must remain unchanged.
!          The solution is over-written on vector y.
!
!          The call is:
!
!             CALL matvec( ALPHA, X, BETA, Y , xmat,n,mua1,mua2)
!
!          The matrix is passed into the routine in a common block.
!
!  psolve  (external subroutine)
!          The user must provide a subroutine to perform the
!          preconditioner solve routine for the linear system
!
!               M*x = b,
!
!          where x and b are vectors, and M a matrix. Vector b must 
!          remain unchanged.
!          The solution is over-written on vector x.
!
!          The call is:
!
!             CALL psolve( X, B , xmatp,n,mua1,mua2)
!
!          The preconditioner is passed into the routine in a common block.
!---------------------------------------------------------------------------
!   INFO    (output) INTEGER
!           Set by CGREVCOM()
!   ============================================================
! 
!      ..
!      .. Local Scalars ..
! This variable used to communicate requests between CG() and CGREVCOM()
! CG -> CGREVCOM: 1 = init, 
!                 2 = use saved state to resume flow.
! CGREVCOM -> CG: -1 = done, return to main, 
!                  1 = matvec using SCLR1/2, NDX1/2 
!                  2 = solve using NDX1/2
!---------------------------------------------------------------------------

subroutine cg(n,b,x,work,ldw,iter,resid,info,xmat,xmatp,mua1,mua2)

  implicit none
  integer,          intent(in)    :: n,ldw
  integer,          intent(in)    :: mua1(*),mua2(*)
  double precision, intent(in)    :: b(*),xmat(*),xmatp(*)  
  integer,          intent(inout) :: iter
  double precision, intent(inout) :: x(*)
  double precision, intent(inout) :: resid
  double precision, intent(out)   :: work(*)
  integer,          intent(out)   :: info
  double precision, external      :: dnrm2
  integer                         :: ijob,ndx1,ndx2
  logical                         :: ftflg
  double precision                :: sclr1,sclr2
  double precision                :: tol,bnrm2
  external                        :: cgrevcom, stoptest2

  info = 0
  if (n.lt.0) then
     info = -1
  else if (ldw.lt.max(1,n)) then
     info = -2
  else if (iter.le.0) then
     info = -3
  endif
  if (info.ne.0) return

  call dcopy(n,b,1,work(1),1)
  call matvec(1d0,x,-1d0,work(1),xmat,n,mua1,mua2)
  if (dnrm2(n,work(1),1).lt.resid) then
     info=0
     iter=0
     return
  endif

! STOP TEST MAY NEED SOME INDEXING INFO FROM REVCOM
! USE THE INIT CALL TO SEND THE REQUEST ACROSS. REVCOM
! WILL NOTE THESE REQUESTS, AND EVERYTIME IT ASKS FOR
! STOP TEST TO BE DONE, IT WILL PROVIDE THE INDEXING INFO.
! 1 == R; 2 == Z; 3 == P; 4 == Q; -1 == ignore; any other == error

  ndx1 = 1
  ndx2 = -1
  tol = resid
  ftflg = .true.

! FIRST TIME CALL ALWAYS INIT.
  ijob = 1

  bucle: do

     call cgrevcom(n,b,x,work,ldw,iter,resid,info,ndx1,ndx2,sclr1,sclr2,ijob)

! ON A RETURN FROM CGREVCOM() WE USE THE TABLE (CGREVCOM -> CG)
! TO FIGURE OUT WHAT IS REQD.
     if (ijob.eq.-1) then
        exit bucle
     elseif (ijob.eq.1) then
        call matvec(sclr1,work(ndx1),sclr2,work(ndx2),xmat,n,mua1,mua2)
     elseif (ijob.eq.2) then
        call psolve(work(ndx1),work(ndx2),xmatp,n,mua1,mua2)
     elseif (ijob.eq.3) then
        call matvec(sclr1,x,sclr2,work(ndx2),xmat,n,mua1,mua2)
     elseif (ijob.eq.4) then
! DO STOPPING TEST 2
! IF FIRST TIME, SET INFO SO THAT BNRM2 IS COMPUTED.
        if(ftflg) info = -1
        call stoptest2(n,work(ndx1),b,bnrm2,resid,tol,info)
        ftflg = .false.
     endif
     ijob = 2

  end do bucle

  return
end
