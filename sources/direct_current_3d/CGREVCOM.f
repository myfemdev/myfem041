*
      SUBROUTINE CGREVCOM( N, B, X, WORK, LDW, ITER, RESID, INFO,
     $                     NDX1, NDX2, SCLR1, SCLR2, IJOB)
*
*  -- Iterative template routine --
*     Univ. of Tennessee and Oak Ridge National Laboratory
*     October 1, 1993
*     Details of this algorithm are described in "Templates for the 
*     Solution of Linear Systems: Building Blocks for Iterative 
*     Methods", Barrett, Berry, Chan, Demmel, Donato, Dongarra, 
*     Eijkhout, Pozo, Romine, and van der Vorst, SIAM Publications,
*     1993. (ftp netlib2.cs.utk.edu; cd linalg; get templates.ps).
*
*     .. Scalar Arguments ..
      INTEGER            N, LDW, ITER, INFO
      DOUBLE PRECISION   RESID
      INTEGER            NDX1, NDX2
      DOUBLE PRECISION   SCLR1, SCLR2
      INTEGER            IJOB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * ), B( * ),  WORK( LDW,* )
*
*     (output) for matvec and solve. These index into WORK[]
cModificado      INTEGER NDX1, NDX2
*     ..
*
*  Purpose
*  =======
*
*  CG solves the linear system Ax = b using the
*  Conjugate Gradient iterative method with preconditioning.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER.
*          On entry, the dimension of the matrix.
*          Unchanged on exit.
*
*  B       (input) DOUBLE PRECISION array, dimension N.
*          On entry, right hand side vector B.
*          Unchanged on exit.
*
*  X       (input/output) DOUBLE PRECISION array, dimension N.
*          On input, the initial guess. This is commonly set to
*          the zero vector.
*          On exit, if INFO = 0, the iterated approximate solution.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDW,4).
*          Workspace for residual, direction vector, etc.
*
*  LDW     (input) INTEGER
*          The leading dimension of the array WORK. LDW >= max(1,N).
*
*  ITER    (input/output) INTEGER
*          On input, the maximum iterations to be performed.
*          On output, actual number of iterations performed.
*
*  RESID   (input/output) DOUBLE PRECISION
*          On input, the allowable convergence measure for
*          norm( b - A*x ) / norm( b ).
*          On output, the final value of this measure.
*
*  INFO    (output) INTEGER
*
*          =  0: Successful exit. Iterated approximate solution returned.
*
*          >  0: Convergence to tolerance not achieved. This will be
*                set to the number of iterations performed.
*
*          <  0: Illegal input parameter.
*
*                   -1: matrix dimension N < 0
*                   -2: LDW < N
*                   -3: Maximum number of iterations ITER <= 0.
*                   -5: Erroneous NDX1/NDX2 in INIT call.
*                   -6: Erroneous RLBL.
*
*  NDX1    (input/output) INTEGER. 
*  NDX2    On entry in INIT call contain indices required by interface
*          level for stopping test.
*          All other times, used as output, to indicate indices into
*          WORK[] for the MATVEC, PSOLVE done by the interface level.
*
*  SCLR1   (output) DOUBLE PRECISION.
*  SCLR2   Used to pass the scalars used in MATVEC. Scalars are reqd because
*          original routines use dgemv.
*
*  IJOB    (input/output) INTEGER. 
*          Used to communicate job code between the two levels.
*
*  BLAS CALLS:   DAXPY, DCOPY, DDOT, DNRM2
*  ============================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER          MAXIT, R, Z, P, Q, NEED1, NEED2
      DOUBLE PRECISION ALPHA, BETA, RHO, RHO1, DDOT, DNRM2,
     $                 TOL
*
*     indicates where to resume from. Only valid when IJOB = 2!
      INTEGER RLBL
*
*     saving all.
      SAVE
*     ..
*     .. External Routines ..
      EXTERNAL         DAXPY, DCOPY, DDOT, DNRM2
*     ..
*     .. Executable Statements ..
*
*     Entry point, so test IJOB
      IF (IJOB .eq. 1) THEN
         GOTO 1
      ELSEIF (IJOB .eq. 2) THEN
*        here we do resumption handling
         IF (RLBL .eq. 2) GOTO 2
         IF (RLBL .eq. 3) GOTO 3
         IF (RLBL .eq. 4) GOTO 4
         IF (RLBL .eq. 5) GOTO 5
*        if neither of these, then error
         INFO = -6
         GOTO 20
      ENDIF
*
* init.
*****************
 1    CONTINUE
*****************
*
      INFO = 0
      MAXIT = ITER
      TOL = RESID
*
*     Alias workspace columns.
*
      R = 1
      Z = 2
      P = 3
      Q = 4
*
*     Check if caller will need indexing info.
*
      IF( NDX1.NE.-1 ) THEN
         IF( NDX1.EQ.1 ) THEN
            NEED1 = ((R - 1) * LDW) + 1
         ELSEIF( NDX1.EQ.2 ) THEN
            NEED1 = ((Z - 1) * LDW) + 1
         ELSEIF( NDX1.EQ.3 ) THEN
            NEED1 = ((P - 1) * LDW) + 1
         ELSEIF( NDX1.EQ.4 ) THEN
            NEED1 = ((Q - 1) * LDW) + 1
         ELSE
*           report error
            INFO = -5
            GO TO 20
         ENDIF
      ELSE
         NEED1 = NDX1
      ENDIF
*
      IF( NDX2.NE.-1 ) THEN
         IF( NDX2.EQ.1 ) THEN
            NEED2 = ((R - 1) * LDW) + 1
         ELSEIF( NDX2.EQ.2 ) THEN
            NEED2 = ((Z - 1) * LDW) + 1
         ELSEIF( NDX2.EQ.3 ) THEN
            NEED2 = ((P - 1) * LDW) + 1
         ELSEIF( NDX2.EQ.4 ) THEN
            NEED2 = ((Q - 1) * LDW) + 1
         ELSE
*           report error
            INFO = -5
            GO TO 20
         ENDIF
      ELSE
         NEED2 = NDX2
      ENDIF
*
*     Set initial residual.
*
      CALL DCOPY( N, B, 1, WORK(1,R), 1 )
      IF ( DNRM2( N, X, 1 ).NE.ZERO ) THEN

*********CALL MATVEC( -ONE, X, ONE, WORK(1,R) )
*
*        Set args for revcom return
         SCLR1 = -ONE
         SCLR2 = ONE
         NDX1 = -1
         NDX2 = ((R - 1) * LDW) + 1
*
*        Prepare for resumption & return
         RLBL = 2
         IJOB = 3
         RETURN
*
*****************
 2       CONTINUE
*****************
*
         IF ( DNRM2( N, WORK(1,R), 1 ).LT.TOL ) GO TO 30
      ENDIF
*
      ITER = 0
*
   10 CONTINUE
*
*        Perform Preconditioned Conjugate Gradient iteration.
*
         ITER = ITER + 1
*
*        Preconditioner Solve.
*
*********CALL PSOLVE( WORK(1,Z), WORK(1,R) )
*
         NDX1 = ((Z - 1) * LDW) + 1
         NDX2 = ((R - 1) * LDW) + 1
*        Prepare for return & return
         RLBL = 3
         IJOB = 2
         RETURN
*
*****************
 3       CONTINUE
*****************
*
         RHO = DDOT( N, WORK(1,R), 1, WORK(1,Z), 1 )
*
*        Compute direction vector P.
*
         IF ( ITER.GT.1 ) THEN
            BETA = RHO / RHO1
            CALL DAXPY( N, BETA, WORK(1,P), 1, WORK(1,Z), 1 )
*
            CALL DCOPY( N, WORK(1,Z), 1, WORK(1,P), 1 )
         ELSE
            CALL DCOPY( N, WORK(1,Z), 1, WORK(1,P), 1 )
         ENDIF
*
*        Compute scalar ALPHA (save A*P to Q).
*
*********CALL MATVEC( ONE, WORK(1,P), ZERO, WORK(1,Q) )
*
         NDX1 = ((P - 1) * LDW) + 1
         NDX2 = ((Q - 1) * LDW) + 1
*        Prepare for return & return
         SCLR1 = ONE
         SCLR2 = ZERO
         RLBL = 4
         IJOB = 1
         RETURN
*
*****************
 4       CONTINUE
*****************
*
         ALPHA =  RHO /DDOT( N, WORK(1,P), 1, WORK(1,Q), 1 )
*
*        Compute current solution vector X.
*
         CALL DAXPY( N, ALPHA, WORK(1,P), 1, X, 1 )
*
*        Compute residual vector R, find norm,
*        then check for tolerance.
*
         CALL DAXPY( N, -ALPHA,  WORK(1,Q), 1, WORK(1,R), 1 )
*
*********RESID = DNRM2( N, WORK(1,R), 1 ) / BNRM2
*********IF ( RESID.LE.TOL ) GO TO 30
*
         NDX1 = NEED1
         NDX2 = NEED2
*        Prepare for resumption & return
         RLBL = 5
         IJOB = 4
         RETURN
*
*****************
 5       CONTINUE
*****************
         IF( INFO.EQ.1 ) GO TO 30
*
         IF ( ITER.EQ.MAXIT ) THEN
            INFO = 1
            GO TO 20
         ENDIF
*
         RHO1 = RHO
*
         GO TO 10
*
   20 CONTINUE
*
*     Iteration fails.
*
      RLBL = -1
      IJOB = -1
      RETURN
*
   30 CONTINUE
*
*     Iteration successful; return.
*
      INFO = 0
      RLBL = -1
      IJOB = -1
      RETURN
*
*     End of CGREVCOM
*
      END
