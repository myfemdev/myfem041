subroutine cgrevcom(n,b,x,work,ldw,iter,resid,info,ndx1,ndx2,sclr1,sclr2,ijob)

  implicit none
  integer,          intent(in)    :: n,ldw
  double precision, intent(in)    :: resid
  double precision, intent(in)    :: b(*)
  double precision, intent(in)    :: x(*)
  integer,          intent(inout) :: iter,ndx1,ndx2,ijob
  integer,          intent(out)   :: info
  double precision, intent(out)   :: sclr1,sclr2
  double precision, intent(out)   :: work(ldw,*)
  double precision, parameter     :: zero=0.D0,one=1.D0
  integer                         :: maxit,r,z,p,q,need1,need2
  double precision                :: alpha,beta,rho,rho1,ddot,dnrm2,tol
  integer                         :: rlbl
  save
  external                        :: daxpy,dcopy,ddot,dnrm2

  if (ijob.eq.2) then
  
    if (rlbl.eq.2) then
       if (dnrm2(n,work(1,r),1).lt.tol) then
! ITERATION SUCCESSFUL; RETURN.
          info = 0
          rlbl = -1
          ijob = -1
          return
       end if
       iter = 0
! PERFORM PRECONDITIONED CONJUGATE GRADIENT ITERATION.
       iter = iter + 1
       ndx1 = ((z-1)*ldw)+1
       ndx2 = ((r-1)*ldw)+1
! PREPARE FOR RETURN & RETURN
       rlbl = 3
       ijob = 2
       return   
    end if
    
    if (rlbl.eq.3) then  
       rho = ddot(n,work(1,r),1,work(1,z),1)
! COMPUTE DIRECTION VECTOR P
       if (iter.gt.1) then
          beta = rho/rho1
          call daxpy(n,beta,work(1,p),1,work(1,z),1)
          call dcopy(n,work(1,z),1,work(1,p),1)
       else
          call dcopy(n,work(1,z),1,work(1,p),1)
       endif
! COMPUTE SCALAR alpha (SAVE A*P TO Q).
       ndx1 = ((p-1)*ldw)+1
       ndx2 = ((q-1)*ldw)+1
! PREPARE FOR RETURN & RETURN
       sclr1 = one
       sclr2 = zero
       rlbl = 4
       ijob = 1
       return   
    end if
    
    if (rlbl.eq.4) then
       alpha =  rho/ddot(n,work(1,p),1,work(1,q),1)
! COMPUTE CURRENT SOLUTION VECTOR x
       call daxpy(n,alpha,work(1,p),1,x,1)
! COMPUTE RESIDUAL VECTOR r, FIND NORM,THEN CHECK FOR TOLERANCE.
       call daxpy(n,-alpha,work(1,q),1,work(1,r),1)
       ndx1 = need1
       ndx2 = need2
! PREPARE FOR RESUMPTION & RETURN
       rlbl = 5
       ijob = 4
       return
    end if
    
    if (rlbl.eq.5) then
       if (info.eq.1) then
 ! ITERATION SUCCESSFUL; RETURN.
          info = 0
          rlbl = -1
          ijob = -1
          return
       end if
       if (iter.eq.maxit) then
          info = 1
! ITERATION FAILS.
          rlbl = -1
          ijob = -1
          return
       endif
       rho1 = rho
! PERFORM PRECONDITIONED CONJUGATE GRADIENT ITERATION.
       iter = iter + 1
       ndx1 = ((z-1)*ldw)+1
       ndx2 = ((r-1)*ldw)+1
! PREPARE FOR RETURN & RETURN
       rlbl = 3
       ijob = 2
       return
    end if
    
! IF NEITHER OF THESE, THEN ERROR
    info = -6
    rlbl = -1
    ijob = -1
    return
    
  endif
  
  info = 0
  maxit = iter
  tol = resid
! ALIAS WORKSPACE COLUMNS.
  r = 1
  z = 2
  p = 3
  q = 4
! CHECK IF CALLER WILL NEED INDEXING INFO.
  if (ndx1.ne.-1) then
     if (ndx1.eq.1) then
        need1 = ((r-1)*ldw)+1
     elseif(ndx1.eq.2) then
        need1 = ((z-1)*ldw)+1
     elseif(ndx1.eq.3) then
        need1 = ((p-1)*ldw)+1
     elseif(ndx1.eq.4) then
        need1 = ((q-1)*ldw)+1
     else
! REPORT ERROR
        info = -5
        rlbl = -1
        ijob = -1
        return
     endif
  else
     need1 = ndx1
  endif
  if (ndx2.ne.-1) then
     if (ndx2.eq.1) then
        need2 = ((r-1)*ldw)+1
     elseif(ndx2.eq.2) then
        need2 = ((z-1)*ldw)+1
     elseif(ndx2.eq.3) then
        need2 = ((p-1)*ldw)+1
     elseif(ndx2.eq.4) then
        need2 = ((q-1)*ldw)+1
     else
! REPORT ERROR
        info = -5
        info = -5
        rlbl = -1
        ijob = -1
        return
     endif
  else
     need2 = ndx2
  endif
! SET INITIAL RESIDUAL
  call dcopy(n,b,1,work(1,r),1)
  if (dnrm2(n,x,1).ne.zero) then
! SET ARGS FOR REVCOM RETURN
     sclr1 = -one
     sclr2 = one
     ndx1 = -1
     ndx2 = ((r-1)*ldw)+1
! PREPARE FOR RESUMPTION & RETURN
     rlbl = 2
     ijob = 3
     return
  end if
     
  if (dnrm2(n,work(1,r),1).lt.tol) then
! ITERATION SUCCESSFUL; RETURN.
     info = 0
     rlbl = -1
     ijob = -1
     return
  end if
  iter = 0
! PERFORM PRECONDITIONED CONJUGATE GRADIENT ITERATION.
  iter = iter + 1
  ndx1 = ((z-1)*ldw)+1
  ndx2 = ((r-1)*ldw)+1
! PREPARE FOR RETURN & RETURN
  rlbl = 3
  ijob = 2
  return
  
end