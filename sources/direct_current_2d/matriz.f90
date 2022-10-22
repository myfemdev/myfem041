!-----------------------------------------------------------------
!            COMPUTATION OF THE MATRIX OF THE PROBLEM 
!-----------------------------------------------------------------

subroutine matriz()

  use malla_2DP1
  use dcurrent_2D
  use external_electros
  use conductividad
 
  implicit none
  double precision :: a(3,3)
  integer          :: ndcc,mm1,mm2,mm3
  double precision :: ab,bc,cd,de,det
  integer          :: i,k

  ndcc = mua(nver+1)
  do i=1,ndcc
     c(i)=0.d0
  enddo

! COMPUTATION AND ASSEMBLY OF THE MATRIX 
  do k=1,nel
     mm1=mm(1,k)
     mm2=mm(2,k)
     mm3=mm(3,k)
     ab = z(1,mm2) - z(1,mm1)
     bc = z(2,mm2) - z(2,mm1)
     cd = z(1,mm3) - z(1,mm1)
     de = z(2,mm3) - z(2,mm1)
     det = ab*de-bc*cd
     if (abs(det).le.1d-12) then
        print*,'det : ',det
        print*,'k : ', k
        print*,'mm : ', mm(1,k), mm(2,k), mm(3,k)
        print*,'abcd : ', ab, bc, cd, de
        stop 1
     endif
     call matel(iop,mm(1,k),nsd(k),z,ab,bc,cd,de,det,condfun,a)
     call ens(mua,a,mm(1,k),c)
  enddo

  return
end
