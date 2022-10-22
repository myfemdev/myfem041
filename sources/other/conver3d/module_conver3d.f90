module module_conver3d
!-----------------------------------------------------------------------
! module to create auxiliary array for the mesh
!
! Last update: 01/02/2012
! Programmers: fran.pena@usc.es
!
! conver3d: read the mesh, transfers it to conver() and returns
!    nemm, number of non-zero elements in the matrix
!    det(nel), determinant
!    binv(3,3,nel), inverse
!    ib(nver+1), row  storage pointer (Morse)
!    jb(nver), column storage pointer (Morse)
!-----------------------------------------------------------------------
use module_compiler_dependant, only: real64
use module_conver3d_source, only: conver,nel,nver,nemm,mm,z,det,binv,ib,jb
implicit none

contains

subroutine conver3d(nel_,nver_,mm_,z_,nemm_,det_,binv_,ib_,jb_)
integer,                   intent(in)  :: nel_, nver_, mm_(:,:)
real(real64),              intent(in)  :: z_(:,:)
integer,                   intent(out) :: nemm_
integer,      allocatable, intent(out) :: ib_(:), jb_(:)
real(real64), allocatable, intent(out) :: det_(:),binv_(:,:,:)

!input
nel  = nel_
nver = nver_
mm(1:4,1:nel) = mm_
z(1:3,1:nver) = z_
!calculations
call conver()
!output
allocate (det_(nel), binv_(3,3,nel), ib_(nver+1), jb_(nemm))
det_(1:nel) = det(1:nel)
binv_(1:3,1:3,1:nel) = binv(1:3,1:3,1:nel)
ib_(1:nver+1) = ib(1:nver+1)
nemm_ = nemm
jb_(1:nemm) = jb(1:nemm)
end subroutine

end module
