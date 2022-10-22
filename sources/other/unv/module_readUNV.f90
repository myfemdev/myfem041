module module_readUNV
!-----------------------------------------------------------------------
! Program to convert a I-Deas universal file to a Modulef (RECONVXX)
! formatted mesh
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Francisco Pena, fran.pena@usc.es
! Last update: 10/06/2011
!
! NOTES: 1) mesh must be composed of a single FE type
!        2) planar meshes must lay in the XY plane
!-----------------------------------------------------------------------
use module_compiler_dependant, only: real64
use module_SO_DEPENDANT
use module_CONVERS
use module_unv
use module_mesh
implicit none

contains

!-----------------------------------------------------------------------
! readUNV: read a UNV file
!-----------------------------------------------------------------------
subroutine readUNV(unvfile, nel, nnod, nver, DI_, LNN, LNV, LNE, LNF, nn, mm, nrc, nra, nrv, z, nsd)
character(len=*),         intent(in)  :: unvfile
integer,                  intent(out) :: nel, nnod, nver, DI_, LNN, LNV, LNE, LNF
integer, allocatable,     intent(out) :: nn(:,:), mm(:,:), nrc(:,:), nra(:,:), nrv(:,:), nsd(:)
real(real64),allocatable, intent(out) :: z(:,:)
type(unv):: u
type(mesh), allocatable :: m(:)
integer :: d, st, DIM

DIM = 3 !it can change after reading dataset 2412
!inital settings
call set_SO()
call set_report_level(REPORT_STDOUT)
!allocate mesh(es)
allocate(m(DIM))
forall (d = 1:DIM) m(d)%DIM = d
!process universal file
call open_unv(u, unvfile)
call read_unv(u, m, DIM)

!return msh variables
nel  = m(DIM)%nl
nnod = m(DIM)%nd
nver = m(DIM)%nv
DI_  = m(DIM)%DIM
LNN  = m(DIM)%LNN
LNV  = m(DIM)%LNV
LNE  = m(DIM)%LNE
LNF  = m(DIM)%LNF
if (allocated(m(DIM)%id)) then; allocate( nn(LNN,nel));  nn(1:LNN,1:nel)  = m(DIM)%id; end if
if (allocated(m(DIM)%iv)) then; allocate( mm(LNV,nel));  mm(1:LNV,1:nel)  = m(DIM)%iv; end if
if (allocated(m(DIM)%rf)) then; allocate(nrc(LNF,nel)); nrc(1:LNF,1:nel)  = m(DIM)%rf; end if
if (allocated(m(DIM)%re)) then; allocate(nra(LNE,nel)); nra(1:LNE,1:nel)  = m(DIM)%re; end if
if (allocated(m(DIM)%rv)) then; allocate(nrv(LNV,nel)); nrv(1:LNV,1:nel)  = m(DIM)%rv; end if
if (allocated(m(DIM)%xv)) then; allocate(  z(DI_,nver));  z(1:DI_,1:nver) = m(DIM)%xv; end if
if (allocated(m(DIM)%rl)) then; allocate(    nsd(nel));       nsd(1:nel)  = m(DIM)%rl; end if

!deallocate mesh
do d = 1, DIM
  call dealloc_mesh(m(d))
enddo
deallocate(m)
end subroutine

end module
