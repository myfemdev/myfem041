module module_readPVD
!-----------------------------------------------------------------------
! module to read PVD files
! Last update: 10/10/2011
!-----------------------------------------------------------------------
use module_compiler_dependant, only: real64
use module_FILES
implicit none

contains

!-----------------------------------------------------------------------
! Read a PVD file
!-----------------------------------------------------------------------
subroutine readPVD(fich, time, vtu)
character(*),               intent(in)  :: fich
real(real64),  allocatable, intent(out) :: time(:)
character(*),  allocatable, intent(out) :: vtu(:)
character(255) :: line
integer :: un, ios, p, q, n, i

un = get_unit()
open (unit=un, file=fich, form='formatted', position='rewind', iostat=ios)
if (ios /= 0) call error('readPVD/open, #'//trim(string(ios)))

n = 0
do
  read (unit=un, fmt='(a)', iostat=ios) line
  if (ios < 0) exit !found EOF
  if (index(line, 'timestep="') > 0) n = n + 1
end do
allocate(time(n), vtu(n))
rewind(un)
i = 0
do
  if (i == n) exit
  read (unit=un, fmt='(a)', iostat=ios) line
  if (ios < 0) call error('readPVD/read, #'//trim(string(ios)))
  !time
  p = index(line, 'timestep="')
  if (p > 0) then
    i = i + 1
    q = index(line(p+10:),'"')
    read(line(p+10:q+p+8),*) time(i)
    !file
    p = index(line, 'file="')
    if (p == 0) stop 'ERROR: mark file= not found'
    q = index(line(p+6:),'"')
    vtu(i) = line(p+6:q+p+4)
  end if
end do
end subroutine

end module
