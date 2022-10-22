module module_writePVD
!-----------------------------------------------------------------------
! module for writing PVD files
! Last update: 11/03/2010
!-----------------------------------------------------------------------
use module_FILES
implicit none

contains

!-----------------------------------------------------------------------
! Write a PVD file
!-----------------------------------------------------------------------
subroutine writePVD(file, prefix, time)
character(len=*), intent(in) :: file, prefix
real, dimension(:) :: time
integer :: un, ios, i

un = get_unit()
open (unit=un, file=file, form='formatted', position='rewind', iostat=ios)
if (ios /= 0) call error('writePVD/open, #'//trim(string(ios)))

!HEAD
write(un,'(a)') '<?xml version="1.0"?>'
write(un,'(a)') '<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">'
write(un,'(a)') '  <Collection>'
!TIME LOOP
do i = 1,size(time,1)
  write(un,'(a)') '    <DataSet timestep="'//trim(string(time(i)))//'" group="" part="0" file="'//&
  &trim(prefix)//'_'//trim(string(i))//'.vtu"/>'
enddo
!TAIL
write(un,'(a)') '  </Collection>'
write(un,'(a)') '</VTKFile>'
close(un)

end subroutine

!-----------------------------------------------------------------------
! Write a PVD for a single time
!-----------------------------------------------------------------------
subroutine writePVD_static(file, prefix, nums)
character(len=*), intent(in) :: file, prefix
integer, dimension(:) :: nums
integer :: un, ios, i

un = get_unit()
open (unit=un, file=file, form='formatted', position='rewind', iostat=ios)
if (ios /= 0) call error('writePVD/open, #'//trim(string(ios)))

!HEAD
write(un,'(a)') '<?xml version="1.0"?>'
write(un,'(a)') '<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">'
write(un,'(a)') '  <Collection>'
!NUMS LOOP
do i = 1,size(nums,1)
  write(un,'(a)') '    <DataSet timestep="0.0" group="" part="0" file="'//&
  &trim(prefix)//trim(string(nums(i)))//'.vtu"/>'
enddo
!TAIL
write(un,'(a)') '  </Collection>'
write(un,'(a)') '</VTKFile>'
close(un)

end subroutine

end module
