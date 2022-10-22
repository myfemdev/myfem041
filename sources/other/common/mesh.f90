module mesh

use defines

implicit none

type mfm_data

integer :: nel      !global number of elements
integer :: nnod     !global number of nodes
integer :: nver     !global number of vertices
integer :: dim      !space dimension
integer :: lnn      !local number of nodes
integer :: lnv      !local number of vertices
integer :: lne      !local number of edges
integer :: lnf      !local number of faces
integer, allocatable, dimension(:,:) :: nn       !nodes index array
integer, allocatable, dimension(:,:) :: mm       !vertices index array
integer, allocatable, dimension(:,:) :: nrv      !vertices reference array
integer, allocatable, dimension(:,:) :: nra      !edge reference array
integer, allocatable, dimension(:,:) :: nrc      !face reference array
real(real64), allocatable, dimension(:,:) :: z        !vertices coordinates array
integer, allocatable, dimension(:)   :: nsd      !subdomain index array

end type mfm_data


contains


! reads unformatted modulef record
integer function mesh_read_array_unformatted(filename, num, array)
implicit none
 character (len=*), intent(IN) :: filename
 integer :: num
 integer, dimension(num) :: array
 integer :: ios, iu, i

 mesh_read_array_unformatted = 0

 iu = 21
 open  (unit=iu, file=filename, form='unformatted', position='rewind', status='old', iostat=ios)
    if (ios /= 0) then
        mesh_read_array_unformatted = 1
        return
    endif

 read (unit=iu, iostat=ios) (array(i), i=1,num)
    if (ios /= 0) then
        mesh_read_array_unformatted = 2
        close(iu)
        return
    endif

 close(iu, iostat=ios)
    if (ios /= 0) then
        mesh_read_array_unformatted = 10
        return
    endif

end function mesh_read_array_unformatted


! reads formatted modulef record
integer function mesh_read_array_formatted(filename, num, array)
implicit none
 character (len=*), intent(IN) :: filename
 integer :: num
 integer, dimension(num) :: array
 integer :: ios, iu, i

 mesh_read_array_formatted = 0

 iu = 21
 open  (unit=iu, file=filename, form='formatted', position='rewind', status='old', iostat=ios)
    if (ios /= 0) then
        mesh_read_array_formatted = 1
        return
    endif

 read (unit=iu, fmt=*, iostat=ios) (array(i), i=1,num)
    if (ios /= 0) then
        mesh_read_array_formatted = 2
        close(iu)
        return
    endif

 close(iu, iostat=ios)
    if (ios /= 0) then
        mesh_read_array_formatted = 10
        return
    endif

end function mesh_read_array_formatted


! reads formatted modulef header of file
integer function mesh_read_header(filename, mesh_data)
implicit none
character (len=*), intent(IN) :: filename
type(mfm_data), intent(INOUT) :: mesh_data
integer :: ios, iu

 mesh_read_header = 0

 call mesh_free(mesh_data)

 iu = 21
 open  (unit=iu, file=filename, form='formatted', position='rewind', status='old', iostat=ios)
    if (ios /= 0) then
        mesh_read_header = 1
        return
    endif

 read (unit=iu, fmt=*, iostat=ios) mesh_data%nel, mesh_data%nnod, mesh_data%nver,&
    &mesh_data%dim, mesh_data%lnn, mesh_data%lnv, mesh_data%lne, mesh_data%lnf
    if (ios /= 0) then
        mesh_read_header = 2
        close(iu)
        return
    endif

 close(iu, iostat=ios)
    if (ios /= 0) then
        mesh_read_header = 10
        return
    endif

end function mesh_read_header


! reads formatted modulef file
integer function mesh_read(filename, mesh_data)
implicit none
character (len=*), intent(IN) :: filename
type(mfm_data), intent(INOUT) :: mesh_data
integer :: i, j, k, ln2, lf2, le2, ios, iu

 mesh_read = 0

 call mesh_free(mesh_data)

 iu = 21
 open  (unit=iu, file=filename, form='formatted', position='rewind', status='old', iostat=ios)
    if (ios /= 0) then
        mesh_read = 1
        return
    endif

 read (unit=iu, fmt=*, iostat=ios) mesh_data%nel, mesh_data%nnod, mesh_data%nver,&
    &mesh_data%dim, mesh_data%lnn, mesh_data%lnv, mesh_data%lne, mesh_data%lnf
    if (ios /= 0) then
        mesh_read = 2
        close(iu)
        return
    endif

 call mesh_print(mesh_data)

 !!save ([nn,if nnod/=nver], mm, [nrc,if dim==3], [nra,if dim>=2], nrv, z, nsd)
 ln2 = mesh_data%lnn; if (mesh_data%nnod == mesh_data%nver) ln2 = 0
 le2 = mesh_data%lne; if (mesh_data%dim < 2) le2 = 0
 lf2 = mesh_data%lnf; if (mesh_data%dim < 3) lf2 = 0

 allocate(mesh_data%nn(ln2, mesh_data%nel))
 allocate(mesh_data%mm(mesh_data%lnv, mesh_data%nel))
 allocate(mesh_data%nrc(lf2, mesh_data%nel))
 allocate(mesh_data%nra(le2, mesh_data%nel))
 allocate(mesh_data%nrv(mesh_data%lnv, mesh_data%nel))
 allocate(mesh_data%z(mesh_data%dim, mesh_data%nver))
 allocate(mesh_data%nsd(mesh_data%nel))

 read (unit=iu, fmt=*, iostat=ios) ((mesh_data%nn(i,k),  i=1,ln2), k=1,mesh_data%nel), &
                                   ((mesh_data%mm(i,k),  i=1,mesh_data%lnv), k=1,mesh_data%nel), &
                                   ((mesh_data%nrc(i,k), i=1,lf2), k=1,mesh_data%nel), &
                                   ((mesh_data%nra(i,k), i=1,le2), k=1,mesh_data%nel), &
                                   ((mesh_data%nrv(i,k), i=1,mesh_data%lnv), k=1,mesh_data%nel), &
                                   ((mesh_data%z(i,j),   i=1,mesh_data%dim), j=1,mesh_data%nver)
    if (ios /= 0) then
        mesh_read = 3
        close(iu)
        return
    endif
 read (unit=iu, fmt=*, iostat=ios) (mesh_data%nsd(k), k=1,mesh_data%nel)
    if (ios /= 0) then
        mesh_read = 4
        close(iu)
        return
    endif

 close(iu, iostat=ios)
    if (ios /= 0) then
        mesh_read = 10
        return
    endif


end function mesh_read


! writes formatted modulef header of file
integer function mesh_write(filename, mesh_data)
character (len=*), intent(IN) :: filename
type(mfm_data), intent(INOUT) :: mesh_data
integer :: i, j, k, ln2, lf2, le2, ios, iu

 mesh_write = 0
 iu = 20
 open  (unit=iu, file=filename, form='formatted', position='rewind', iostat=ios)
    if (ios /= 0) then
        mesh_write = 1
        return
    endif

 write (unit=iu, fmt=*, iostat=ios) mesh_data%nel, mesh_data%nnod, mesh_data%nver,&
    &mesh_data%dim, mesh_data%lnn, mesh_data%lnv, mesh_data%lne, mesh_data%lnf
    if (ios /= 0) then
        mesh_write = 5
        close(iu)
        return
    endif

 !!save ([nn,if nnod/=nver], mm, [nrc,if dim==3], [nra,if dim>=2], nrv, z, nsd)
 ln2 = mesh_data%lnn; if (mesh_data%nnod == mesh_data%nver) ln2 = 0
 le2 = mesh_data%lne; if (mesh_data%dim < 2) le2 = 0
 lf2 = mesh_data%lnf; if (mesh_data%dim < 3) lf2 = 0
 write (unit=iu, fmt=*, iostat=ios) ((mesh_data%nn(i,k),  i=1,ln2), k=1,mesh_data%nel), &
                                   ((mesh_data%mm(i,k),  i=1,mesh_data%lnv), k=1,mesh_data%nel), &
                                   ((mesh_data%nrc(i,k), i=1,lf2), k=1,mesh_data%nel), &
                                   ((mesh_data%nra(i,k), i=1,le2), k=1,mesh_data%nel), &
                                   ((mesh_data%nrv(i,k), i=1,mesh_data%lnv), k=1,mesh_data%nel), &
                                   ((mesh_data%z(i,j),   i=1,mesh_data%dim), j=1,mesh_data%nver)
    if (ios /= 0) then
        mesh_write = 6
        close(iu)
        return
    endif

 write (unit=iu, fmt=*, iostat=ios) (mesh_data%nsd(k), k=1,mesh_data%nel)
    if (ios /= 0) then
        mesh_write = 7
        close(iu)
        return
    endif

 close(iu, iostat=ios)

    if (ios /= 0) then
        mesh_write = 10
        return
    endif

end function mesh_write


! free memory
subroutine mesh_free(mesh_data)
type(mfm_data), intent(INOUT) :: mesh_data
    if (allocated(mesh_data%nn)) deallocate(mesh_data%nn)
    if (allocated(mesh_data%mm)) deallocate(mesh_data%mm)
    if (allocated(mesh_data%nrv)) deallocate(mesh_data%nrv)
    if (allocated(mesh_data%nra)) deallocate(mesh_data%nra)
    if (allocated(mesh_data%nrc)) deallocate(mesh_data%nrc)
    if (allocated(mesh_data%z)) deallocate(mesh_data%z)
    if (allocated(mesh_data%nsd)) deallocate(mesh_data%nsd)
end subroutine


! free memory and clear variables
subroutine mesh_clear(mesh_data)
type(mfm_data), intent(INOUT) :: mesh_data
    call mesh_free(mesh_data)
    mesh_data%nel = 0
    mesh_data%nnod = 0
    mesh_data%nver = 0
    mesh_data%dim = 0
    mesh_data%lnn = 0
    mesh_data%lnv = 0
    mesh_data%lne = 0
    mesh_data%lnf = 0
end subroutine


! print header
subroutine mesh_print(mesh_data)
    type(mfm_data), intent(IN) :: mesh_data
    print*, mesh_data%nel, mesh_data%nnod, mesh_data%nver,&
        &mesh_data%dim, mesh_data%lnn, mesh_data%lnv, mesh_data%lne, mesh_data%lnf
end subroutine


! convert error number to error string corresponding to read/write operations
function mesh_error_string(error_num)
 implicit none
 integer :: error_num
 character(len=100) :: mesh_error_string

    select case(error_num)
        case (0)
            mesh_error_string = 'no error'
        case (1)
            mesh_error_string = 'error opening mesh file'
        case (2)
            mesh_error_string = 'error reading mesh file header'
        case (3)
            mesh_error_string = 'error reading mesh file body (1)'
        case (4)
            mesh_error_string = 'error reading mesh file body (2)'
        case (5)
            mesh_error_string = 'error writing mesh file header'
        case (6)
            mesh_error_string = 'error writing mesh file body (1)'
        case (7)
            mesh_error_string = 'error writing mesh file body (2)'
        case (10)
            mesh_error_string = 'error closing mesh file'
        case default
            mesh_error_string = 'unknown error'
    end select
end function


end module mesh

