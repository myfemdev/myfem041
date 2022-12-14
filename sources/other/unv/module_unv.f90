module module_unv
!-----------------------------------------------------------------------
! Module for UNV file management
! Last update: 04/04/2010
!-----------------------------------------------------------------------
use module_ALLOC
use module_FILES
use module_mesh
use module_dataset_2411
use module_dataset_2412
use module_dataset_2467
use module_bsearch
implicit none

!Types
type unv
  private
  character(len=MAXPATH) :: filename = ' ' !file name
  integer                :: UNIT     = -1  !associated unit number
end type

!Private module procedures
private :: search_dataset_delimiter, search_dataset_type, create_vertex_data

contains

!***********************************************************************
! INPUT PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! open: open universal file
!-----------------------------------------------------------------------
subroutine open_unv(this, filename)

type(unv)                    :: this     !unv object
character(len=*), intent(in) :: filename !unv file
integer :: ios

! open file
this%filename = filename
this%unit = get_unit()
open (unit=this%unit, file=this%filename, form='formatted', iostat=ios, &
status='old', position='rewind')
if (ios /= 0) call error('unv/open, #'//trim(string(ios)))

end subroutine

!-----------------------------------------------------------------------
! read: read universal file
!-----------------------------------------------------------------------
subroutine read_unv(this, m, maxdim)

  type(unv),                              intent(in)    :: this   !universal object
  type(mesh),  dimension(:), allocatable, intent(inout) :: m      !mesh
  integer,                                intent(inout) :: maxdim !dimension detected
  integer :: ios, n, j

  if(.not. allocated(m)) call error('unv/read, mesh(es) not allocated')
  n = size(m,1) !last mesh

! dataset 2411, node coordinates
  rewind(unit=this%unit, iostat=ios)
  if (ios /= 0) call error('unv/read/rewind, #'//trim(string(ios)))
  if (search_dataset_type(this,2411) /= 0) call error('unv/read, dataset 2411 not found')
  call read_2411(this%unit, m(n))

! dataset 2412, elements
  rewind(unit=this%unit, iostat=ios)
  if (ios /= 0) call error('unv/read/rewind, #'//trim(string(ios)))
  if (search_dataset_type(this,2412) /= 0) call error('unv/read, dataset 2412 not found')
  call read_2412(this%unit, m, maxdim)
  if (maxdim < n) then !mesh dimension is less than allocated one  
    do j = 1, m(n)%nd  !tranfer node coordinates from m(n) to m(maxdim)
      call set(m(maxdim)%xd, m(n)%xd(1:3,j), col=m(maxdim)%nd+1, fit_row=.true.)
      m(maxdim)%nd = m(maxdim)%nd + 1
    enddo
    do j = maxdim+1,n !deallocate meshes from m(maxdim+1) to m(n) 
      call dealloc_mesh(m(j))
    enddo
    n = maxdim
  endif
  print'(a,a)', 'Detected FE of higher dimension: ',trim(m(n)%FEtype)

! dataset 2467, groups
  rewind(unit=this%unit, iostat=ios)
  if (ios /= 0) call error('unv/read/rewind, #'//trim(string(ios)))
  if (search_dataset_type(this,2467) /= 0) then
    call info('unv/read, dataset 2467 (gropus) not found')
    call alloc(m(n)%rv, m(n)%LNV, m(n)%nl)
    call alloc(m(n)%re, m(n)%LNE, m(n)%nl)
    call alloc(m(n)%rf, m(n)%LNF, m(n)%nl)
    call alloc(m(n)%rl, m(n)%nl)
  else
    call read_2467(this%unit, m, n)
  end if

! create vertex data
  call create_vertex_data(m(n))
  if (n == 2 .and. m(n)%DIM == 3) call info('Triangles does not belong to XY plane: &
  &counter-clockwise orientation may not be ensured.')

! close file
  close(unit=this%unit, iostat=ios)
  if (ios /= 0) call error('univ_file/close, #'//trim(string(ios)))

end subroutine

!***********************************************************************
! PRIVATE  PROCEDURES
!***********************************************************************
!-----------------------------------------------------------------------
! search_dataset_delimiter: searchs the next dataset delimiter
! RETURN:  0 if the delimiter is found
!         <0 if the end of file is reached
!         >0 if an error occurs
!-----------------------------------------------------------------------
function search_dataset_delimiter(this) result(res)

  type(unv), intent(in) :: this !universal file
  integer :: res, val

  do
    read (unit=this%unit, fmt='(I10)', iostat=res) val
!   find EOF or '-1', then return
    if (res<0 .or. val==-1) return
!   res>0 when reading chars, then cycle
  end do

end function

!-----------------------------------------------------------------------
! search_dataset_type: searchs a specific dataset type
! RETURN:  0 if the delimiter is found
!         <0 if the end of file is reached
!         >0 if an error occurs
!-----------------------------------------------------------------------
function search_dataset_type(this, ds) result(res)

  type(unv), intent(in) :: this !universal object
  integer,   intent(in) :: ds   !dataset type
  integer :: res, dsnumber

  do
    res = search_dataset_delimiter(this) 
    if (res /= 0) return
!   read the type of dataset
    read (unit=this%unit, fmt='(I10)', iostat=res) dsnumber
    if (res /= 0) call error('unv/search_datast_type, #'//trim(string(res)))
    if (dsnumber == ds) exit
    res = search_dataset_delimiter(this)
    if (res /= 0) return
  enddo

end function

!-----------------------------------------------------------------------
! create_vertex_data: create iv and xv from id and xd
!-----------------------------------------------------------------------
subroutine create_vertex_data(m)
type(mesh), intent(inout) :: m !mesh
integer, allocatable :: vert2node(:), node2vert(:)
integer :: nv2d, pos, maxv, i, j, k

if (m%LNN /= m%LNV) then
  !construct vert2node
  nv2d = 0
  do k = 1, m%nl
    do j = 1, m%LNV
      if (m%id(j,k) == 0) cycle
      pos = bsearch(nv2d, vert2node, m%id(j,k)) 
      if (pos < 0) then
        call set(vert2node, m%id(j,k), -pos, insert=.true.)
        nv2d = nv2d + 1 
      endif
    end do
  end do
  call reduce(vert2node, nv2d)
  m%nv = nv2d
  !construct node2vert
  maxv = 0
  do i = 1, size(vert2node, 1)
    maxv = max(maxv, vert2node(i))
  enddo
  allocate(node2vert(maxv))
  node2vert = 0
  do i = 1, size(vert2node, 1)
    if (vert2node(i) == 0) cycle
    node2vert(vert2node(i)) = i
  enddo
  !vertices renumbering
  call alloc(m%iv, m%LNV, m%nl)
  do k = 1, m%nl
    do j = 1, m%LNV
      m%iv(j,k) = node2vert(m%id(j,k))
    end do
  end do
  !correct dimension
  if (any(abs(m%xd(3,:))>1e3*epsilon(m%xd))) m%DIM = 3
  print'(a,i1)', 'Maximum detected dimension: ',m%DIM
  !vertices coordinates
  call alloc(m%xv, m%DIM, m%nv)
  do i = 1, m%nv
    m%xv(1:m%DIM,i) = m%xd(1:m%DIM, vert2node(i))
  end do
else
   m%nv = m%nd
   call alloc(m%iv, m%LNV, m%nl)
   m%iv(1:m%LNV,1:m%nl) = m%id(1:m%LNV,1:m%nl)
   if (any(abs(m%xd(3,:))>1e3*epsilon(m%xd))) m%DIM = 3
   print'(a,i1)', 'Maximum detected dimension: ',m%DIM
   !vertices coordinates
   call alloc(m%xv, m%DIM, m%nv)
   m%xv(1:m%DIM,1:m%nv) = m%xd(1:m%DIM,1:m%nv)
end if

end subroutine

end module
