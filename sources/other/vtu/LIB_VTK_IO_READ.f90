module LIB_VTK_IO_READ

use LIB_VTK_IO !temporal use, while this functions are not included in LIB_VTK_IO
implicit none

private
! functions for VTK XML
public:: VTK_INI_XML_READ
public:: VTK_GEO_XML_READ
public:: VTK_CON_XML_READ
public:: VTK_VAR_XML_READ
public:: VTK_END_XML_READ

! overloading of VTK_GEO_XML
interface VTK_GEO_XML_READ
  module procedure VTK_GEO_XML_UNST_R8_READ    ! real(R8P) UnstructuredGrid
end interface
! overloading of VTK_VAR_XML
interface VTK_VAR_XML_READ
  module procedure VTK_VAR_XML_R8_READ !, & ! real(R8P)    scalar/vectorial
  module procedure VTK_VAR_XML_R4_READ !, & ! real(R4P)    scalar/vectorial
end interface

!private variables
character(len=maxlen) :: fname         ! VTK filename 
character(len=maxlen) :: buffer        ! to store formatted records from VTK file
integer               :: append_offset ! offset for the appended data
contains

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_INI_XML_READ(input_format,filename,mesh_topology,npieces) result(E_IO)
character(*), intent(IN)::  input_format  ! input format: ASCII or BINARY
character(*), intent(IN)::  filename      ! file name
character(*), intent(IN)::  mesh_topology ! mesh topology
integer(I4P), intent(OUT):: npieces       ! Number of pieces stored in the file
integer(I4P)::              E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
character :: c1, c2

topology = trim(mesh_topology)
Unit_VTK=GetUnit()
fname = filename
select case(trim(Upper_Case(input_format)))
case('ASCII')
  stop 'Not implemented'
case('BINARY')
  f_out = f_out_binary
  select case(trim(topology))
  case('RectilinearGrid','StructuredGrid')
    stop 'Not implemented'
  case('UnstructuredGrid')    
    open(unit=Unit_VTK,file=trim(fname),status='old',form='UNFORMATTED',access='STREAM',action='READ',&
    convert='BIG_ENDIAN',iostat=E_IO, position='REWIND')
    ! count the pieces
    npieces = 0
    do
      E_IO = read_record(buffer)
      buffer = trim(adjustlt(Upper_Case(buffer)))
      if (index(buffer, '</UNSTRUCTUREDGRID') > 0) exit !end of ASCII header section found
      if (index(buffer, '<PIECE') > 0) npieces = npieces + 1
    enddo
    ! calculate the offset to reach the appended data
    rewind(unit=Unit_VTK, iostat=E_IO)
    read(unit=Unit_VTK,iostat=E_IO) c1
    do 
      read(unit=Unit_VTK,iostat=E_IO) c2
      if (iachar(c1)==10 .and. c2 =='_') exit
      c1 = c2
    enddo
    inquire(unit=Unit_VTK, pos=append_offset)
  end select
end select  
end function VTK_INI_XML_READ

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_GEO_XML_UNST_R8_READ(NN,NC,X,Y,Z,npiece) result(E_IO)
integer(I4P), intent(OUT) :: NN       ! number of nodes
integer(I4P), intent(OUT) :: NC       ! number of cells
real(R8P),    intent(OUT), allocatable :: X(:)  ! x coordinates
real(R8P),    intent(OUT), allocatable :: Y(:)  ! y coordinates
real(R8P),    intent(OUT), allocatable :: Z(:)  ! z coordinates
integer(I4P), intent(IN), optional :: npiece   ! Number of the piece to read (by default: 1)
integer(I4P) :: E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
character(len=maxlen) :: fmt
integer :: np, i, offs
integer(I4P) :: N_Byte

np = 1; if (present(npiece)) np = npiece
select case(f_out)
case(f_out_ascii)
  stop 'Not implemented'
case(f_out_binary)
  rewind(unit=Unit_VTK, iostat=E_IO)
  E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np) ! find the 'np' piece
  call get_int('NumberOfPoints', NN)
  allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
  call get_int('NumberOfCells', NC)
  E_IO = search(inside='Points', to_find='DataArray', with_attribute='Name', of_value='Point')
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, (X(i), Y(i), Z(i), i=1,NN) !get appended array
end select
end function

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_CON_XML_READ(NC,connect,offset,cell_type, npiece) result(E_IO)
integer(I4P), intent(OUT):: NC  ! number of cells
integer(I4P), intent(OUT), allocatable :: connect(:)   ! mesh connectivity
integer(I4P), intent(OUT), allocatable :: offset(:)    ! cell offset
integer(I1P), intent(OUT), allocatable :: cell_type(:) ! VTK cell type
integer(I4P), intent(IN), optional :: npiece   ! Number of the piece to read (by default: 1)
integer(I4P)::             E_IO            ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
character(len=maxlen) :: fmt
integer :: np, pos, offs
integer(I4P) :: N_Byte

np = 1; if (present(npiece)) np = npiece
select case(f_out)
case(f_out_ascii)
  stop 'Not implemented'
case(f_out_binary)
  rewind(unit=Unit_VTK, iostat=E_IO)
  E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np)
  inquire(unit=Unit_VTK, pos=pos, iostat=E_IO) !annotate the current position in the file
  call get_int('NumberOfCells', NC)
  ! get appended array offsets
  allocate(offset(NC), stat=E_IO)
  E_IO = search(from=pos,inside='Cells', to_find='DataArray', with_attribute='Name', of_value='Offsets')
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, offset !get appended array connect
  ! get appended array cell_type
  allocate(cell_type(NC), stat=E_IO)
  E_IO = search(from=pos,inside='Cells', to_find='DataArray', with_attribute='Name', of_value='Types')
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, cell_type !get appended array connect
  ! get appended array connect
  allocate(connect(offset(NC)), stat=E_IO)
  E_IO = search(from=pos,inside='Cells', to_find='DataArray', with_attribute='Name', of_value='Connectivity')
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, connect 
end select
end function

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_VAR_XML_R8_READ(var_location,NC_NN,NCOMP,varname,var,npiece) result(E_IO)
implicit none
character(*), intent(IN) :: var_location ! location of variables: CELL for cell-centered, NODE for node-centered
integer(I4P), intent(OUT):: NC_NN        ! number of cells or nodes
integer(I4P), intent(OUT):: NCOMP        ! number of components
character(*), intent(IN) :: varname      ! variable name
real(R8P),    intent(OUT), allocatable :: var(:) ! variable to be saved
integer(I4P), intent(IN), optional :: npiece ! Number of the piece to read (by default: 1)
integer(I4P)::              E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
character(len=maxlen) :: fmt
integer :: np, offs
integer(I4P) :: N_Byte

np = 1; if (present(npiece)) np = npiece
select case(f_out)
case(f_out_ascii)
  stop 'Not implemented'
case(f_out_binary)
  rewind(unit=Unit_VTK, iostat=E_IO)
  E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np)
  select case(trim(Upper_case(var_location)))
  case('NODE')
    call get_int('NumberOfPoints', NC_NN)
    E_IO = search(inside='PointData', to_find='DataArray', with_attribute='Name', of_value=varname)
    call get_int('NumberOfComponents', NCOMP)
    allocate(var(NC_NN*NCOMP), stat=E_IO)
  case('CELL')
    call get_int('NumberOfCells', NC_NN)
    E_IO = search(inside='CellData', to_find='DataArray', with_attribute='Name', of_value=varname)
    call get_int('NumberOfComponents', NCOMP)
    allocate(var(NC_NN*NCOMP), stat=E_IO)
  end select
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, var
endselect
end function

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_VAR_XML_R4_READ(var_location,NC_NN,NCOMP,varname,var,npiece) result(E_IO)
implicit none
character(*), intent(IN) :: var_location ! location of variables: CELL for cell-centered, NODE for node-centered
integer(I4P), intent(OUT):: NC_NN        ! number of cells or nodes
integer(I4P), intent(OUT):: NCOMP        ! number of components
character(*), intent(IN) :: varname      ! variable name
real(R4P),    intent(OUT), allocatable :: var(:) ! variable to be saved
integer(I4P), intent(IN), optional :: npiece ! Number of the piece to read (by default: 1)
integer(I4P)::              E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
character(len=maxlen) :: fmt
integer :: np, offs
integer(I4P) :: N_Byte

np = 1; if (present(npiece)) np = npiece
select case(f_out)
case(f_out_ascii)
  stop 'Not implemented'
case(f_out_binary)
  rewind(unit=Unit_VTK, iostat=E_IO)
  E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np)
  select case(trim(Upper_case(var_location)))
  case('NODE')
    call get_int('NumberOfPoints', NC_NN)
    E_IO = search(inside='PointData', to_find='DataArray', with_attribute='Name', of_value=varname)
    call get_int('NumberOfComponents', NCOMP)
    allocate(var(NC_NN*NCOMP), stat=E_IO)
  case('CELL')
    call get_int('NumberOfCells', NC_NN)
    E_IO = search(inside='CellData', to_find='DataArray', with_attribute='Name', of_value=varname)
    call get_int('NumberOfComponents', NCOMP)
    allocate(var(NC_NN*NCOMP), stat=E_IO)
  end select
  call get_int('offset', offs)
  call get_char('format', fmt)
  if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED') stop 'Format not implemented'
  read(unit=Unit_VTK, iostat=E_IO, pos = append_offset+offs) N_Byte, var
endselect
end function

!-----------------------------------------------------------------------------------------------------------------------------------
function VTK_END_XML_READ() result(E_IO)
integer(I4P):: E_IO 

select case(f_out)
case(f_out_ascii)
  stop 'Not implemented'
case(f_out_binary)
  close(unit=Unit_VTK, iostat=E_IO)
end select
end function

!***********************************************************************************************************************************
! PRIVATE PROCEDURES
!***********************************************************************************************************************************
!-----------------------------------------------------------------------------------------------------------------------------------
! adjustlt: extension of adjustl to remove tab characters (char(9))
!-----------------------------------------------------------------------------------------------------------------------------------
function adjustlt(string) result(res)
character(len=*), intent(in) :: string
character(len=len(string)) :: res

res = string
do while ((res(1:1) == char(9) .or. res(1:1) == ' ') .and. len_trim(res)>0)
  res = res(2:)
enddo
end function

!-----------------------------------------------------------------------------------------------------------------------------------
! get_int: get in buffer, the value of attribute 'attrib'
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine get_int(attrib, val)
character(len=*), intent(in)  :: attrib
integer,          intent(out) :: val
integer :: pos, po2

pos = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="')+len_trim(adjustlt(attrib))+2
if (pos <= len_trim(adjustlt(attrib))+2) stop 'Attrib not found'
po2 = index(buffer(pos:len_trim(buffer)), '"')+pos-2
read(buffer(pos:po2),*) val
end subroutine

!-----------------------------------------------------------------------------------------------------------------------------------
! get_char: get in buffer, the value of attribute 'attrib'
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine get_char(attrib, val)
character(len=*), intent(in)  :: attrib
character(len=*), intent(out) :: val
integer :: pos, po2

pos = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="')+len_trim(adjustlt(attrib))+2
if (pos <= len_trim(adjustlt(attrib))+2) stop 'Attrib not found'
po2 = index(buffer(pos:len_trim(buffer)), '"')+pos-2
read(buffer(pos:po2),'(a)') val
end subroutine

!-----------------------------------------------------------------------------------------------------------------------------------
! read_record: read characters in the unit 'Unit_VTK' from position 'from' to read string 'buffer'
! The read action stops when finding a EOR character (char(10))
!-----------------------------------------------------------------------------------------------------------------------------------
function read_record(buffer, from) result(E_IO)
character(len=*),  intent(inout) :: buffer
integer, optional, intent(in)    :: from
integer :: E_IO
character(len=1) :: c
integer :: n, p

n = 1
buffer = ' '
if (present(from)) then
  p = from
else
  inquire(unit=Unit_VTK, iostat=E_IO, pos=p)     
end if
read(unit=Unit_VTK, iostat=E_IO, pos=p) c
do while (c /= char(10) .and. n <= len(buffer))
 buffer(n:n) = c 
 n = n + 1
 read(unit=Unit_VTK, iostat=E_IO) c
enddo
end function

!-----------------------------------------------------------------------------------------------------------------------------------
! move: advance in VTK file inside the mark 'inside', until find the mark 'to_find', 'repeat' times
!-----------------------------------------------------------------------------------------------------------------------------------
function move(inside, to_find, repeat) result(E_IO)
character(len=*), intent(in) :: inside, to_find
integer,          intent(in) :: repeat
integer(I4P)                 :: E_IO 
integer :: n

do !search the beginnig of the mark 'inside' 
  E_IO = read_record(buffer)
  buffer = trim(adjustlt(Upper_Case(buffer)))
  if (index(buffer, '<'//trim(adjustlt(Upper_Case(inside)))) > 0) exit !Mark 'inside' founded once
enddo
n = repeat
do !search 'repeat' times the mark 'to_find'
  E_IO = read_record(buffer)
  buffer = trim(adjustlt(Upper_Case(buffer)))
  if (index(buffer, '</'//trim(adjustlt(Upper_Case(inside)))) > 0) stop 'Too few marks inside'
  if (index(buffer, '<'//trim(adjustlt(Upper_Case(to_find)))) > 0) n = n - 1 !Mark 'to_find' founded once 
  if (n == 0) exit !Mark 'to_find' founded 'repeat' times
enddo
end function move

!-----------------------------------------------------------------------------------------------------------------------------------
! search: search in VTK file from position 'pos' inside the mark 'inside', until find the mark 'to_find', eventually, having 
! attribute 'with_attribute' matching the value 'of_value'
!-----------------------------------------------------------------------------------------------------------------------------------
function search(from, inside, to_find, with_attribute, of_value) result(E_IO)
integer, optional, intent(in) :: from
character(len=*),  intent(in) :: inside, to_find
character(len=*),  intent(in) :: with_attribute, of_value
integer(I4P)                  :: E_IO 
character(maxlen) :: str
integer :: pos

pos = 1; if (present(from)) pos = from
E_IO = read_record(buffer, from=pos)
do !search the beginnig of the mark 'inside' from position 'pos'
  buffer = trim(adjustlt(Upper_Case(buffer)))
  if (index(buffer, '<'//trim(adjustlt(Upper_Case(inside)))) > 0) exit !Mark 'inside' founded once
  E_IO = read_record(buffer)
enddo
do !search 'repeat' times the mark 'to_find'
  E_IO = read_record(buffer)
  buffer = trim(adjustlt(Upper_Case(buffer)))
  if (index(buffer, '</'//trim(adjustlt(Upper_Case(inside)))) > 0) stop 'Mark not found or attribute does not match'
  if (index(buffer, '<'//trim(adjustlt(Upper_Case(to_find)))) > 0) then
    if (len_trim(of_value) == 0) exit !there is no attribute value to seach
    call get_char(with_attribute, str)
    if (trim(adjustlt(Upper_Case(str))) == trim(adjustlt(Upper_Case(of_value)))) exit !Attribute match the value
  end if
enddo
end function

end module
