module vector

use defines

implicit none

type :: char_text
    character, dimension(:), allocatable :: value
end type

! coidado con tamanho de vector 0

interface vector_append_inc; module procedure vector_append_inc_int32;  end interface
interface vector_append_inc; module procedure vector_append_inc_int64;  end interface
interface vector_append_inc; module procedure vector_append_inc_real32; end interface
interface vector_append_inc; module procedure vector_append_inc_real64; end interface
interface vector_append_inc; module procedure vector_append_inc_complex64; end interface
interface vector_append; module procedure vector_append_logical;  end interface
interface vector_append; module procedure vector_append_int64;  end interface
interface vector_append; module procedure vector_append_int32;  end interface
interface vector_append; module procedure vector_append_real64; end interface
interface vector_append; module procedure vector_append_real32; end interface
interface vector_append; module procedure vector_append_complex64; end interface
interface pvector_append; module procedure pvector_append_int32;  end interface
interface pvector_append; module procedure pvector_append_real64; end interface
interface pvector_append; module procedure pvector_append_complex64; end interface
interface vector_append; module procedure vector_append_char;  end interface
interface vector_append; module procedure vector_append_chara;  end interface
interface vector_append_vector; module procedure vector_append_vector_int32;  end interface
interface vector_append_vector; module procedure vector_append_vector_real64; end interface
interface vector_append_vector; module procedure vector_append_vector_complex64; end interface

interface vector_adjust; module procedure vector_adjust_int64;  end interface
interface vector_adjust; module procedure vector_adjust_int32;  end interface
interface vector_adjust; module procedure vector_adjust_real64; end interface
interface vector_adjust; module procedure vector_adjust_real32; end interface
interface vector_adjust; module procedure vector_adjust_complex64; end interface
interface vector_adjust; module procedure vector_adjust_char;  end interface
interface vector_adjust; module procedure vector_adjust_chara;  end interface

private :: vector_append_int32, vector_append_int64, &
    vector_append_real32, vector_append_real64, vector_append_complex64, &
    vector_append_inc_int32, vector_append_inc_int64, &
    vector_append_inc_real32, vector_append_inc_real64, vector_append_inc_complex64, &
    vector_adjust_int32, vector_adjust_int64, &
    vector_adjust_real32, vector_adjust_real64, vector_adjust_complex64

contains

subroutine vector_append_inc_int32(length, vector, value)
implicit none
integer, intent(INOUT) :: length ! old length
integer(4), allocatable, dimension(:), intent(INOUT) :: vector
integer(4), intent(IN) :: value

 call vector_append_int32(length, vector, value)
 length = length + 1

end subroutine vector_append_inc_int32


subroutine vector_append_inc_int64(length, vector, value)
implicit none
integer, intent(INOUT) :: length ! old length
integer(8), allocatable, dimension(:), intent(INOUT) :: vector
integer(8), intent(IN) :: value

 call vector_append_int64(length, vector, value)
 length = length + 1

end subroutine vector_append_inc_int64



subroutine vector_append_inc_real32(length, vector, value)
implicit none
integer, intent(INOUT) :: length ! old length
real(real32), allocatable, dimension(:), intent(INOUT) :: vector
real(real32), intent(IN) :: value

 call vector_append_real32(length, vector, value)
 length = length + 1

end subroutine vector_append_inc_real32



subroutine vector_append_inc_real64(length, vector, value)
implicit none
integer, intent(INOUT) :: length ! old length
real(real64), allocatable, dimension(:), intent(INOUT) :: vector
real(real64), intent(IN) :: value

 call vector_append_real64(length, vector, value)
 length = length + 1

end subroutine vector_append_inc_real64



subroutine vector_append_inc_complex64(length, vector, value)
implicit none
integer, intent(INOUT) :: length ! old length
complex(real64), allocatable, dimension(:), intent(INOUT) :: vector
complex(real64), intent(IN) :: value

 call vector_append_complex64(length, vector, value)
 length = length + 1

end subroutine vector_append_inc_complex64



subroutine vector_append_logical(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
logical, allocatable, dimension(:), intent(inout) :: vector
logical, intent(IN) :: value
logical, allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_logical



subroutine vector_append_int64(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
integer(8), allocatable, dimension(:), intent(inout) :: vector
integer(8), intent(IN) :: value
integer(8), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_int64

subroutine vector_append_int32(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
integer(4), allocatable, dimension(:), intent(inout) :: vector
integer(4), intent(IN) :: value
integer(4), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_int32
! gfortran does not allow pure move_alloc ! my bug: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46411
pure subroutine pvector_append_int32(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
integer, allocatable, dimension(:), intent(inout) :: vector
integer, intent(IN) :: value
integer, allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    allocate(vector(newsize))
    vector = temp
    deallocate(temp)
endif

vector(1+length) = value

end subroutine pvector_append_int32


subroutine vector_append_real32(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
real(real32), allocatable, dimension(:), intent(inout) :: vector
real(real32), intent(IN) :: value
real(real32), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder

    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_real32


subroutine vector_append_real64(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
real(real64), allocatable, dimension(:), intent(inout) :: vector
real(real64), intent(IN) :: value
real(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder

    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_real64
! gfortran does not allow pure move_alloc ! my bug: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46411
pure subroutine pvector_append_real64(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
real(real64), allocatable, dimension(:), intent(inout) :: vector
real(real64), intent(IN) :: value
real(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder

    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    allocate(vector(newsize))
    vector = temp
    deallocate(temp)
endif

vector(1+length) = value

end subroutine pvector_append_real64



subroutine vector_append_complex64(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
complex(real64), allocatable, dimension(:), intent(inout) :: vector
complex(real64), intent(IN) :: value
complex(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder

    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine vector_append_complex64
! gfortran does not allow pure move_alloc ! my bug: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46411
pure subroutine pvector_append_complex64(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
complex(real64), allocatable, dimension(:), intent(inout) :: vector
complex(real64), intent(IN) :: value
complex(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder

    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    allocate(vector(newsize))
    vector = temp
    deallocate(temp)
endif

vector(1+length) = value

end subroutine pvector_append_complex64



! reduce el tamanho del vector al minimo
subroutine vector_adjust_int32(length, vector)
implicit none
integer, intent(IN) :: length
integer(4), allocatable, dimension(:), intent(inout) :: vector
integer(4), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine vector_adjust_int32


! reduce el tamanho del vector al minimo
subroutine vector_adjust_int64(length, vector)
implicit none
integer, intent(IN) :: length
integer(8), allocatable, dimension(:), intent(inout) :: vector
integer(8), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine vector_adjust_int64


! reduce el tamanho del vector al minimo
subroutine vector_adjust_real32(length, vector)
implicit none
integer, intent(IN) :: length
real(real32), allocatable, dimension(:), intent(inout) :: vector
real(real32), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine vector_adjust_real32



! reduce el tamanho del vector al minimo
subroutine vector_adjust_real64(length, vector)
implicit none
integer, intent(IN) :: length
real(real64), allocatable, dimension(:), intent(inout) :: vector
real(real64), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine vector_adjust_real64



! reduce el tamanho del vector al minimo
subroutine vector_adjust_complex64(length, vector)
implicit none
integer, intent(IN) :: length
complex(real64), allocatable, dimension(:), intent(inout) :: vector
complex(real64), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine vector_adjust_complex64



subroutine vector_append_vector_int32(length, vector, inc, values)
implicit none
integer, intent(IN) :: length ! old length
integer(4), allocatable, dimension(:), intent(inout) :: vector
integer, intent(IN) :: inc
integer(4), dimension(:), intent(IN) :: values
integer(4), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+inc))

if (size(vector,1) < length + inc) then
    newsize = size(vector,1) * 2
    if (newsize < length+inc) newsize = length+inc ! pode suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(length+1:length+inc) = values(1:inc)

end subroutine vector_append_vector_int32


subroutine vector_append_vector_int64(length, vector, inc, values)
implicit none
integer, intent(IN) :: length ! old length
integer(8), allocatable, dimension(:), intent(inout) :: vector
integer, intent(IN) :: inc
integer(8), dimension(:), intent(IN) :: values
integer(8), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+inc))

if (size(vector,1) < length + inc) then
    newsize = size(vector,1) * 2
    if (newsize < length+inc) newsize = length+inc ! pode suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(length+1:length+inc) = values(1:inc)

end subroutine vector_append_vector_int64


subroutine vector_append_vector_real64(length, vector, inc, values)
implicit none
integer, intent(IN) :: length ! old length
real(real64), allocatable, dimension(:), intent(inout) :: vector
integer, intent(IN) :: inc
real(real64), dimension(:), intent(IN) :: values
real(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+inc))

if (size(vector,1) < length + inc) then
    newsize = size(vector,1) * 2
    if (newsize < length+inc) newsize = length+inc ! pode suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(length+1:length+inc) = values(1:inc)

end subroutine vector_append_vector_real64

subroutine vector_append_vector_complex64(length, vector, inc, values)
implicit none
integer, intent(IN) :: length ! old length
complex(real64), allocatable, dimension(:), intent(inout) :: vector
integer, intent(IN) :: inc
complex(real64), dimension(:), intent(IN) :: values
complex(real64), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+inc))

if (size(vector,1) < length + inc) then
    newsize = size(vector,1) * 2
    if (newsize < length+inc) newsize = length+inc ! pode suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(length+1:length+inc) = values(1:inc)

end subroutine vector_append_vector_complex64


subroutine vector_append_char(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
character, allocatable, dimension(:), intent(inout) :: vector
character, intent(IN) :: value
character, allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine




subroutine vector_append_chara(length, vector, value)
implicit none
integer, intent(IN) :: length ! old length
type(char_text), allocatable, dimension(:), intent(inout) :: vector
type(char_text), intent(IN) :: value
type(char_text), allocatable, dimension(:) :: temp
integer :: newsize

if (.not. allocated(vector)) allocate(vector(length+1))

if (size(vector,1) <= length) then
    newsize = size(vector,1) * 2
    if (newsize < length+1) newsize = length+1 ! non debera suceder
    allocate(temp(newsize))
    temp(1:size(vector,1)) = vector
    deallocate(vector)
    call move_alloc(temp, vector)
endif

vector(1+length) = value

end subroutine


! reduce el tamanho del vector al minimo
subroutine vector_adjust_char(length, vector)
implicit none
integer, intent(IN) :: length
character, allocatable, dimension(:), intent(inout) :: vector
character, allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine


! reduce el tamanho del vector al minimo
subroutine vector_adjust_chara(length, vector)
implicit none
integer, intent(IN) :: length
type(char_text), allocatable, dimension(:), intent(inout) :: vector
type(char_text), allocatable, dimension(:) :: temp

if (.not. allocated(vector)) allocate(vector(length))

if (size(vector,1) /= length) then
    allocate(temp(length))
    temp = vector(1:length)
    deallocate(vector)
    call move_alloc(temp, vector)
endif

end subroutine



end module vector

