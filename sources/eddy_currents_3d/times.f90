module times_mod

implicit none

type :: times_struct
integer :: start
integer :: last
end type

contains

subroutine times_start(tt)
implicit none
type(times_struct) :: tt
integer, dimension(3) :: it

 call itime(it)
 tt%start = it(1)*3600+it(2)*60+it(3)
 tt%last = tt%start
 print *, 'time: start'

end subroutine

subroutine times_next(tt)
implicit none
type(times_struct) :: tt
integer, dimension(3) :: it
integer :: newlast

 call itime(it)
 newlast = it(1)*3600+it(2)*60+it(3)
 print *, 'time: total:', newlast - tt%start, 'parcial:', newlast - tt%last
 tt%last = newlast

end subroutine

end module
