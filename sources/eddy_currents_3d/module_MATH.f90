module module_MATH

use defines
use vector
use inversa_mod

! modified in 2010-07 by Rodrigo Valiña Gutiérrez
! - use real64 type instead of DOUBLE type
! - added matrix inverse procedure
! - added find_1s, find_1v, find_2s, find_2v, find_1l1: integer
! - added cross3

!-----------------------------------------------------------------------
! Module for mathematical operations
! Last update: 30/07/2009
! Programmer: fran.pena@usc.es
!
! PUBLIC PROCEDURES:
! - det: find the determinant of a square matrix 
!-----------------------------------------------------------------------
implicit none

!Private procedures
private :: swap

contains

!-----------------------------------------------------------------------
! det: find the determinant of a square matrix 
! The equivalent upper triangular matrix is calculated; then, det is
! the product of the diagonal
!-----------------------------------------------------------------------
function det(muser) result(res)
  implicit none
  real(real64), dimension(:,:), intent(in) :: muser
  real(real64), dimension(size(muser,1),size(muser,2)) :: m
  real(real64) :: res
  integer :: sgn, n, k, j
  integer, dimension(2) :: p 

  res = 0; sgn = 1; m = muser; n = size(m, 1)
  do k = 1, n-1
    p = maxloc(abs(m(k:n, k:n)), mask = abs(m(k:n, k:n))>epsilon(m)) 
    if (all(p == 0)) return !no valid pivot
    p = p + k - 1 !position respect to the total matrix
    if (p(1) /= k) then !permute rows
      call swap(m(k,:), m(p(1),:)); sgn = -sgn
    end if   
    if (p(2) /= k) then !permute cols
      call swap(m(:,k), m(:,p(2))); sgn = -sgn
    end if   
    do j = k+1, n !use the pivot
      m(j,:) = m(j,:) - m(j,k) * m(k,:) / m(k,k)
    end do
  end do
  res = sgn * product((/(m(k,k), k=1,n)/)) !calculation of det

end function
      
!--------------------------------------------------------------------
! swap: swap the values of two real arrays
!--------------------------------------------------------------------
subroutine swap(u, v)
  implicit none

  real(real64), dimension(:), intent(inout) :: u, v
  real(real64), dimension(size(u)) :: tmp
  
  tmp = u; u = v; v = tmp
  
end subroutine

! ---

! determinant
!http://www.dreamincode.net/code/snippet1273.htm
! matrix inversion
!http://www.dreamincode.net/code/snippet1272.htm

!!!!!

! modified by Rodrigo Valiña Gutiérrez: REAL -> real64, commented out 'l'

logical function inverse4x4old(M, I)
real(real64), dimension(4,4), intent(in) :: M
real(real64), dimension(4,4), intent(out) :: I
!real(real64), dimension(4,4) :: T
logical :: l
 inverse4x4old = .false.

 l = inv4x4a(M, I)

 if (.not. l) then
!    print *,'2'
    l = inv4x4b(M, I)
 else
!    print *,'1'
 endif

 if (.not. l) then
    print *,'Offending matrix:'
    print *,M(1,:)
    print *,M(2,:)
    print *,M(3,:)
    print *,M(4,:)
    return
 else
!    print *, '--'
!    T = matmul(M, I)
!    print *,T(1,:)
!    print *,T(2,:)
!    print *,T(3,:)
!    print *,T(4,:)
 endif

 inverse4x4old = .true.
end function


logical function inverse4x4(M, I)
real(real64), dimension(4,4), intent(in) :: M
real(real64), dimension(4,4), intent(out) :: I
!real(real64), dimension(4,4) :: T
logical :: l
 inverse4x4 = .false.

 l = inv4x4z(M, I)

 if (.not. l) then
    print *,'Inverse: Offending matrix:'
    print *,M(1,:)
    print *,M(2,:)
    print *,M(3,:)
    print *,M(4,:)
    return
! else
!    print *, '--'
!    T = matmul(M, I)
!    print *,T(1,:)
!    print *,T(2,:)
!    print *,T(3,:)
!    print *,T(4,:)
 endif

 inverse4x4 = .true.
end function

logical function inverse3x4_1(M, I)
real(real64), dimension(3,4), intent(in) :: M
real(real64), dimension(4,4), intent(out) :: I
!real(real64), dimension(4,4) :: T
logical :: l
 inverse3x4_1 = .false.

 l = inv3x4_1(M, I)

 if (.not. l) then
    print *,'Inverse: Offending matrix:'
    print *,1,1,1,1
    print *,M(1,:)
    print *,M(2,:)
    print *,M(3,:)
    return
 endif

 inverse3x4_1 = .true.
end function
!--------



subroutine find_1s(main, value, results)
implicit none
integer, dimension(:), intent(IN) :: main
integer, intent(IN) :: value
integer, dimension(:), allocatable, intent(OUT) :: results
integer :: num, i
    num = 0
    if (allocated(results)) deallocate(results)
    do i=1,size(main,1)
        if (main(i) == value) then
                call vector_append_inc(num, results, i)
        endif
    enddo
    call vector_adjust(num, results)
end subroutine

subroutine find_2s(main, value, resultsR, resultsC)
implicit none
integer, dimension(:,:), intent(IN) :: main
integer, intent(IN) :: value
integer, dimension(:), allocatable, intent(OUT) :: resultsR
integer, dimension(:), allocatable, intent(OUT) :: resultsC
integer :: num, i, j
    num = 0
    if (allocated(resultsR)) deallocate(resultsR)
    if (allocated(resultsC)) deallocate(resultsC)
    do i=1,size(main,2)
        do j=1,size(main,1)
           if (main(j,i) == value) then
                call vector_append(num, resultsR, j)
                call vector_append(num, resultsC, i)
                num = num + 1
           endif
        enddo
    enddo
    call vector_adjust(num, resultsR)
    call vector_adjust(num, resultsC)
end subroutine

subroutine find_1v(main, values, results)
implicit none
integer, dimension(:), intent(IN) :: main
integer, dimension(:), intent(IN) :: values
integer, dimension(:), allocatable, intent(OUT) :: results
integer :: num, i, j
    num = 0
    if (allocated(results)) deallocate(results)
    do i=1,size(main,1)
        do j=1,size(values,1)
            if (main(i) == values(j)) then
                call vector_append_inc(num, results, i)
                exit
            endif
        enddo
    enddo
    call vector_adjust(num, results)
end subroutine

subroutine find_2v(main, values, resultsR, resultsC)
implicit none
integer, dimension(:,:), intent(IN) :: main
integer, dimension(:), intent(IN) :: values
integer, dimension(:), allocatable, intent(OUT) :: resultsR
integer, dimension(:), allocatable, intent(OUT) :: resultsC
integer :: num, i, j, k
    num = 0
    if (allocated(resultsR)) deallocate(resultsR)
    if (allocated(resultsC)) deallocate(resultsC)
    do i=1,size(main,2)
       do j=1,size(main,1)
            do k=1,size(values,1)
               if (main(j,i) == values(k)) then
                    call vector_append(num, resultsR, j)
                    call vector_append(num, resultsC, i)
                    num = num + 1
                    exit
               endif
            enddo
        enddo
    enddo
    call vector_adjust(num, resultsR)
    call vector_adjust(num, resultsC)
end subroutine


integer function find_1l1(main)
implicit none
logical, dimension(:), intent(IN) :: main
integer :: i
    find_1l1 = 0
    do i=1,size(main,1)
        if (main(i)) then
            !print *, 'find_1l1: found:', i
            find_1l1 = i
            return
        endif
    enddo
    print *, 'ERROR: module_MATH: find_1l1: no valid value found. Returning 0'
end function find_1l1


integer function find_1i1(main, value)
implicit none
integer, dimension(:), intent(IN) :: main
integer, intent(IN) :: value
integer :: i
    find_1i1 = 0
    do i=1,size(main,1)
        if (main(i) == value) then
            !print *, 'find_1i1: found:', i
            find_1i1 = i
            return
        endif
    enddo
    print *, 'ERROR: module_MATH: find_1i1: no valid value found. Returning 0'
end function find_1i1


function cross3(a, b)
 implicit none
 real(real64), dimension(3), intent (in) :: a, b
 real(real64), dimension(3) :: cross3
 cross3(1) = a(2) * b(3) - a(3) * b(2)
 cross3(2) = a(3) * b(1) - a(1) * b(3)
 cross3(3) = a(1) * b(2) - a(2) * b(1)
end function cross3


! returns the unique values, sorted
! and then position of the last ocurrence of each unique value
! not optimized
subroutine unique2(input, values, rpos)
 implicit none
 integer, dimension(:), allocatable, intent(in) :: input
 integer, dimension(:), allocatable, intent(out) :: values
 integer, dimension(:), allocatable, intent(out) :: rpos
 integer :: counter
 integer :: minim
 integer :: level
 integer :: pos
 integer :: i

 level = 0 ! any value would do
 counter = 0

 do while ( .true. )
    pos = 0
    do i = 1, size(input,1)
        if ((pos == 0 .or. minim>=input(i)) .and. (counter == 0 .or. input(i)>level)) then
            minim = input(i)
            pos = i
        endif
    enddo
    if (pos == 0) then ! non hai mais
        exit
    else
        call vector_append(counter, values, minim)
        call vector_append(counter, rpos, pos)
        counter = counter + 1
        level = minim
    endif
 enddo

 call vector_adjust(counter, values)
 call vector_adjust(counter, rpos)

end subroutine


end module

