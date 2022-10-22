module sparse_aux

implicit none

contains

!--------------------------------------------
! find X between A(From) and A(To), inclusive
!--------------------------------------------
Integer Function BinarySearch(A,X,From,To)
      ! Search for the value X in A(From)..A(To), inclusive
       Integer, intent(in) :: A(:),X          !The array is indexed 1 to ?
       Integer, intent(in) :: From,To         !Stated inclusive range.
       Integer L,R,P
        L = From - 1           !Exclusive bounds,
        R = To + 1             !To search elements From to To.
    1   P = (R - L)/2          !Probe; integer division. Not (L + R)/2!
        if (P <= 0) then       !Search exhausted.
            BinarySearch = -1
            return
        endif
        P = L + P              !Convert an offset from L to an array index.
        if (X - A(P)) 3,4,2    !Test: negative,zero,positive.
    2   L = P                  !A(P) < X. Shift the left bound up.
        go to 1
    3   R = P                  !X < A(P). Shift the right bound down.
        go to 1
    4   BinarySearch = P
        return                 !X = A(P). Found at index P.
End Function BinarySearch


!-----------------------------------------------------------------------
! sperror: report an error
!-----------------------------------------------------------------------
subroutine sperror(err)
 implicit none
 character(len = *), intent(in) :: err
 write(0,'(a)') 'ERROR: '//trim(err)
 stop 1
end subroutine


end module
