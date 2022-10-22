module solve_mumps_mod

use sparse_class

implicit none

INCLUDE 'mpif.h'
INCLUDE 'zmumps_struc.h'

contains

logical function solve_mumps(A, B, X, opt)
implicit none
type(sparse_complex), intent(in) :: A ! only valid for ID 0
complex(real64), dimension(:), intent(in) :: B ! only valid for ID 0
complex(real64), dimension(:), intent(inout) :: X ! only valid for ID 0
integer, intent(in) :: opt ! 0: unsym 1: sym def pos 2: sym 10:sym_def_pos_keep_lower 11:sym_def_pos_keep_upper 12:sym_keep_lower 13:sym_keep_upper !10,11,12,13: matrix is symmetric but all nonzeros are given
TYPE(ZMUMPS_STRUC) :: mumps_par
!INTEGER :: IERR
integer :: i, j, z, nnz, nnz2

 solve_mumps = .true.

!C print *, ' ~~MUMPS~~'

      !CALL MPI_INIT(IERR)
!C Define a communicator for the package.
      mumps_par%COMM = MPI_COMM_WORLD
!C  Initialize an instance of the package
!C  for L U factorization (sym = 0, with working host)
      mumps_par%JOB = -1

        ! 0 unsym
        ! 1 sym pos def
        ! 2 sym
      select case (opt)
        case (0);  mumps_par%SYM = 0
        case (1);  mumps_par%SYM = 1
        case (2);  mumps_par%SYM = 2
        case (10); mumps_par%SYM = 1
        case (11); mumps_par%SYM = 1
        case (12); mumps_par%SYM = 2
        case (13); mumps_par%SYM = 2
        case default
            print *, 'Error: wrong opt'
            solve_mumps = .false.
            return
      end select

        ! 0 host is not involved in factorization/solve phases
        ! 1 host is involved in factorization/solve phases
      mumps_par%PAR = 1

      CALL ZMUMPS(mumps_par)
      if (mumps_par%INFOG(1) /= 0) then
        print *, 'Error in MUMPS (a)', mumps_par%INFOG(1)
        solve_mumps = .false.
        return
      endif
!C  Define problem on the host (processor 0)
      IF ( mumps_par%MYID .eq. 0 ) THEN

        ! aqui, so para o proceso 0 
        if (A%nc /= A%nr) then
            print *, 'Error: matrix is not square'
            solve_mumps = .false.
            return 
        endif

        ! compute number of nonzeros for symmetrization
        if (opt == 10 .or. opt == 12) then
            nnz = 0
            do j = 1, A%nc
                do z = A%colptr(j), A%colptr(j + 1) - 1
                    i = A%rowind(z)
                    if (i>=j) nnz = nnz + 1
                enddo
            enddo
        else if (opt == 11 .or. opt == 13) then
            nnz = 0
            do j = 1, A%nc
                do z = A%colptr(j), A%colptr(j + 1) - 1
                    i = A%rowind(z)
                    if (i<=j) nnz = nnz + 1
                enddo
            enddo
        else
            nnz = A%nnz
        endif

        mumps_par%N = A%nc
        mumps_par%NZ = nnz
        ALLOCATE( mumps_par%IRN ( mumps_par%NZ ) )
        ALLOCATE( mumps_par%JCN ( mumps_par%NZ ) )
        ALLOCATE( mumps_par%A   ( mumps_par%NZ ) )
        ALLOCATE( mumps_par%RHS ( mumps_par%N  ) )

        if (opt == 10 .or. opt == 12) then ! pass lower half
            nnz2 = 0
            do j = 1, A%nc
                do z = A%colptr(j), A%colptr(j + 1) - 1
                    i = A%rowind(z)
                    if (i>=j) then
                        nnz2 = nnz2 + 1
                        mumps_par%IRN(nnz2) = i
                        mumps_par%JCN(nnz2) = j
                        mumps_par%A(nnz2) = A%nzval(z)
                    endif
                enddo
            enddo
        else if (opt == 11 .or. opt == 13) then ! pass upper half
            nnz2 = 0
            do j = 1, A%nc
                do z = A%colptr(j), A%colptr(j + 1) - 1
                    i = A%rowind(z)
                    if (i<=j) then
                        nnz2 = nnz2 + 1
                        mumps_par%IRN(nnz2) = i
                        mumps_par%JCN(nnz2) = j
                        mumps_par%A(nnz2) = A%nzval(z)
                    endif
                enddo
            enddo
        else ! pass all
            mumps_par%IRN = A%rowind(1:A%nnz)
            do j = 1, A%nc
                do z = A%colptr(j), A%colptr(j + 1) - 1
                    mumps_par%JCN(z) = j
                enddo
            enddo
            mumps_par%A = A%nzval(1:A%nnz)
        endif

        mumps_par%RHS = B(1:mumps_par%N)
      END IF

! to fix error -9
        ! percentage increase in the estimated working space.
        ! the default value is 20 (which corresponds to a 20 % increase).
        ! malla0 30
        ! malla1 60
        ! malls2 90 a _100_ ?
      mumps_par%ICNTL(14) = 100

!C  Call package for solution
      mumps_par%JOB = 6

      CALL ZMUMPS(mumps_par)

      if (mumps_par%INFOG(1) /= 0) then
        print *, 'Error in MUMPS (b)', mumps_par%INFOG(1)
        solve_mumps = .false.
        return
      endif
!C  Solution has been assembled on the host
      IF ( mumps_par%MYID .eq. 0 ) THEN
        X = mumps_par%RHS
      END IF
!C  Deallocate user data
      IF ( mumps_par%MYID .eq. 0 ) THEN
        DEALLOCATE( mumps_par%IRN )
        DEALLOCATE( mumps_par%JCN )
        DEALLOCATE( mumps_par%A   )
        DEALLOCATE( mumps_par%RHS )
      END IF
!C  Destroy the instance (deallocate internal data structures)
      mumps_par%JOB = -2

      CALL ZMUMPS(mumps_par)
      if (mumps_par%INFOG(1) /= 0) then
        print *, 'Error in MUMPS (c)', mumps_par%INFOG(1)
        solve_mumps = .false.
        return
      endif
      !CALL MPI_FINALIZE(IERR)
 
!C print *, ' ~~MUMPS~~ end'

end function

end module
