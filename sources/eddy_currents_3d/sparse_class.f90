module sparse_class

!-----------------------------------------------------------------------
! Module for sparse matrix management
!
! Licensing: This code is distributed under the GNU GPL license.
! Author: Francisco Pena, fran.pena@usc.es
! Author: Rodrigo Valiña Gutiérrez, rodrigo.valina@usc.es
! Last update: 16/09/2010
!
! PUBLIC TYPE-BOUND PROCEDURES:
!   build: allocate memory
!   set: set values in matrix
!   add: add values to matrix
!   get: get values from matrix
!-----------------------------------------------------------------------

use sparse_real_class
use sparse_complex_class

implicit none

!Constants
!integer, parameter, private :: real64 = selected_real_kind(15, 307) !double precision

!Types

!Interfaces
interface sparse_free; module procedure sparse_real_free; end interface
interface sparse_free; module procedure sparse_complex_free; end interface
interface sparse_copy; module procedure sparse_real_copy; end interface
interface sparse_copy; module procedure sparse_complex_copy; end interface
interface sparse_equal; module procedure sparse_real_equal; end interface
interface sparse_equal; module procedure sparse_complex_equal; end interface
interface sparse_print; module procedure sparse_real_print; end interface
interface sparse_print; module procedure sparse_complex_print; end interface
interface sparse_print_s; module procedure sparse_real_print_s; end interface
interface sparse_print_s; module procedure sparse_complex_print_s; end interface
interface sparse_build; module procedure sparse_real_build; end interface
interface sparse_build; module procedure sparse_complex_build; end interface
interface sparse_build1; module procedure sparse_real_build1; end interface
interface sparse_build1; module procedure sparse_complex_build1; end interface
interface sparse_build2; module procedure sparse_real_build2; end interface
interface sparse_build2; module procedure sparse_complex_build2; end interface
interface sparse_collapse; module procedure sparse_real_collapse; end interface
interface sparse_collapse; module procedure sparse_complex_collapse; end interface

interface sparse_from_dense; module procedure sparse_real_from_dense; end interface
interface sparse_from_dense; module procedure sparse_complex_from_dense; end interface
interface sparse_to_dense; module procedure sparse_real_to_dense; end interface
interface sparse_to_dense; module procedure sparse_complex_to_dense; end interface

interface sparse_add; module procedure sparse_real_add; end interface
interface sparse_add; module procedure sparse_complex_add; end interface
interface sparse_set; module procedure sparse_real_set; end interface
interface sparse_set; module procedure sparse_complex_set; end interface
interface sparse_get; module procedure sparse_real_get; end interface
interface sparse_get; module procedure sparse_complex_get; end interface
interface sparse_get_element; module procedure sparse_real_get_element; end interface
interface sparse_get_element; module procedure sparse_complex_get_element; end interface
interface sparse_get_col_sparse; module procedure sparse_real_get_col_sparse; end interface
interface sparse_get_col_sparse; module procedure sparse_complex_get_col_sparse; end interface
interface sparse_get_col; module procedure sparse_real_get_col; end interface
interface sparse_get_col; module procedure sparse_complex_get_col; end interface
interface sparse_get_row_sparse; module procedure sparse_real_get_row_sparse; end interface
interface sparse_get_row_sparse; module procedure sparse_complex_get_row_sparse; end interface
interface sparse_get_row; module procedure sparse_real_get_row; end interface
interface sparse_get_row; module procedure sparse_complex_get_row; end interface

interface sparse_scale; module procedure sparse_real_scale; end interface
interface sparse_scale; module procedure sparse_complex_scale; end interface
interface sparse_scale_row; module procedure sparse_real_scale_row; end interface
interface sparse_scale_row; module procedure sparse_complex_scale_row; end interface
interface sparse_sum; module procedure sparse_real_sum; end interface
interface sparse_sum; module procedure sparse_complex_sum; end interface
! sparse_multiply_b and sparse_multiply_bt are much slower (~5x) than sparse_multiply_a and sparse_multiply_at
! sparse_multiply_at gives C = A' * B
! sparse_multiply_a transposes A and calls sparse_multiply_at, giving C = A * B
interface sparse_multiply_a; module procedure sparse_real_multiply_a; end interface ! C = A * B
interface sparse_multiply_a; module procedure sparse_complex_multiply_a; end interface ! C = A * B
interface sparse_multiply_b; module procedure sparse_real_multiply_b; end interface ! C = A * B
interface sparse_multiply_b; module procedure sparse_complex_multiply_b; end interface ! C = A * B
interface sparse_multiply_c; module procedure sparse_real_multiply_c; end interface ! C = A * B
interface sparse_multiply_c; module procedure sparse_complex_multiply_c; end interface ! C = A * B
interface sparse_multiply_at; module procedure sparse_real_multiply_at; end interface ! C = A' * B
interface sparse_multiply_at; module procedure sparse_complex_multiply_at; end interface ! C = A' * B
interface sparse_multiply_bt; module procedure sparse_real_multiply_bt; end interface ! C = A' * B
interface sparse_multiply_bt; module procedure sparse_complex_multiply_bt; end interface ! C = A' * B
interface sparse_multiply_ct; module procedure sparse_real_multiply_ct; end interface ! C = A' * B
interface sparse_multiply_ct; module procedure sparse_complex_multiply_ct; end interface ! C = A' * B
! choose _a or _b or _c.
! in general, these should be called and not _a or _b directly, to allow changing implementation without changing users.
interface sparse_multiply; module procedure sparse_real_multiply_c; end interface ! C = A * B
interface sparse_multiply; module procedure sparse_complex_multiply_c; end interface ! C = A * B
interface sparse_multiply_t; module procedure sparse_real_multiply_ct; end interface ! C = A' * B
interface sparse_multiply_t; module procedure sparse_complex_multiply_ct; end interface ! C = A' * B

interface sparse_transpose; module procedure sparse_real_transpose; end interface
interface sparse_transpose; module procedure sparse_complex_transpose; end interface

interface sparse_remove_col; module procedure sparse_real_remove_col; end interface
interface sparse_remove_col; module procedure sparse_complex_remove_col; end interface
interface sparse_remove_cols; module procedure sparse_real_remove_cols; end interface
interface sparse_remove_cols; module procedure sparse_complex_remove_cols; end interface
interface sparse_remove_rows; module procedure sparse_real_remove_rows; end interface
interface sparse_remove_rows; module procedure sparse_complex_remove_rows; end interface
interface sparse_remove; module procedure sparse_real_remove; end interface
interface sparse_remove; module procedure sparse_complex_remove; end interface
! sparse matrix creation
interface sparse_identity; module procedure sparse_real_identity; end interface
interface sparse_identity; module procedure sparse_complex_identity; end interface
interface sparse_row; module procedure sparse_real_row; end interface
interface sparse_row; module procedure sparse_complex_row; end interface
interface sparse_col; module procedure sparse_real_col; end interface
interface sparse_col; module procedure sparse_complex_col; end interface
interface sparse_one; module procedure sparse_real_one; end interface
interface sparse_one; module procedure sparse_complex_one; end interface
! log
interface sparse_print_to_file; module procedure sparse_real_print_to_file; end interface
interface sparse_print_to_file; module procedure sparse_complex_print_to_file; end interface
! other
! sparse_upper_triangular_diagonal: for pardiso with transpose
interface sparse_upper_triangular_diagonal; module procedure sparse_real_upper_triangular_diagonal; end interface
interface sparse_upper_triangular_diagonal; module procedure sparse_complex_upper_triangular_diagonal; end interface
interface sparse_lower_triangular; module procedure sparse_real_lower_triangular; end interface
interface sparse_lower_triangular; module procedure sparse_complex_lower_triangular; end interface
interface sparse_test; module procedure sparse_real_test; end interface
interface sparse_test; module procedure sparse_complex_test; end interface
interface sparse_symmetric_error; module procedure sparse_real_symmetric_error; end interface
interface sparse_symmetric_error; module procedure sparse_complex_symmetric_error; end interface
interface sparse_0_to_1; module procedure sparse_real_0_to_1; end interface
interface sparse_0_to_1; module procedure sparse_complex_0_to_1; end interface
interface sparse_1_to_0; module procedure sparse_real_1_to_0; end interface
interface sparse_1_to_0; module procedure sparse_complex_1_to_0; end interface
interface sparse_add_last_column; module procedure sparse_real_add_last_column; end interface




!private procedures

contains


!----------
! copy data
!----------
subroutine sparse_real_to_complex(this, other)
implicit none
type(sparse_real), intent(in) :: this
type(sparse_complex), intent(inout) :: other
 call sparse_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = this%nzval(1:this%nnz)
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


!----------
! copy data
!----------
subroutine sparse_real_to_complex_imag(this, other)
implicit none
type(sparse_real), intent(in) :: this
type(sparse_complex), intent(inout) :: other
 call sparse_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = (0.0d0,1.0d0) * this%nzval(1:this%nnz)
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


!-------------------------------
! copy real part of complex data
!-------------------------------
subroutine sparse_complex_to_real_real(this, other)
implicit none
type(sparse_complex), intent(in) :: this
type(sparse_real), intent(inout) :: other
 call sparse_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = dreal(this%nzval(1:this%nnz))
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


!------------------------------------
! copy imaginary part of complex data
!------------------------------------
subroutine sparse_complex_to_real_imag(this, other)
implicit none
type(sparse_complex), intent(in) :: this
type(sparse_real), intent(inout) :: other
 call sparse_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = dimag(this%nzval(1:this%nnz))
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


!------------------------------------
! copy absolute value of complex data
!------------------------------------
subroutine sparse_complex_to_real_abs(this, other)
implicit none
type(sparse_complex), intent(in) :: this
type(sparse_real), intent(inout) :: other
 call sparse_free(other)
 other%nr = this%nr
 other%nc = this%nc
 other%nnz = this%nnz
 allocate(other%nzval(this%nnz))
 allocate(other%rowind(this%nnz))
 allocate(other%colptr(this%nc+1))
 other%nzval = abs(this%nzval(1:this%nnz))
 other%rowind = this%rowind(1:this%nnz)
 other%colptr = this%colptr(1:this%nc+1)
end subroutine


end module sparse_class
