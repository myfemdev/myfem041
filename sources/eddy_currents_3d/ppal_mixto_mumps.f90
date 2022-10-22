program eddy_currents_3d

use globales
use mesh
use matriz_Nedelec_mod
use matriz_Lagrange_mod
use dielectrico_caras_mod
use matriz_Crouzeix_Raviart_mod
use condicion_intensidad_general_mod
use postproceso_mod
use ensamblar_mod
use solve_mumps_mod
use comprobaciones
use readxml_mod
use leedata_mod
use times_mod
use sparse_class

implicit none

! INCLUDE 'mpif.h'

 type(times_struct) :: tt
 logical :: ls
 integer :: res
 integer :: tamatotal
 ! complexas
 type(sparse_complex) :: Atot
 ! complexas
 type(sparse_complex) :: AA, temp1, temp2

 complex(real64), dimension(:), allocatable :: H
 complex(real64), dimension(:), allocatable :: Htot
 complex(real64), dimension(:), allocatable :: btot

 type(sparse_complex) :: Htot_s, H_s, tempH

!!
 type(sparse_complex) :: Dc, Nc
 type(sparse_complex) :: temp3
 type(sparse_complex) :: temp4
 type(sparse_complex) :: tempzu
 type(sparse_complex) :: m11
 type(sparse_real) :: m12
 type(sparse_real) :: m21
 type(sparse_real) :: m32
 type(sparse_complex) :: temp5
 integer :: ierr, proc_id

    CALL MPI_INIT(IERR)
    if (ierr /= 0) then
        print *, 'Error in MPI_INIT', ierr
        stop 1
    endif

    call MPI_Comm_rank(MPI_COMM_WORLD, proc_id, ierr)
    if (ierr /= 0) then
        print *, 'Error in MPI_COMM_RANK', ierr
        stop 1
    endif

!    print *, 'Start process', proc_id

if (proc_id == 0) then

    call times_start(tt)

 call globales_init()

!%******************************************************************************
!%     Lectura de datos / malla                                                %
!%******************************************************************************

 if (command_argument_count() == 0) then
    call leedata()
 else
    call readxml()
 endif

 ! comprobacion de datos de entrada: sirve para leedata y readxml
 if (.not. comprueba()) then
    write(error_unit,*) 'Input data check failed'
    stop 1
 else
    write(output_unit,*) 'Input data check passed'
 endif

! print *, ' malla total'

 res = mesh_read(filenamecond, meshcond)
 if (res /= 0) then
    write (error_unit,*), 'Error', res, 'reading conductor mesh ', trim(filenamecond)
    stop 1
 endif
 if (meshcond%nnod == meshcond%nver .or. meshcond%dim /= 3 .or. meshcond%lnn /= 6 .or.&
    &meshcond%lnv /= 4 .or. meshcond%lne /= 6 .or. meshcond%lnf /= 4) then
    write (error_unit,*), 'Error: invalid conductor mesh type'
    stop 1
 endif


! print *, ' malla dielectrico'

 res = mesh_read(filenamediel, meshdiel)
 if (res /= 0) then
    write (error_unit,*), 'Error', res, 'reading dielectric mesh ', trim(filenamediel)
    stop 1
 endif
 if (meshdiel%nnod == meshdiel%nver .or. meshdiel%dim /= 3 .or. meshdiel%lnn /= 4 .or.&
    &meshdiel%lnv /= 4 .or. meshdiel%lne /= 6 .or. meshdiel%lnf /= 4) then
    write (error_unit,*), 'Error: invalid dielectric mesh type'
    stop 1
 endif

!%******************************************************************************
!%     Signos y extremos de las aristas                                        %
!%******************************************************************************

 call signo()

! print *, ' matriz_Nedelec.f90'
 call matriz_Nedelec()

 call find_1v(meshcond%nsd, dielectric_refs, dofdiel) ! find dielectric_refs(:) in meshcond%nsd(:)

! print *, ' matriz_Lagrange.f90'
 call matriz_Lagrange()

! print *, ' dielectrico_caras.f90'
 call dielectrico_caras()

! print *, ' matriz_Crouzeix_Raviart.f90'
 call matriz_Crouzeix_Raviart()

! print *, ' condicion_intensidad_general.f90'
 call condicion_intensidad_general()


 tamatotal = (meshcond%nnod-num_inputs) + (3*meshdiel%nel) + caras_int + ncm1

! print *, ' tamatotal', tamatotal, ' = ', (meshcond%nnod-num_inputs), ' + ', (3*meshdiel%nel), ' + ', caras_int , ' + ', ncm1

!AA: meshcond%nnod,meshcond%nnod
 call sparse_real_to_complex_imag(M, temp1) ! temp1 = (0,M)
 call sparse_real_to_complex(Kc, temp2) ! temp2 = (Kc,0)
 call sparse_sum(temp1, temp2, AA)
 call sparse_free(temp1)
 call sparse_free(temp2)

! print *, ' D to complex'
 call sparse_real_to_complex(D, Dc)

! print *, ' multiply Dtc * AA -> temp3'
 call sparse_multiply_t(Dc, AA, temp3) ! tempo
! call sparse_real_to_complex(zerosunos, tempzu)
! print *, ' multiply temp3 * zerosunos -> temp4'
 call sparse_multiply(temp3, zerosunos, temp4)
! print *, ' multiply temp3 * Dc -> m11'
 call sparse_multiply(temp3, Dc, m11) ! tempo

! print *, ' free temp3'
 call sparse_free(temp3)

! print *, ' multiply Dt * N -> m12'
 call sparse_multiply_t(D, N, m12) ! real ! tempo
! print *, ' transpose m12 -> m21'
 call sparse_transpose(m12, m21) ! real
! print *, ' transpose P -> m32'
 call sparse_transpose(P, m32) ! real

 !print *, ' ensamblar'

 ! construir Atot
 call ensamblar(Atot, m11, m12, m21, P, m32, tamatotal)

 call sparse_print_s(Atot)

 ! liberar resto
 call sparse_free(m11)
 call sparse_free(m12)
 call sparse_free(m21)
 call sparse_free(m32)

! print *, ' multiply Nt * zerosunos -> temp5'
 call sparse_real_to_complex(N, Nc)
 call sparse_multiply_t(Nc, zerosunos, temp5)

 ! CAMBIO DE SIGNO
 call sparse_scale(temp4, cmplx(-1.0,0.0d0,real64))
 call sparse_scale(temp5, cmplx(-1.0,0.0d0,real64))

 call ensamblar_b(btot, temp4, temp5, tamatotal)


 call sparse_free(temp4)
 call sparse_free(temp5)

 allocate(Htot(tamatotal))
 Htot = btot

! print *, ' solve start'

    call times_next(tt)

endif ! proc_id == 0

  !call MPI_Barrier(MPI_COMM_WORLD)
  ls = solve_mumps(Atot, btot, Htot, 12)

  if (.not. ls) then
     call error('Error solving')
  endif

if (proc_id == 0) then

    call times_next(tt)


 call sparse_free(Atot)
! print *, ' scale tempzu * -1.0 -> tempzu'
! call sparse_scale(tempzu, cmplx(-1.0,0.0d0,real64))
! print *, ' dense_to_sparse Htot -> Htot_s'
 call sparse_from_dense(Htot_s, reshape(Htot(1:meshcond%nnod-num_inputs),[meshcond%nnod-num_inputs,1]) )
! print *, ' multiply Dc * Htot_s -> tempH'
 call sparse_multiply(Dc, Htot_s, tempH)
! print *, ' sum tempH + tempzu -> H_s'
 call sparse_sum(tempH, zerosunos, H_s)
! print *, ' sparse_get_col H_s -> H'
 allocate(H(meshcond%nnod))
 call sparse_get_col(H_s, 1, H)

 call sparse_free(Dc)
 call sparse_free(tempzu)
 call sparse_free(zerosunos)
 call sparse_free(Htot_s)
 call sparse_free(H_s)
 call sparse_free(tempH)

 call postproceso(H)

 if (allocated(btot)) deallocate(btot)
 if (allocated(Htot)) deallocate(Htot)
 if (allocated(H))    deallocate(H)

 call sparse_free(AA)

 call globales_free()

 call times_next(tt)

endif ! proc_id == 0

    CALL MPI_FINALIZE(IERR)
    if (ierr /= 0) then
        print *, 'Error in MPI_FINALIZE', ierr
        stop 1
    endif

!    print *, 'End process', proc_id

end program

