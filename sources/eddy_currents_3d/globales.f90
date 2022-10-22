module globales

use mesh
use sparse_class

implicit none

type domain_reference
    integer :: refnum
    logical :: cond_not_diel ! true conductor; false dielectric
    real(real64) :: electrical_conductivity
    real(real64) :: relative_magnetic_permeability
end type domain_reference

type intensity_input
    ! referencias de las aristas que delimitan la zona de entrada de la intensidad
    integer, dimension(:), allocatable :: boundary_references
    ! intensidad entrante
    real(real64) :: intensity
    real(real64) :: phase
end type intensity_input

type ref_fronts
    integer, dimension(:), allocatable :: diel_refs
end type

real(real64), parameter :: mu0 = 12.566368e-7

!
! novos
!

 character(len=1024) :: filenamecond, filenamediel
 type(mfm_data) :: meshcond, meshdiel ! cambiar de meshcond a meshtotal

 type(domain_reference), dimension(:), allocatable :: references
 type(intensity_input), dimension(:), allocatable :: inputs
 integer :: num_inputs

 integer :: conductor_num
 integer, dimension(:), allocatable :: conductor_refs ! of meshcond
 real(real64), dimension(:), allocatable :: conductor_sigmas
 integer :: dielectric_num
 integer, dimension(:), allocatable :: dielectric_refs ! of meshcond
 real(real64), dimension(:), allocatable :: dielectric_mus
 integer :: all_num
 integer, dimension(:), allocatable :: all_refs ! of meshcond
 logical, dimension(:), allocatable :: all_is_cond ! true <=> conductor
 real(real64), dimension(:), allocatable :: all_mus
 real(real64), dimension(:), allocatable :: all_sigmas

 ! index of domains of dielectric in meshcond
 ! tamanho ajustado
 integer, dimension(:), allocatable :: dofdiel ! meshcond

 ! referencias de las caras de la frontera del dielectrico en la malla del dielectrico
 ! tamanho ajustado
 type(ref_fronts), dimension(:), allocatable :: ref_front_omegaD ! meshdiel
 type(ref_fronts), dimension(:), allocatable :: nod_comp_con ! meshdiel

 integer, dimension(:), allocatable :: nod_frontera_total
 integer :: ncm1

 real(real64) :: freq
 real(real64) :: omega1

!%*****************************************************************************%
!% Malla y otros
!%*****************************************************************************%

integer, dimension(:), allocatable :: nod_frontera ! no tiene valores repetidos
integer :: caras_int

! refsigma -> sigma_refs
! refcond -> conductor_refs
! refdiel -> dielectric_refs
! RADIO

! -> meshcond ! nel, nver, nnod, mm, nn, nrc, nra, nrv, zz, nsd
! -> meshdiel ! neld, nverd, nnodd_caras, mmd, nnnd, nrcd, nrad, nrvd, zd, nsdd

!local nod_int 
!local tot_diel
!local ipost

!global nnodd ref_a_total



!%*****************************************************************************%
!% Vectores asociados con la orientacion de las aristas y                      %
!% vector asociado con la numeracion de los vertices en cada cara              %
!%*****************************************************************************%

integer, dimension(:,:), allocatable :: sga, exa
integer, dimension(:), allocatable :: ref_aristas, sel_refs, iflag

!integer, dimension(4,3) :: indc
integer, dimension(2,6) :: e !global inda !global e

!%*****************************************************************************%
!% Condicion Intensidades
!%*****************************************************************************%

type(sparse_real) :: D
type(sparse_complex) :: zerosunos

!%*****************************************************************************%
!% Matrices y segundo miembro                                                 %
!%*****************************************************************************%

! kk -> no necesaria (global) para mlocalt => pasar como parametro
!global H Z p
type(sparse_real) :: M
type(sparse_real) :: Kc
type(sparse_real) :: N
type(sparse_real) :: P
! MT KT en matriz_Nedelec
real(real64), dimension(:), allocatable :: volT ! calculada en matriz_Nedelec -> mlocalt

!%*****************************************************************************%
!%  Errores
!%*****************************************************************************%

!global l2_Htot l2_curlHtot l2_Htots l2_curlHtots error_porc

!global InterpoladaNede
!global l2_Hc l2_curlHc l2_Hcs l2_curlHcs l2_Hd l2_Hds l2_curlHd

!%*****************************************************************************%
!%  auxiliar condicion intensidad y test
!%*****************************************************************************%

!local vec_aux
!para_comprobacions: ipost_sale vec_aux_sale

!%*****************************************************************************%
!%  Post Proceso -pintar-
!%*****************************************************************************%

!global Hver Hmod

contains

subroutine globales_init()

!%******************************************************************************
!% Definicion de los numeros de los vertices que pertenecen a cada una de las  %
!% caras y que se definen teniendo en cuenta la numeracion de MODULEF          %
!%******************************************************************************

! non utilizado !
!indc(1,:) = (/ 1, 2, 3 /)
!indc(2,:) = (/ 1, 3, 4 /)
!indc(3,:) = (/ 1, 2, 4 /)
!indc(4,:) = (/ 2, 3, 4 /)

!%******************************************************************************
!% Definicon de los numeros de los vertices que pertenecen a cada una de las   %
!% aristas y que se definen teniendo en cuenta la numeracion de MODULEF        %
!%******************************************************************************

e(1,:) = (/ 1, 2, 3, 1, 2, 3 /)
e(2,:) = (/ 2, 3, 1, 4, 4, 4 /)

end subroutine globales_init

subroutine globales_free()
 implicit none
 integer :: i

 call mesh_free(meshcond)
 call mesh_free(meshdiel)
 if (allocated(references)) deallocate(references)
 if (allocated(inputs)) then
    do i = 1, size(inputs,1)
        if (allocated(inputs(i)%boundary_references)) deallocate(inputs(i)%boundary_references)
    enddo
    deallocate(inputs)
 endif
 if (allocated(conductor_refs)) deallocate(conductor_refs)
 if (allocated(conductor_sigmas)) deallocate(conductor_sigmas)
 if (allocated(dielectric_refs)) deallocate(dielectric_refs)
 if (allocated(dielectric_mus)) deallocate(dielectric_mus)
 if (allocated(all_refs)) deallocate(all_refs)
 if (allocated(all_is_cond)) deallocate(all_is_cond)
 if (allocated(all_mus)) deallocate(all_mus)
 if (allocated(all_sigmas)) deallocate(all_sigmas)
 if (allocated(dofdiel)) deallocate(dofdiel)
 if (allocated(ref_front_omegaD)) deallocate(ref_front_omegaD)
 if (allocated(nod_frontera)) deallocate(nod_frontera)
 if (allocated(sga)) deallocate(sga)
 if (allocated(exa)) deallocate(exa)
 if (allocated(ref_aristas)) deallocate (ref_aristas)
 if (allocated(iflag)) deallocate (iflag)
 if (allocated(volT)) deallocate (volT)

 call sparse_free(P)
 call sparse_free(N)
 call sparse_free(M)
 call sparse_free(Kc)
 call sparse_free(D)
 call sparse_free(zerosunos)

end subroutine globales_free

end module globales

