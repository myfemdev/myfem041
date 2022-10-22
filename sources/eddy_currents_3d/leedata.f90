module leedata_mod

use globales
use vector

implicit none

contains

subroutine leedata()
implicit none
integer :: s
real(real64) :: sigma

!
! novos
!

 filenamecond = 'malla3dtotal.mfm'
 filenamediel = 'malladcaras.mfm'

 ! e posible facelos axustados
 sigma = 1.515658d5
 call vector_append(conductor_num, conductor_refs, 2) ! malla total
 call vector_append(conductor_num, conductor_sigmas, sigma)
 conductor_num = conductor_num + 1
 call vector_append(all_num, all_refs, 2) ! malla total
 call vector_append(all_num, all_mus, mu0)
 call vector_append(all_num, all_sigmas, sigma)
 call vector_append(all_num, all_is_cond, .true.)
 all_num = all_num + 1

 call vector_append(dielectric_num, dielectric_refs, 1) ! malla total
 call vector_append(dielectric_num, dielectric_mus, mu0)
 dielectric_num = dielectric_num + 1
 call vector_append(all_num, all_refs, 1) ! malla total
 call vector_append(all_num, all_mus, mu0)
 call vector_append(all_num, all_sigmas, 0.0d0)
 call vector_append(all_num, all_is_cond, .false.)
 all_num = all_num + 1

 s = 0
! Comentado para la generalización a varias componentes no conexas de dielectrico
! call vector_append_inc(s, ref_front_omegaD, 4) ! meshdiel
! call vector_append_inc(s, ref_front_omegaD, 7)
! call vector_append_inc(s, ref_front_omegaD, 2)
! call vector_append_inc(s, ref_front_omegaD, 3)
! call vector_adjust(s, ref_front_omegaD) ! reduce el vector, eliminando posiciones si es necesario

 allocate(inputs(1))
 allocate(inputs(1)%boundary_references(1))
 inputs(1)%boundary_references(1) = 24 ! 24 48 meshtotal
 inputs(1)%intensity = 62000
 num_inputs = 1

!%******************************************************************************
!% LEEDATA.M                                                                   %
!% Entrada de datos                                                            %
!%******************************************************************************

!ficht = 'malla3dtotal_matlab'
!fichd = 'malladcaras_matlab'

 freq = 50 ! ø
 omega1 = 2*pi*freq

!%******************************************************************************
!% referencia de la interfase desde el conductor                               %
!%******************************************************************************

! non utilizados !

!nrcont = 1;
!irefcc = [4];

!%******************************************************************************
!% referencia de los vertices asociados al corte                               %
!%******************************************************************************

! non utilizados !

!refdirmas = 19;
!refdirmenos = 16;

!%******************************************************************************
!% referencia de las caras asociadas al corte                                  %
!%******************************************************************************

! non utilizados !

!refmenos = 15;
!refmas = 18;

!RADIO = 1

end subroutine leedata

end module leedata_mod

