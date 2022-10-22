module dielectrico_caras_mod

use globales
use module_MATH
use module_SET

implicit none

contains

subroutine dielectrico_caras()
implicit none
integer, dimension(:), allocatable :: nod_int
integer, dimension(:), allocatable :: temp
integer, dimension(:), allocatable :: rows
integer, dimension(:), allocatable :: cols
integer :: i, j, k, tamnodfron, ini, fin

 allocate(nod_comp_con(size(ref_front_omegaD,1)))

 ncm1 = size(ref_front_omegaD,1) - 1
 tamnodfron = 0

 do j = 1, size(ref_front_omegaD,1)
     call find_2v(meshdiel%nrc, ref_front_omegaD(j)%diel_refs, rows, cols)

     if (allocated(temp)) deallocate(temp)
     allocate(temp(size(rows,1)))
     do i = 1, size(rows,1)
        temp(i) = meshdiel%nn(rows(i),cols(i))
     !    print *, i, rows(i), cols(i), temp(i)
     enddo

     ! aviso: este unique non ordena, ao contrario do unique de Matlab
     call unique(temp, nod_comp_con(j)%diel_refs)
     ! call unique(temp, nod_frontera) [OLD]

     tamnodfron = tamnodfron + size(nod_comp_con(j)%diel_refs, 1)
 enddo

 if (allocated(nod_frontera)) deallocate(nod_frontera)
 allocate(nod_frontera(size(nod_comp_con(1)%diel_refs)))
 nod_frontera(:) = nod_comp_con(1)%diel_refs(:)

 if (allocated(nod_frontera_total)) deallocate(nod_frontera_total)
 allocate(nod_frontera_total(tamnodfron))

 ! construcción de nod_frontera_total
 ! Falta compobacion en los indices para pasar de los limites
 ini = 1
 fin = 0
 do i = 1, size(nod_comp_con,1)
     fin = fin + size(nod_comp_con(i)%diel_refs,1)
     nod_frontera_total(ini:fin) = nod_comp_con(i)%diel_refs(1:fin-ini+1)
     ini = fin+1
 enddo

 k = 0
 call setdiff([(k, k=1,meshdiel%nnod)], nod_frontera_total, nod_int) ! nnodd_caras <-> meshdiel%nnod

 caras_int = size(nod_int, 1)

 if (allocated(rows)) deallocate(rows)
 if (allocated(cols)) deallocate(cols)
 if (allocated(temp)) deallocate(temp)
 if (allocated(nod_int)) deallocate(nod_int)

end subroutine dielectrico_caras

end module dielectrico_caras_mod

