module subdomains_mod

use defines


! uso:
!
!   integer, dimension(:), allocatable :: map, mapel
!   integer, dimension(:,:), allocatable :: mm_new
!   real(real64), dimension(:,:), allocatable :: z_new
!   real(real64), dimension(:), allocatable :: sol_new
!
! call extract_subdomain(nver, mm, nsd, 36, mm_new, map, mapel)
! call extract_z(z, z_new, map)
! call extract_field_1(sol, sol_new, map)
! call writeVTU(size(mapel,1), size(map,1), mm_new, z_new, 'tetra', &
!      sol_new, 'solucion', 'scalar', 'node', 'temp36.vtu')


implicit none

contains


subroutine extract_subdomain(vnum, mm, sub, subnum, mm_new, map, mapel)
integer, intent(in) :: vnum
integer, dimension(:,:), intent(in) :: mm
integer, dimension(:), intent(in) :: sub
integer, intent(in) :: subnum
integer, dimension(:,:), allocatable, intent(out) :: mm_new
integer, dimension(:), allocatable, intent(out) :: map ! new vertex -> old vertex
integer, dimension(:), allocatable, intent(out) :: mapel ! new element -> old element
integer, dimension(vnum) :: map_inv ! old vertex -> new vertex
logical, dimension(vnum) :: temp
integer :: i, j, nov, noc

 temp = .false.
 noc = 0
 ! marca vertices usados
 do i = 1, size(mm,2)
    if (sub(i) == subnum) then
        noc = noc + 1
        temp(mm(:, i)) = .true.
    endif
 enddo

 nov = 0
 ! conta numero de vertice usados
 do i = 1, size(temp,1)
    if (temp(i)) nov = nov + 1
 enddo

 allocate(mm_new(size(mm,1),noc))
 allocate(map(nov))
 allocate(mapel(noc))
 j = 1
 ! calcula novo mm (falta cambiar indices)
 do i = 1, size(mm,2)
    if (sub(i) == subnum) then
        mapel(j) = i
        mm_new(:, j) = mm(:, i)
        j = j + 1
    endif
 enddo

 j = 1
 ! calcula punteiros de vertice novo a vello e de vello a novo
 do i = 1 , vnum
    if (temp(i)) then ! se se utiliza...
        map(j) = i
        map_inv(i) = j
        j = j + 1
    endif
 enddo

 ! reescribir novo mm, cabiando indices
 do i = 1, size(mm_new,2)
    do j = 1, size(mm_new,1)
        mm_new(j,i) = map_inv(mm_new(j,i))
    enddo
 enddo

end subroutine



subroutine extract_z(z, z_new, map)
 real(real64), dimension(:,:), intent(in) :: z
 real(real64), dimension(:,:), allocatable, intent(out) :: z_new
 integer, dimension(:), intent(in) :: map
 integer :: i
    allocate(z_new(size(z,1),size(map,1)))
    do i = 1 , size(map,1)
        z_new(:,i) = z(:,map(i))
    enddo
end subroutine



! pass map for point fields and mapel for cell fields
subroutine extract_field_v(f, f_new, map)
 real(real64), dimension(:,:), intent(in) :: f
 real(real64), dimension(:,:), allocatable, intent(out) :: f_new
 integer, dimension(:), intent(in) :: map
 integer :: i
    allocate(f_new(size(f,1),size(map,1)))
    do i = 1 , size(map,1)
        f_new(:,i) = f(:,map(i))
    enddo
end subroutine



! pass map for point fields and mapel for cell fields
subroutine extract_field_1(f, f_new, map)
 real(real64), dimension(:), intent(in) :: f
 real(real64), dimension(:), allocatable, intent(out) :: f_new
 integer, dimension(:), intent(in) :: map
 integer :: i
    allocate(f_new(size(map,1)))
    do i = 1 , size(map,1)
        f_new(i) = f(map(i))
    enddo
end subroutine


! pass map for point fields and mapel for cell fields
subroutine extract_field_n(f, f_new, map, n)
 real(real64), dimension(:), intent(in) :: f
 real(real64), dimension(:), allocatable, intent(out) :: f_new
 integer, dimension(:), intent(in) :: map
 integer, intent(in) :: n
 integer :: i, j
    allocate(f_new(size(map,1)*n))
    do i = 1 , size(map,1)
        do j = 1 , n
            f_new((i-1)*n+j) = f((map(i)-1)*n+j)
        enddo
    enddo
end subroutine


! pass map for point fields and mapel for cell fields
subroutine extract_field_c(f, f_new, map)
 real(real64), dimension(:,:), intent(in) :: f
 real(real64), dimension(:), allocatable, intent(out) :: f_new
 integer, dimension(:), intent(in) :: map
 integer :: i, j
    allocate(f_new(size(map,1)*size(f,1)))
    do i = 1 , size(map,1)
        do j = 1 , size(f,1)
            f_new((i-1)*size(f,1)+j) = f(j, map(i))
        enddo
    enddo
end subroutine


end module

