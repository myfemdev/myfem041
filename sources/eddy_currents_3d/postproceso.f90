module postproceso_mod

!use defines
use globales
use sparse_class
use module_MATH
use module_writeVTU
use subdomains_mod
use module_fem_extract

implicit none

contains

subroutine postproceso(HH)
implicit none
! reais ou complexos ???
complex(real64), dimension(meshcond%nnod), intent(in) :: HH
real(real64), dimension(:), allocatable :: cev
integer, dimension(4,6) :: MCB
integer, dimension(4) :: cb
integer, dimension(4,4), parameter :: aux = reshape([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],[4,4]) ! ok: so diagonal
real(real64), dimension(4,3), parameter :: temp1 = reshape([0,1,0,0, 0,0,1,0, 0,0,0,1],[4,3]) ! [0 0 0;eye(3)]
real(real64), dimension(1,4), parameter :: ones14 = reshape([1,1,1,1],[1,4]) ! [1 1 1 1]
!real(real64), dimension(4,4) :: temp2
real(real64), dimension(4,4) :: temp2inv
real(real64), dimension(6,6) :: temp3 = 0 ! suficiente 0 aqui porque so se cambia a diagonal
real(real64), dimension(3,4) :: temp_ifort
real(real64), dimension(3,4) :: temp_m
real(real64), dimension(4) :: temp_abs2
real(real64), dimension(3,4) :: GG
real(real64), dimension(3,6) :: MMM
complex(real64), dimension(6,1) :: HHpart
complex(real64), dimension(3,1) :: Hhk
complex(real64), dimension(3,1) :: curlH
integer :: k, i, j, ns, subnel, subnver
real(real64), dimension(:), allocatable :: cvfield1, cvfield2, cvfield3
real(real64), dimension(:), allocatable :: evfield1, evfield2, evfield3
complex(real64), dimension(:,:), allocatable :: Hver1, Bver1, Jel, Jjoule_el !Eel
!real(real64), dimension(meshcond%nver) :: tempfield
real(real64) :: mu, sigma
logical :: is_cond
character(len=100) :: filename
integer, dimension(:), allocatable :: glob, globel
integer, dimension(:,:), allocatable :: submm
real(real64), dimension(:,:), allocatable :: subz

 MCB = 0


!%******************************************************************************
!%     Bucle para recorrer subdominios                                         %
!%******************************************************************************
 do ns = 1, all_num

    mu = all_mus(ns)
    sigma = all_sigmas(ns)
    is_cond = all_is_cond(ns)
    
    !Extraemos cada subdominoi
    call extract_mesh(meshcond%nver, meshcond%mm, meshcond%z, meshcond%nsd, &
                     [all_refs(ns)], submm, subz, glob, globel)

   subnel = size(globel,1)
   subnver = size(glob,1)

   if (allocated(cev)) deallocate(cev)
   allocate(cev(subnver))
   cev = 0.0d0

   if (allocated(evfield1)) deallocate(evfield1)
   allocate(evfield1(3*subnver))
   evfield1 = 0.0d0
   if (allocated(evfield2)) deallocate(evfield2)
   allocate(evfield2(3*subnver))
   evfield2 = 0.0d0
   if (allocated(evfield3)) deallocate(evfield3)
   allocate(evfield3(3*subnver))
   evfield3 = 0.0d0

   if (allocated(cvfield1)) deallocate(cvfield1)
   allocate(cvfield1(3*subnel))
   cvfield1 = 0.0d0
   if (allocated(cvfield2)) deallocate(cvfield2)
   allocate(cvfield2(3*subnel))
   cvfield2 = 0.0d0
   if (allocated(cvfield3)) deallocate(cvfield3)
   allocate(cvfield3(3*subnel))
   cvfield3 = 0.0d0

   if (allocated(Hver1))    deallocate(Hver1)
   allocate(Hver1(3,subnver))
   Hver1 = 0.0d0
   if (allocated(Bver1))    deallocate(Bver1)
   allocate(Bver1(3,subnver))
   Bver1 = 0.0d0

   if (allocated(Jel))      deallocate(Jel)
   allocate(Jel(3,subnel))
   Jel = 0.0d0
!   if (allocated(Eel))      deallocate(Eel)
!   allocate(Eel(3,subnel))
!   Eel = 0
   if (allocated(Jjoule_el)) deallocate(Jjoule_el)
   allocate(Jjoule_el(3,subnel))
   Jjoule_el = 0.0d0

   !Recorremos cada elemento. Postproceso
   do k = 1, subnel
     cev(submm(:,k)) = cev(submm(:,k)) + 1

     do j = 1, 6
        temp3(j,j) = sga(j,globel(k))
     enddo

     temp_ifort = meshcond%z(:,meshcond%mm(:,globel(k)))
     if (.not. inverse3x4_1(temp_ifort, temp2inv)) &
         call sperror('Unable to find matrix inverse in "postproceso.f90(...)" subroutine')
     GG = transpose(matmul(temp2inv, temp1)) ! GG = (inv([1 1 1 1;coord])*[0 0 0;eye(3)])';
     !HHpart = sparse_get(HH,meshcond%nn(:,k), [1]) ! evitable con HH densa

     HHpart = reshape([HH(meshcond%nn(:,globel(k)))],[6,1])

     do i = 1, 4
        cb = aux(i,:)

        MCB(1,[1,4]) = -cb([2,4])
        MCB(1,3) = cb(3)
        MCB(2,1) = cb(1)
        MCB(2,[2,5]) = -cb([3,4])
        MCB(3,2) = cb(2)
        MCB(3,[3,6]) = -cb([1,4])
        MCB(4,4:6) = cb([1,2,3])

        Hhk = matmul(matmul(matmul(GG,MCB),temp3),HHpart)
        ! 3x4 4x6 6x6 6x1 -> 3x1


        Hver1(:,submm(i,k)) = Hver1(:,submm(i,k)) + Hhk(:,1)
        Bver1(:,submm(i,k)) = Bver1(:,submm(i,k)) + Hhk(:,1) * mu
        !Jdensver(:,meshcond%mm(i,k)) = Jdensver(:,meshcond%mm(i,k)) + curlH(:,1)

     enddo

    if (is_cond) then
        MMM = reshape([ cross3(GG(:,1),GG(:,2)), cross3(GG(:,2),GG(:,3)), cross3(GG(:,3),GG(:,1)), &
            & cross3(GG(:,1),GG(:,4)), cross3(GG(:,2),GG(:,4)), cross3(GG(:,3),GG(:,4)) ], [3,6])
        curlH = 2.0d0 * matmul( matmul( MMM , temp3 ) , HHpart )
        temp_m = matmul(curlH, ones14)

        temp_abs2 = temp_m(1,:) * temp_m(1,:) + &
            temp_m(2,:) * temp_m(2,:) + &
            temp_m(3,:) * temp_m(3,:)
        !Jver(meshcond%mm(:,k)) = Jver(meshcond%mm(:,k)) + temp_abs2 / (2.0 * sigma)

        Jel(:,k) = curlH(:,1)
!        Eel(:,k) = Jel(:,k)/sigma Campos electrico
!        Jjoule_el(:,k) = temp_abs2(1:3) / (2.0 * sigma) !!!!!! El rango de temp_abs hace que compile, pero no estaba así. Hablar con Pili.
!        Jjoule_el(:,k) = (Jel(1,k)*Jel(1,k) + Jel(2,k)*Jel(2,k) + Jel(3,k)*Jel(3,k))/(2.0d0*sigma)
        Jjoule_el(:,k) = (cdabs(Jel(1,k))**2 + cdabs(Jel(2,k))**2 + cdabs(Jel(3,k))**2)/(2.0d0*sigma)

    endif

   enddo


    ! Escritura a formato VTU
    write(unit=filename, fmt=*) references(ns)%refnum
    filename = 'out_real_'//trim(adjustl(filename))//'.vtu'
    call VTU_open(filename)
    call VTU_write_mesh(subnel, size(subz,2), submm, subz, 'tetra')
    call VTU_begin_pointdata()


    if (is_cond) then
        do i = 1, subnel
            cvfield1(i*3-2) = dreal(Jel(1,i))
            cvfield1(i*3-1) = dreal(Jel(2,i))
            cvfield1(i*3)   = dreal(Jel(3,i))

            cvfield2(i*3-2) = Jjoule_el(1,i)
            cvfield2(i*3-1) = Jjoule_el(2,i)
            cvfield2(i*3)   = Jjoule_el(3,i)

!            cvfield3(i*3-2) = dreal(Eel(1,i))
!            cvfield3(i*3-1) = dreal(Eel(2,i))
!            cvfield3(i*3)   = dreal(Eel(3,i))
        enddo

        call cell2node(subnver, submm, cvfield1, evfield1)
        call cell2node(subnver, submm, cvfield2, evfield2)
!        call cell2node(subnver, submm, cvfield3, evfield3)

        call VTU_write_pointdata(evfield1, 'Current density (A/m^2), real part', 'vector')
        call VTU_write_pointdata(evfield2, 'Active power density (W/m^3)', 'vector')
!        call VTU_write_pointdata(evfield3, 'ElectricField_real', 'vector')

    end if 

    Hver1(1,:) = Hver1(1,:) / cev
    Hver1(2,:) = Hver1(2,:) / cev
    Hver1(3,:) = Hver1(3,:) / cev

    Bver1(1,:) = Bver1(1,:) / cev
    Bver1(2,:) = Bver1(2,:) / cev
    Bver1(3,:) = Bver1(3,:) / cev

    do i = 1, subnver
        evfield1(i*3-2) = dreal(Hver1(1,i))
        evfield1(i*3-1) = dreal(Hver1(2,i))
        evfield1(i*3)   = dreal(Hver1(3,i))

        evfield2(i*3-2) = dreal(Bver1(1,i))
        evfield2(i*3-1) = dreal(Bver1(2,i))
        evfield2(i*3)   = dreal(Bver1(3,i))
    enddo


    call VTU_write_pointdata(evfield1, 'Magnetic field (A/m), real part', 'vector')
    call VTU_write_pointdata(evfield2, 'Magnetic flux density (T), real part', 'vector')

    if (.not.is_cond) then
        call VTU_write_pointdata(evfield3, 'Current density (A/m^2), real part', 'vector')
        call VTU_write_pointdata(evfield3, 'Active power density (W/m^3)', 'vector')
    endif

    call VTU_end_pointdata()
    call VTU_close()

    cvfield1 = 0
    cvfield3 = 0

    write(unit=filename, fmt=*) references(ns)%refnum
    filename = 'out_imag_'//trim(adjustl(filename))//'.vtu'
    call VTU_open(filename)
    call VTU_write_mesh(subnel, size(subz,2), submm, subz, 'tetra')
    call VTU_begin_pointdata()


    if (is_cond) then
        do i = 1, subnel
            cvfield1(i*3-2) = dimag(Jel(1,i))
            cvfield1(i*3-1) = dimag(Jel(2,i))
            cvfield1(i*3)   = dimag(Jel(3,i))

!            cvfield3(i*3-2) = dimag(Eel(1,i))
!            cvfield3(i*3-1) = dimag(Eel(2,i))
!            cvfield3(i*3)   = dimag(Eel(3,i))
        enddo

        call cell2node(subnver, submm, cvfield1, evfield1)
        call cell2node(subnver, submm, cvfield3, evfield3)

        call VTU_write_pointdata(evfield1, 'Current density (A/m^2), imaginary part', 'vector')
!        call VTU_write_pointdata(evfield3, 'ElectricField_imag', 'vector')

    end if 


    do i = 1, subnver
        evfield1(i*3-2) = dimag(Hver1(1,i))
        evfield1(i*3-1) = dimag(Hver1(2,i))
        evfield1(i*3)   = dimag(Hver1(3,i))

        evfield2(i*3-2) = dimag(Bver1(1,i))
        evfield2(i*3-1) = dimag(Bver1(2,i))
        evfield2(i*3)   = dimag(Bver1(3,i))
    enddo


    call VTU_write_pointdata(evfield1, 'Magnetic field (A/m), imaginary part', 'vector')
    call VTU_write_pointdata(evfield2, 'Magnetic flux density (T), imaginary part', 'vector')

    if (.not.is_cond) then
        call VTU_write_pointdata(evfield3, 'Current density (A/m^2), imaginary part', 'vector')
    endif

    call VTU_end_pointdata()
    call VTU_close()


 enddo

   if (allocated(cev))      deallocate(cev)
   if (allocated(evfield1)) deallocate(evfield1)
   if (allocated(evfield2)) deallocate(evfield2)
   if (allocated(evfield3)) deallocate(evfield3)
   if (allocated(cvfield1)) deallocate(cvfield1)
   if (allocated(cvfield2)) deallocate(cvfield2)
   if (allocated(cvfield3)) deallocate(cvfield3)
   if (allocated(Hver1))    deallocate(Hver1)
   if (allocated(Bver1))    deallocate(Bver1)
   if (allocated(Jel))      deallocate(Jel)
!   if (allocated(Eel)) deallocate(Eel)
   if (allocated(Jjoule_el)) deallocate(Jjoule_el)
end subroutine postproceso

end module postproceso_mod

