
! comprobaciones sobre los datos de entrada

module comprobaciones

use globales

implicit none

public :: comprueba

private

integer :: i, j, k, l

contains

logical function comprueba()
implicit none
type(mfm_data) :: mallatot
type(mfm_data) :: malladie
integer :: res, res1

    comprueba = .FALSE.

    if (dielectric_num <= 0) then
        print *, 'ERROR: at least one dielectric material required'
        return
    endif

    if (conductor_num <= 0) then
        print *, 'ERROR: at least one conductor material required'
        return
    endif

    res = mesh_read_header(filenamecond, mallatot)
    if (res /= 0) then
        print *, 'ERROR reading total mesh'
        return
    endif

    res = mesh_read_header(filenamediel, malladie)
    if (res /= 0) then
        print *, 'ERROR reading dielectric mesh'
        return
    endif

    res = 0

    if (mallatot%nnod == mallatot%nver) then
        print *, 'ERROR: total mesh: nnod == nver'
        res = res + 1
    endif

    if (mallatot%dim /= 3) then
        print *, 'ERROR: total mesh: dimension is not 3:', mallatot%dim
        res = res + 1
    endif

    if (mallatot%lnn /= 6 .or. mallatot%lnv /= 4 .or. mallatot%lne /= 6 .or. mallatot%lnf /= 4) then
        print *, 'ERROR: total mesh: incorrect element type'
        print *, 'ERROR:    found: lnn=', mallatot%lnn, 'lnv=', mallatot%lnv, 'lne=', mallatot%lne, 'lnf=', mallatot%lnf
        print *, 'ERROR: expected: lnn=', 6, 'lnv=', 4, 'lne=', 6, 'lnf=', 4
        res = res + 1
    endif

    if (res /= 0) then
        print *, 'ERROR: total mesh: Nedelec elements required'
    endif

    res1 = res

    if (malladie%nnod == malladie%nver) then
        print *, 'ERROR: dielectric mesh: nnod == nver'
        res = res + 1
    endif

    if (malladie%dim /= 3) then
        print *, 'ERROR: dielectric mesh: dimension is not 3:', malladie%dim
        res = res + 1
    endif

    if (malladie%lnn /= 4 .or. malladie%lnv /= 4 .or. malladie%lne /= 6 .or. malladie%lnf /= 4) then
        print *, 'ERROR: dielectric mesh: incorrect element type'
        print *, 'ERROR:    found: lnn=', malladie%lnn, 'lnv=', malladie%lnv, 'lne=', malladie%lne, 'lnf=', malladie%lnf
        print *, 'ERROR: expected: lnn=', 4, 'lnv=', 4, 'lne=', 6, 'lnf=', 4
        res = res + 1
    endif

    if (res /= res1) then
        print *, 'ERROR: dielectric mesh: Raviart-Thomas elements required'
    endif

    if (res /= 0) then ! houbo erros
        return
    endif

    ! comprobar: referencias repetidas en distintos conductores
    do i = 1, num_inputs
        do j = i + 1, num_inputs
            do k = 1, size(inputs(i)%boundary_references,1)
                do l = 1, size(inputs(j)%boundary_references,1)
                    if (inputs(i)%boundary_references(k) == inputs(j)%boundary_references(l)) then
                        print *, 'ERROR: duplicated conductor boundary reference', inputs(i)%boundary_references(k)
                        return
                    endif
                enddo
            enddo
        enddo
    enddo

    if (num_inputs == 0) then
        print *, 'Warning: no intensity inputs specified'
    endif

    ! comprobar: materiales


    ! comprobar: referencias repetidas
    do i = 1 , size(references,1)
        do j = i + 1 , size(references,1)
            if (references(i)%refnum == references(j)%refnum) then
                print *, 'ERROR: duplicated reference', references(j)%refnum
                return
            endif
        enddo
    enddo

    comprueba = .TRUE.
    
    return
    
end function comprueba


end module comprobaciones
