module readxml_mod

use module_SO_DEPENDANT
use module_REPORT
use module_xml_parser, inti => int ! para ifort
!Solver modules
use globales


implicit none

contains

!-----------------------------------------------------------------------
! procedure for reading the solver variables
!-----------------------------------------------------------------------
subroutine readxml()

implicit none

integer :: i, ide, im
character(len=MAXPATH) :: matxml, sval
character(len=MAXPATH), dimension(:), allocatable :: list, list2, list3, refs

 call set_SO()
 call set_report_level(REPORT_STDOUT)


 ide = fopen()

! Mesh

 print *, 'Mesh'
! Total Mesh ! Tetraedros Nèdèlec
 call fread(ide, '/Mesh/Domain mesh/Mesh file', filenamecond)
! Dielectric Mesh ! Tetraedros Raviart - Thomas
 call fread(ide, '/Mesh/Dielectric mesh/Mesh file', filenamediel)

! Data

 print *, 'Data'
 call fread(ide, '/Data/Frequency/Frequency', freq)

! Boundary conditions

! Dielectric boundary

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 call flist(ide, '/Boundary conditions/Dielectric boundary/Connected components', list)
 allocate(ref_front_omegaD(size(list,1))) ! pode ser superior ao necesario se hai condicions con 0 referencias
 do i = 1, size(list,1) !loop for defined Intensity input BC's
      !References
      call fread_alloc(ide, '/Boundary conditions/Dielectric boundary/Connected components/'//trim(list(i)),&
            refs, realloc=.true.)
      if (size(refs,1)>0) then
        allocate(ref_front_omegaD(i)%diel_refs(size(refs,1)))

        ref_front_omegaD(i)%diel_refs = inti(refs)
        !ref_front_omegaD(i)%num = size(refs,1)

      else
        print * , 'Dielectric boundary B.C. with 0 references: skipping'
      endif
 enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Intensity input B.C.

 print *, 'Intensity input B.C.'
 num_inputs = 0
 call flist(ide, '/Boundary conditions/Input/Intensity', list)
 allocate(inputs(size(list,1))) ! pode ser superior ao necesario se hai condicions con 0 referencias
 do i = 1, size(list,1) !loop for defined Intensity input BC's
      !References
      call fread_alloc(ide, '/Boundary conditions/Input/Intensity/'//trim(list(i))//&
      &'/Surface references', refs, realloc=.true.)
      if (size(refs,1)>0) then
        allocate(inputs(num_inputs+1)%boundary_references(size(refs,1)))
        inputs(num_inputs+1)%boundary_references = inti(refs)
        !Constant value
        call fread(ide, '/Boundary conditions/Input/Intensity/'//trim(list(i))//&
            &'/RMS value', inputs(num_inputs+1)%intensity)
        call fread(ide, '/Boundary conditions/Input/Intensity/'//trim(list(i))//&
            &'/Phase angle', inputs(num_inputs+1)%phase)
        num_inputs = num_inputs + 1
      else
        print * , 'Intensity input B.C. with 0 references: skipping'
      endif
 enddo

 print*,'Materials database'

! Open materials database
 call fread(ide, '/Materials file/Open/materialsDB', matxml)
 im = fopen(matxml)

 print*,'Magnitudes'
 !Magnitudes
 call flist(ide, '/Properties/Materials/Materials', list)
 allocate(references(size(list,1)))
 do i = 1, size(list,1)
  references(i)%refnum = inti(list(i))
  call fread(ide, '/Properties/Materials/Materials/'//trim(list(i)), sval)
  call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity', list2)
  if (size(list2,1)==0) call error('readxml: missing electrical conductivity type for material')
  select case(trim(list2(1)))
      case ('A constant')
         call flist(im,'/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant', list3)
         if (size(list3,1)==0) call error('readxml: missing constant electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(im, '/Materials database/Open/Materials/'//trim(sval)//'/Electrical conductivity/A constant/Isotropic',&
                 references(i)%electrical_conductivity)
             case('Orthotropic')
                call error('readxml: Orthotropic magnitudes are not supported.')
             case default; call error('readxml: Type of constant: Case not implemented.')
         end select
      case ('A temperature dependant table')
        call error('readxml: Temperature dependant magnitudes are not supported.')
      case ('A function')
        call error('readxml: Function-defined magnitudes are not supported.')
      case default; call error('readxml: Type of magnitude: Case not implemented.')
  end select
  call flist(im,'/Materials database/Open/Materials/'//trim(sval)//&
                    '/Magnetic properties/Relative magnetic permeability', list2)
  if (size(list2,1)==0) call error('readxml: missing electrical conductivity type for material')
  select case(trim(list2(1)))
      case ('A constant')
         call flist(im,'/Materials database/Open/Materials/'//trim(sval)//&
                    '/Magnetic properties/Relative magnetic permeability/A constant', list3)
         if (size(list3,1)==0) call error('readxml: missing constant electrical conductivity type for material')
         select case(trim(list3(1)))
             case('Isotropic')
                 call fread(im, '/Materials database/Open/Materials/'//trim(sval)//&
                    '/Magnetic properties/Relative magnetic permeability/A constant/Isotropic',&
                 references(i)%relative_magnetic_permeability)
             case('Orthotropic')
                call error('readxml: Orthotropic magnitudes are not supported.')
             case default; call error('readxml: Type of constant: Case not implemented.')
         end select
      case ('A temperature dependant table')
        call error('readxml: Temperature dependant magnitudes are not supported.')
      case ('A function')
        call error('readxml: Function-defined magnitudes are not supported.')
      case default; call error('readxml: Type of magnitude: Case not implemented.')
  end select

  references(i)%cond_not_diel = references(i)%electrical_conductivity > 1.0d-12 ! air: 0.5d-14

  if (references(i)%cond_not_diel) then ! conductor

    call vector_append(all_num, all_is_cond, .true.)
    call vector_append(conductor_num, conductor_refs, references(i)%refnum) ! malla total
    call vector_append(conductor_num, conductor_sigmas, references(i)%electrical_conductivity)
    conductor_num = conductor_num + 1

  else ! dielectrico

    call vector_append(all_num, all_is_cond, .false.)
    call vector_append(dielectric_num, dielectric_refs, references(i)%refnum) ! malla total
    call vector_append(dielectric_num, dielectric_mus, mu0*references(i)%relative_magnetic_permeability)
    dielectric_num = dielectric_num + 1

  endif

  call vector_append(all_num, all_refs, references(i)%refnum) ! malla total
  call vector_append(all_num, all_mus, mu0*references(i)%relative_magnetic_permeability)
  call vector_append(all_num, all_sigmas, references(i)%electrical_conductivity)
  all_num = all_num + 1

 enddo

 omega1 = 2*pi*freq

 call fclose(im)
 call fclose(ide)


end subroutine

end module

