!---------------------------------------------------------------------------
!                     READING OF THE TEMPERATURE FIELD
!---------------------------------------------------------------------------  
  subroutine leetmp()
  
  use electros3D,      only : teta
  use fich_electros3D, only : fichteta
  use malla_3DP1,      only : nver
  use module_convers
  use LIB_VTK_IO_READ
  
  implicit none
  
  integer :: p,npieces,nnd,nco,nverteta,i
  
  if (allocated(teta)) deallocate(teta) !deallocate teta
  p = index(fichteta, '.', back=.true.) !search extension
  if  (p == 0) stop 'Error (temperature): file has not extension'
  select case (lcase(fichteta(p+1:len_trim(fichteta))))
  case('mff')
     open(unit=10,file=fichteta,form='formatted', position='rewind')
     read(10,*) nverteta
     if (nverteta /= nver) stop 'Error (temperature): nverteta /= nver'
     allocate(teta(nver))
     rewind(10)
     read(10,*) nverteta
     read(10,*) (teta(i), i=1, nverteta)
     close(10)
     if(nverteta.ne.nver) stop 'The number of vertix in the temperature file&
                             & is different from the number of vertix in the mesh'
  case('vtu')
     print*,'Reading a VTU binary file for temperature...'
     if (vtk_ini_xml_read('Binary',fichteta,'UnstructuredGrid', npieces) /=0) &
                               stop 'Error (temperature): unable open vtu file'
     if (npieces > 1) stop 'Error (temperature): too many pieces'
     if (vtk_var_xml_read('node', nnd, nco, 'Temperature', teta) /=0) & 
                           stop 'Error (temperature): unable to read pointdata'
     if (vtk_end_xml_read() /=0) stop 'Error (temperature): close'
  case default
     stop 'Unrecognized temperature file extension'
  end select
    
  end subroutine