!------------------------------------------------------------------
! goal : subroutine for the reading of the 3D mesh 
!------------------------------------------------------------------
!                                                                
!   IN:        fichma: mesh file                      
!                                                              
!   OUT:                                                      
!              nel   : number of elements in the mesh
!              nver  : number of vertices in the mesh 
!              mm    : connectivity matrix 
!              nra   : edges references array 
!              nrv   : vertex references array 
!              z     : coordinates array 
!              nsd   : domains array 
!------------------------------------------------------------------

subroutine leema3D(iformat)
                       
  use module_compiler_dependant, only: iostat_end
  use fich_electros3D,  only: fichma
  use electros3D
  use malla_3DP1
  use derivados3D
  use module_conver3d, only: conver3d
  
  implicit none
  integer :: i,j,k,l,nv,ios,iformat
  

  select case(iformat)
  
  case(1)  ! formatted mesh
     
     open(unit=10,file=fichma,form='formatted')!, recl=2092290)
     rewind(10)
     read(10,*) nel,nver
  
     if(allocated(mm))deallocate(mm)
     allocate(mm(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array mm',nel
        stop 1
     endif

     if(allocated(nra))deallocate(nra)
     allocate(nra(6,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nra',nel
        stop 1
     endif

     if(allocated(nrv))deallocate(nrv)
     allocate(nrv(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nrv',nel
        stop 1
     endif

     if(allocated(nrc))deallocate(nrc)      
     allocate(nrc(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nrvc',nel
        stop 1
     endif

     if(allocated(nsd))deallocate(nsd)
     allocate(nsd(nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nsd',nel
        stop 1
     endif
     
     if(allocated(z))deallocate(z)
     allocate(z(3,nver),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array z',nver
        stop 1
     endif     
      
     print*,'Number of mesh elements      :',nel
     print*,'Number of degrees of freedom :',nver
     read(10,*) ((mm(i,j),i=1,4),j=1,nel), &
                ((nrc(i,j),i=1,4),j=1,nel),&
                ((nra(i,j),i=1,6),j=1,nel),&
                ((nrv(i,j),i=1,4),j=1,nel),&
                ((z(i,j),i=1,3),j=1,nver)                                   
     read(10,*)  (nsd(k),k=1,nel)
     read(10,*, iostat=ios) nemm
     if (ios == iostat_end) then
        call conver3d(nel, nver, mm, z, nemm, det, binv, ib, jb)
        !add data to meshfile
        backspace(unit=10)
        write(10,*)nemm
        write(10,*)(det(k),k=1,nel),                      &
                   (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
                   (ib(i),i=1,nver+1),(jb(i),i=1,nemm)     
     else
       if(allocated(jb)) deallocate(det);  allocate(det(nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate det'
       if(allocated(jb)) deallocate(binv); allocate(binv(3,3,nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate binv'
       if(allocated(jb)) deallocate(ib);   allocate(ib(nver+1), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate ib'
       if(allocated(jb)) deallocate(jb);   allocate(jb(nemm), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate jb'
       read(10,*)(det(k),k=1,nel),                      &
                 (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
                 (ib(i),i=1,nver+1),(jb(i),i=1,nemm)
     end if
     close(10)

  case(2) ! unformatted mesh
  
     open(unit=10,file=fichma,form='unformatted')
     rewind(10)
     read(10) nel,nver
  
     if(allocated(mm))deallocate(mm)
     allocate(mm(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array mm',nel
        stop 1
     endif

     if(allocated(nra))deallocate(nra)
     allocate(nra(6,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nra',nel
        stop 1
     endif

     if(allocated(nrv))deallocate(nrv)
     allocate(nrv(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nrv',nel
        stop 1
     endif
  
     if(allocated(nrc))deallocate(nrc)      
     allocate(nrc(4,nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nrvc',nel
        stop 1
     endif

     if(allocated(nsd))deallocate(nsd)
     allocate(nsd(nel),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array nsd',nel
        stop 1
     endif

     if(allocated(z))deallocate(z)
     allocate(z(3,nver),stat=ierror)
     if (ierror.ne.0) then
        print*,'Error while allocating array z',nver
        stop 1
     endif
     
     print*,'Number of mesh elements      :',nel
     print*,'Number of degrees of freedom :',nver
     read(10)   ((mm(i,j),i=1,4),j=1,nel), &
                ((nrc(i,j),i=1,4),j=1,nel),&
                ((nra(i,j),i=1,6),j=1,nel),&
                ((nrv(i,j),i=1,4),j=1,nel),&
                ((z(i,j),i=1,3),j=1,nver)                                   
     read(10)    (nsd(k),k=1,nel) 
     read(10, iostat=ios) nemm
     if (ios == iostat_end) then
       call conver3d(nel, nver, mm, z, nemm, det, binv, ib, jb)
       !write new info into the meshfile
       write(10)(det(k),k=1,nel),                      &
                (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
                (ib(i),i=1,nver+1),(jb(i),i=1,nemm)    
     else
       if(allocated(jb)) deallocate(det);  allocate(det(nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate det'
       if(allocated(jb)) deallocate(binv); allocate(binv(3,3,nel), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate binv'
       if(allocated(jb)) deallocate(ib);   allocate(ib(nver+1), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate ib'
       if(allocated(jb)) deallocate(jb);   allocate(jb(nemm), stat = ierror)
       if (ierror /= 0) stop 'ERROR: leema3D, unable to allocate jb'
       read(10)(det(k),k=1,nel),                      &
               (((binv(i,j,k),i=1,3),j=1,3),k=1,nel), &
               (ib(i),i=1,nver+1),(jb(i),i=1,nemm)
     end if
     close(10)
 
  case default
 
    stop 'Error: value not recognised of iformat in leema'
 
  end select        

  return
end
