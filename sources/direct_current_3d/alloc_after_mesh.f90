subroutine alloc_after_mesh()
 
  use electros3D
  use malla_3DP1
  use derivados3D
  
  implicit none
  integer :: i,j,k,l,nv
      
   if(allocated(nrvg))deallocate(nrvg)
   allocate(nrvg(nver),stat=ierror)
   if (ierror.ne.0) then
      print*,'Error while allocating array nrvg',nver
      stop 1
   endif

   !le asignamos a cada vertice (dado en la numeracion global) la
   !referencia que tiene
   nrvg=0
   do k=1,nel
     caras: do j=1,4
       if(iopblo1.eq.1) then 
         do i=1,nrd     
           if(nrc(j,k).eq.irefd(i)) then
             nv=mm(indc(1,j),k)
             nrvg(nv)=nrc(j,k)
             nv=mm(indc(2,j),k)
             nrvg(nv)=nrc(j,k)
             nv=mm(indc(3,j),k)
             nrvg(nv)=nrc(j,k)
             exit caras
           endif
         end do
       endif
       if(iopblo2.eq.1) then 
         do i=1,blofron%numero     
           if(nrc(j,k).eq.blofron%referencias(i)) then
             nv=mm(indc(1,j),k)
             nrvg(nv)=nrc(j,k)
             nv=mm(indc(2,j),k)
             nrvg(nv)=nrc(j,k)
             nv=mm(indc(3,j),k)
             nrvg(nv)=nrc(j,k)
             exit caras
           endif
         end do
       endif
      end do caras
    end do 

   if(allocated(camor))deallocate(camor)      
   allocate(camor(ib(nver+1)),stat=ierror)
   if (ierror.ne.0) then
      print*,'Error while allocating array camor',ib(nver+1)
      stop 1
   endif
      
   if(allocated(b))deallocate(b)
   allocate(b(nver),stat=ierror)
   if (ierror.ne.0) then
      print*,'Error while allocating array b',nver
      stop 1
   endif

   if(allocated(sol))deallocate(sol)
   allocate(sol(nver),stat=ierror)
   if (ierror.ne.0) then
      print*,'Error while allocating array sol',nver
      stop 1
   endif
   
 end subroutine
