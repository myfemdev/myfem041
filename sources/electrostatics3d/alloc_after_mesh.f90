subroutine alloc_after_mesh()
 
  use fich_electros3D,  only: fichma,fichteta     
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

! nrvg WHEN IS POSSIBLE, FOR EACH VERTEX INDICATES THE DIRICHLET REFERENCE OF ONE OF IT'S FACES
   nrvg = 0
   do k = 1, nel
      caras: do j = 1, 4
         do i = 1, nrd
            if (nrc(j,k) == irefd(i)) then
               do l = 1, 3
                  nrvg(mm(indc(l,j),k)) = nrc(j,k)
               enddo
               cycle caras
            end if
         enddo
         do i = 1, blofron%numero
            if (nrc(j,k) == blofron%referencias(i)) then
               do l = 1, 3
                  nrvg(mm(indc(l,j),k)) = nrc(j,k)
               enddo
               cycle caras
            end if
         enddo
      enddo caras
   enddo

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

   return
 end subroutine
