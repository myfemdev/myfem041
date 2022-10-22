!------------------------------------------------------------------------
!                       CHECKING OVER INPUT DATA 
!------------------------------------------------------------------------

module comprobaciones

  use mesh
  use electros_2D
  use derivados
  use fich_electros

  implicit none

  public :: comprueba, blocking_node
  private

  integer :: i, j

contains

! nrd & blofron%numero ARE SUPPOSED TO HAVE CORRECT VALUES (EVEN IF iopblo==0)
  logical function comprueba()
   comprueba = .FALSE.

  !  Dirichlet_por_funcion
  ! nrd, irefd, ! non -> dir%fun
  !  Dirichlet_por_constante
  ! blofron%numero, blofron%referencias, blofron%valor

   do i=1,nrd
      do j=1,i-1
         if (irefd(i) == irefd(j)) then
            print*, 'ERROR: duplicated reference in Dirichlet B.C. by function', irefd(i)
            return
         endif
      enddo
      do j=1,blofron%numero
         if (irefd(i) == blofron%referencias(j)) then
            print*, 'ERROR: duplicated reference in Dirichlet B.C. (constant/function)', irefd(i)
            return
         endif
      enddo
   enddo
   do i=1,blofron%numero
     do j=1,i-1
        if (blofron%referencias(i) == blofron%referencias(j)) then
           print*, 'ERROR: duplicated reference in Dirichlet B.C. by constant', blofron%referencias(i)
           return
        endif
     enddo
   enddo
    
  !  Neumann_por_funcion
  ! nrn, irefn, ! non -> neu%fun
  !  Neumann_por_constante
  ! neuman%numero, neuman%referencias, neuman%valor

   do i=1,nrn
      do j=1,i-1
         if (irefn(i) == irefn(j)) then
            print*, 'ERROR: duplicated reference in Neumann B.C. by function', irefn(i)
            return
         endif
      enddo
      do j=1,neuman%numero
         if (irefn(i) == neuman%referencias(j)) then
            print*, 'ERROR: duplicated reference in Neumann B.C. (constant/function)', irefn(i)
            return
         endif
      enddo
   enddo
   do i=1,neuman%numero
      do j=1,i-1
         if (neuman%referencias(i) == neuman%referencias(j)) then
            print*, 'ERROR: duplicated reference in Neumann B.C. by constant', neuman%referencias(i)
            return
         endif
      enddo
   enddo

  ! Dirichlet / Neumann
   do i=1,nrd
      do j=1,nrn
         if (irefd(i) == irefn(j)) then
            print*, 'ERROR: the same reference is used in Neumann and Dirichlet B.C.', irefd(i)
            return
         endif
      enddo
      do j=1,neuman%numero
         if (irefd(i) == neuman%referencias(j)) then
            print*, 'ERROR: the same reference is used in Neumann and Dirichlet B.C.', irefd(i)
            return
         endif
      enddo
   enddo
   do i=1,blofron%numero
      do j=1,nrn
         if (blofron%referencias(i) == irefn(j)) then
            print*, 'ERROR: the same reference is used in Neumann and Dirichlet B.C.', blofron%referencias(i)
            return
         endif
      enddo
      do j=1,neuman%numero
         if (blofron%referencias(i) == neuman%referencias(j)) then
            print*, 'ERROR: the same reference is used in Neumann and Dirichlet B.C.', blofron%referencias(i)
            return
         endif
      enddo
   enddo
    
  ! Surface charges 
  ! Carvol{numero,referencias,valor,constante} ! no -> vol%fun
   do i=1,carvol%numero
      do j=1,i-1
         if (carvol%referencias(i) == carvol%referencias(j)) then
            print*, 'ERROR: duplicated reference in surface source', carvol%referencias(i)
            return
         endif
      enddo
   enddo
    
  ! Curvilinear charges 
  ! Carcur{numero,referencias,valor,constante} ! no -> cur%fun
   do i=1,carcur%numero
      do j=1,i-1
         if (carcur%referencias(i) == carcur%referencias(j)) then
            print*, 'ERROR: duplicated reference in line source', carcur%referencias(i)
            return
         endif
      enddo
   enddo
    
   comprueba = .TRUE.

   return    
  end function comprueba

! -1 ERROR 0 NOT ADDED 1 ADDED
  integer function blocking_node()
  
   implicit none
   integer, dimension(3) :: datos
   integer               :: res
   integer               :: nver

   blocking_node = 0

   if(.not. (nrd.gt.0.or.blofron%numero.gt.0)) then
      print *, 'Neumann problem: assigning 0.0 to last vertex'
      res = mesh_read_array_formatted(fichma, 3, datos) ! nel, nnod, nver
      nver = datos(3)
      if (res /= 0) then
         print *, 'Neumann problem: ', trim(mesh_error_string(res))
         blocking_node = -1
         return
      endif
      if (nver < 1) then
         print *, 'Neumann problem: wrong number of vertices ', nver
         blocking_node = -1
         return
      endif
      iopblo = 1
      iopblo3 = 1
      blopun%numero = blopun%numero + 1
      blopun%referencias(blopun%numero) = nver
      blopun%valor(blopun%numero) = 0.0d0
      blocking_node = 1
      print *, 'Neumann problem: assigned 0.0 to vertex', nver
   end if

  end function blocking_node

end module comprobaciones
