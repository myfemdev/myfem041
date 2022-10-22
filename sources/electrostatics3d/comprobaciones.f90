!------------------------------------------------------------------------
!                       CHECKING OVER INPUT DATA 
!------------------------------------------------------------------------

 module comprobaciones

  use mesh
  use fich_electros3D
  use electros3D
  use cargavol
  use cargacur
  use cargapun
  use permitividad
  use bloqueo
  use derivados3D
  use auxiliar_cargas

  implicit none
  public :: comprueba, blocking_node, calculate_funs
  private

  integer :: i, j

 contains

! nrd & blofron%numero ARE SUPPOSED TO HAVE CORRECT VALUES (EVEN IF iopblo==0)
 logical function comprueba()
   comprueba = .FALSE.

  !  Dirichlet por funcion
  ! nrd, irefd, dir%fun
  !  Dirichlet por constante
  ! blofron%numero, blofron%referencias, blofron%valor

   do i=1,nrd
      if (dir%fun(i) <= 0 .or. dir%fun(i) > size(functions,1)) then
         print*, 'ERROR: unknown function in Dirichlet B.C.'
         return
      endif
   enddo
    
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
    
  !  Neumann por funcion
  ! nrn, irefn, neu%fun
  !  Neumann por constante
  ! neuman%numero, neuman%referencias, neuman%valor

   do i=1,nrn
      if (neu%fun(i) <= 0 .or. neu%fun(i) > size(functions,1)) then
         print*, 'ERROR: unknown function in Neumann B.C.'
         return
      endif
   enddo
    
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
    
  ! VOLUMIC CHARGES  
  ! carvol{numero,referencias,valor,constante} vol%fun
   do i=1,carvol%numero
      if ( (.not. carvol%constante(i)) .and. (vol%fun(i) <= 0 .or. vol%fun(i) > size(functions,1)) ) then
         print*, 'ERROR: unknown function in volume source'
         return
      endif
      do j=1,i-1
         if (carvol%referencias(i) == carvol%referencias(j)) then
            print*, 'ERROR: duplicated reference in volume source', carvol%referencias(i)
            return
         endif
      enddo
   enddo
    
  ! SURFACE CHARGES     
  ! carsup{numero,referencias,valor,constante} sup%fun
   do i=1,carsup%numero
      if ( (.not. carsup%constante(i)) .and. (sup%fun(i) <= 0 .or. sup%fun(i) > size(functions,1)) ) then
         print*, 'ERROR: unknown function in surface source'
         return
      endif
      do j=1,i-1
         if (carsup%referencias(i) == carsup%referencias(j)) then
            print*, 'ERROR: duplicated reference in surface source', carsup%referencias(i)
            return
         endif
      enddo
   enddo
    
  ! CURVILINEAR CHARGES
  ! carcur{numero,referencias,valor,constante} cur%fun
   do i=1,carcur%numero
      if ( (.not. carcur%constante(i)) .and. (cur%fun(i) <= 0 .or. cur%fun(i) > size(functions,1)) ) then
         print*, 'ERROR: unknown function in line source'
         return
      endif
      do j=1,i-1
         if (carcur%referencias(i) == carcur%referencias(j)) then
            print*, 'ERROR: duplicated reference in line source', carcur%referencias(i)
            return
         endif
      enddo
   enddo
    
  ! RELATIVE PERMITIVITY 
   do i=1,permirel%numero
      if (permirel%iopermir(i) == 1) then
         if (permirel%fun(i)<1 .or. permirel%fun(i)>size(functions_perm,1)) then
            print*, 'ERROR: permittivity function number out of range: ', permirel%fun(i)
            return
         endif
      endif
   enddo

   comprueba = .TRUE.
    
   return
    
 end function comprueba

!-1 error 0 not added 1 added
 integer function blocking_node()
   implicit none
   integer, dimension(3) :: datos
   integer :: res
   integer :: nver

   blocking_node = 0

   if (.not. (nrd.gt.0.or.blofron%numero.gt.0)) then
      print *, 'Neumann problem: assigning 0.0 to last vertex'
      res = mesh_read_array_unformatted(fichma, 3, datos)
      nver = datos(2)
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

! calculates the number of the function if there is only one distinct function
! -1: mixed functiones
! 0: no functions
! 1..7: one function
 subroutine calculate_funs()
   implicit none
   integer :: i

   dir%funs = 0
   neu%funs = 0
   vol%funs = 0
   sup%funs = 0
   cur%funs = 0
            
  ! Dirichlet
   do i=1,nrd
      if (dir%funs==0) then
         dir%funs = dir%fun(i)
      else
         if (dir%funs /= dir%fun(i)) then
            dir%funs = -1
            exit
         endif
      endif
   end do

  ! Neumann
   do i=1,nrn
      if (neu%funs==0) then
         neu%funs = neu%fun(i)
      else
         if (neu%funs /= neu%fun(i)) then
            neu%funs = -1
            exit
         endif
      endif
   end do
            
  ! Volume
   do i=1,carvol%numero
      if (.not. carvol%constante(i)) then
         if (vol%funs==0) then
            vol%funs = vol%fun(i)
         else
            if (vol%funs /= vol%fun(i)) then
               vol%funs = -1
               exit
            endif
         endif
      endif
   end do
            
  ! Surface
   do i=1,carsup%numero
      if (.not. carsup%constante(i)) then
         if (sup%funs==0) then
            sup%funs = sup%fun(i)
         else
            if (sup%funs /= sup%fun(i)) then
               sup%funs = -1
               exit
            endif
         endif
      endif
   end do
           
  ! Line
   do i=1,carcur%numero
      if (.not. carcur%constante(i)) then
         if (cur%funs==0) then
            cur%funs = cur%fun(i)
         else
            if (cur%funs /= cur%fun(i)) then
               cur%funs = -1
               exit
            endif
         endif
      endif
   end do

 end subroutine

end module comprobaciones
