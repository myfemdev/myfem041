!---------------------------------------------------------------------------
!                 RESOLUTION OF THE DIRECT CURRENT 2D PROBLEM             
!---------------------------------------------------------------------------

subroutine dc2D()
    
  use malla_2DP1
  use dcurrent_2D
  use external_electros
  use derivados
    
  implicit none
  integer :: i
    
!---------------------------------------------------------------------------
!                  computation of the second member
!---------------------------------------------------------------------------
       
  call semi ()
 
!---------------------------------------------------------------------------
!  Existence of solution
!  int_\Gamma g + \int_\Omega f =0
!  g : Neumann BC function
!  f : second member function
!---------------------------------------------------------------------------
       
  if (ichneu.eq.1) then
     if (dabs(sum(b)).gt.1.e-12) then
        print*,'   '
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'     The problem has no solution'
        print*,'     Please, verify the data'
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'   '
        stop 1
     else
        if (iopblo3.ne.1) then
           print*,'   '
           print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
           print*,'     The solution of the problem is not unique'
           print*,'     Please, introduce the value of V in one point'
           print*,'     in the Data button'
           print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
           print*,'   '
           stop 1
        endif
     endif
  endif
       
!---------------------------------------------------------------------------
!                    matrix computation
!---------------------------------------------------------------------------
    
  print*,'---------------------------------------------------------'
  print*,'Computing the system matrix'
  print*,'Degrees of freeedom:',nver
  print*,'Number of vertex:',nver
  print*,'Number of elements:',nel
  print*,'---------------------------------------------------------'
      
  call matriz()
 
!---------------------------------------------------------------------------
!               Dirichlet boundary conditions
!---------------------------------------------------------------------------
  opcion_bloqueo_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
     if (nrd.gt.0) then
        do i=1,nrd
           call blomat(mua,c,mm,nra,nel,irefd(i))
        enddo
     end if
  endif opcion_bloqueo_funcion

  opcion_bloqueo_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
     if (blofron%numero.gt.0) then
        do  i=1,blofron%numero
            call blomat(mua,c,mm,nra,nel,blofron%referencias(i))
        enddo
      end if
   endif opcion_bloqueo_constantes	 

   opcion_bloqueo_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
      if (blopun%numero.gt.0) then
         do i=1,blopun%numero
            c(mua(blopun%referencias(i)+1))=1.d50
         enddo
      end if
   endif opcion_bloqueo_puntos	
       
!---------------------------------------------------------------------------
!        strong imposition of the boundary conditions
!---------------------------------------------------------------------------
      
  opcion_bloq_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
     if (nrd .gt. 0) then
        do i=1,nrd
           call bloseg(b,mm,z,nra,nel,irefd(i))
        end do
     end if
  endif opcion_bloq_funcion

  opcion_bloq_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
     if (blofron%numero.gt.0) then
        do i=1,blofron%numero
           call blosegc(b,mm,nra,nel,blofron%referencias(i),blofron%valor(i))
        enddo
     end if
  endif opcion_bloq_constantes

  opcion_bloq_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
     if (blopun%numero.gt.0) then
        do i=1,blopun%numero
           b(blopun%referencias(i))=1.d50*blopun%valor(i)
        enddo
     end if
  endif opcion_bloq_puntos	

!---------------------------------------------------------------------------
!         Cholesky factorization of the matrix
!---------------------------------------------------------------------------
      
  call chol(nver,mua,c)

!---------------------------------------------------------------------------
!           System resolution
!---------------------------------------------------------------------------
  
  print*,'Solving the linear system'
  print*,'---------------------------------------------------------'

  call sols(c,b,nver,mua)
     
  deallocate(c)
  deallocate(mua)

  do i=1,nver
     sol(i) = b(i)
  end do

  deallocate(b)

  return
end
