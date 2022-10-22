!---------------------------------------------------------------------------
!                   SOLVING OF THE ELECTROSTATIC PROBLEM                
!---------------------------------------------------------------------------

subroutine electrostatica()
    
  use malla_2DP1
  use electros_2D
  use external_electros
  use derivados
     
  implicit none
  integer :: i
     
  call arint ()    
     
!---------------------------------------------------------------------------
!                  calculation of the second member vector                     
!---------------------------------------------------------------------------
       
  call semi ()
              
!---------------------------------------------------------------------------
!  Verification of the existence of the problem solution
!  We check wether int_\Gamma g + \int_\Omega f =0
!  g : Neumann condition function
!  f : function that gives the second member of the problem 
!---------------------------------------------------------------------------
           
  if (ichneu.eq.1) then ! if Newmann problem ... === if no Dirichlet ...
     if(dabs(sum(b)).gt.1.e-12) then
        print*
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'     The problem has no solution'
        print*,'     Please, verify the data'
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*
        stop 1
     else
        if(iopblo3.ne.1) then ! this should not happend due to the automatic blocking
           print*
           print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
           print*,'     Error:'
           print*,'     The solution of the problem is not unique'
           print*,'     and no blocking node was specified'
           print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
           print*
           stop 1
        endif
     endif
  endif
       
!---------------------------------------------------------------------------
!                        matrix calculation                               
!---------------------------------------------------------------------------
    
  print*,'Computing the system matrix...'
  print*,'Degrees of freeedom:',nver
  print*,'Number of vertex:',nver
  print*,'Number of elements:',nel
       
  call matriz()
 
!---------------------------------------------------------------------------
!                  Dirichlet boundary conditions                      
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
        do i=1,blofron%numero
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
!             strong imposition of Dirichlet conditions
!---------------------------------------------------------------------------

  opcion_bloq_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
     if (nrd .gt. 0) then
        do i=1,nrd
           call bloseg(b,mm,z,nra,nel,irefd(i))
        enddo
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
!               Choleski's factorization of the matrix                     
!---------------------------------------------------------------------------

  call chol(nver,mua,c)

!---------------------------------------------------------------------------
!                      system resolution                             
!---------------------------------------------------------------------------
   
  print*,'Solving the linear system'

  call sols(c,b,nver,mua)
     
  deallocate(c,mua)

  do i=1,nver
     sol(i) = b(i)
  enddo

  deallocate(b)

  return
end
