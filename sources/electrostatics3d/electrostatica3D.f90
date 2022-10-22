!---------------------------------------------------------------------------
!                   SOLVING OF THE ELECTROSTATIC PROBLEM                
!---------------------------------------------------------------------------

subroutine electrostatica3D()
    
  use malla_3DP1
  use electros3D
  use external_electros3D
  use derivados3D
  use resolucion_sistema_lineal
     
  implicit none
  integer          :: info
  double precision :: suma
  integer          :: i

!---------------------------------------------------------------------------
!                        matrix calculation                               
!---------------------------------------------------------------------------
  call arint3D ()

  call matriz3D_orto()
       
!---------------------------------------------------------------------------
!                  calculation of the second member vector                     
!---------------------------------------------------------------------------
  b=0.d0

  call semi3D()     

!---------------------------------------------------------------------------
!  Verification of the existence of the problem solution
!---------------------------------------------------------------------------

! if Newmann problem ... === if no Dirichlet ...
  if (iopblo.ne.1 .or. (nrd.le.0.and.blofron%numero.le.0)) then
     if (dabs(sum(b)).gt.1.e-12) then
        print*
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'     The problem has no solution'
        print*,'     Please, verify the data'
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*
        stop 1
     else
        if (iopblo3.ne.1) then
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
!                  Dirichlet boundary conditions                      
!---------------------------------------------------------------------------

  opcion_bloqueo_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
     if (nrd.gt.0) then                   
        call blomamor(camor,nrd,irefd,b)          
     end if
  endif opcion_bloqueo_funcion
      
  opcion_bloqueo_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
     if (blofron%numero.gt.0) then
        call blomamorc(camor,blofron%numero,blofron%referencias,blofron%valor,b) 
     end if
  endif opcion_bloqueo_constantes
       
  opcion_bloqueo_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
     if (blopun%numero.gt.0) then
        call blomamorp(camor,blopun%numero,blopun%referencias,blopun%valor,b)
     end if
  endif opcion_bloqueo_puntos

  opcion_bloq_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
     if (nrd .gt. 0) then
        call bloseg3D(b,nrd,irefd)
     end if
  endif opcion_bloq_funcion
   
  opcion_bloq_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
     if (blofron%numero.gt.0) then
        call bloseg3Dc(b,blofron%numero,blofron%referencias,blofron%valor)
     end if
  endif opcion_bloq_constantes

  opcion_bloq_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
     if (blopun%numero.gt.0) then
        do i=1,blopun%numero
           b(blopun%referencias(i))=blopun%valor(i)
        enddo
     end if
  endif opcion_bloq_puntos
      
!---------------------------------------------------------------------------
!                         Linear system resoltuion 
!---------------------------------------------------------------------------

  if(allocated(xcg))deallocate(xcg)
  allocate(xcg(nver),stat=ierror)
  if (ierror.ne.0) then
    print*,'Error while allocating array xcg',nver
    stop 1
  endif 
  if(allocated(preex))deallocate(preex)
  allocate(preex(nver),stat=ierror)
  if (ierror.ne.0) then
    print*,'Error while allocating work',nver
    stop 1
  endif  
      
  sol=0.d0 
  xcg=b
  preex=b

!---------------------------------------------------------------------------
!               Linear system resolution: DIRECT METHOD
!---------------------------------------------------------------------------
      
  if (iopsl.eq.1) then

! NOT IMPLEMENTED
      
  else
      
!---------------------------------------------------------------------------
!               Linear system resolution: ITERATIVE METHOD
!---------------------------------------------------------------------------

    if(allocated(camorf))deallocate(camorf)
    allocate(camorf(ib(nver+1)),stat=ierror)
    if (ierror.ne.0) then
       print*,'Error while allocating array camorf',ib(nver+1)
       stop 1
    endif
    
    if(allocated(work))deallocate(work) 
    allocate(work(7*nver),stat=ierror)
    if (ierror.ne.0) then
       print*,'Error while allocating array work',7*nver
       stop 1
    endif  
   
! COMPUNTING OF THE PRECONDITIONER
    call conlud(nver,ib,jb,camor,camorf) 
        
! SOLVING OF THE LINEAR SYSTEM WITH THE CONJUGATE GRADIENT METHOD
    call cg(nver,b,sol,work,nver,nitcg,epscg,info,camor,camorf,ib,jb)
     
! ERROR COMPUTATION
    call matvec(1d0,sol,0d0,xcg,camor,nver,ib,jb)
    suma=0d0
    do i=1,nver
       suma=suma+(xcg(i)-preex(i))*(xcg(i)-preex(i))
    enddo
    print*,'Euclidean norm of the error = ',dsqrt(suma)
        
    deallocate(camorf)
  endif

  deallocate(camor,ib,jb,b)

  return
end
