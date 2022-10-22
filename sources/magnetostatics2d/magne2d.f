***************************************************************************
*                 resolucion del problema de magnetostatica               *
***************************************************************************

      SUBROUTINE magne2d()
    
      use malla_2DP1
      use electros_2D
      use external_electros
      use derivados
      use parametros_electros
      use nolineal
      use dirichlet
      use neumann
     
 
      implicit none
      

      integer          ::  n,i,k 

      double precision ::  p(2,ndel),p0(2,ndel), errorel, errorabs
      
      
***************************************************************************     
*                      calculo de la matriz                               *
***************************************************************************
    
 
      call matriz()
      print*,'the matrix has been computed'
      print*,'---------------------------------------------'
     
      
      ! bloqueo de la matriz 


      if (dirichlet_bc%numero.gt.0) then                   
        call blomat(c,mua)  
!      else 
!        c(mua(751))=1.d0        
      end if
        
 
      print*,'the matrix has been blocked'
      print*,'---------------------------------------------'


***************************************************************************
*               factorizacion de Choleski de la matriz                    *
***************************************************************************
      call chol(nver,mua,c)
      
      print*,'the matrix has been Choleski-factorized'
      print*,'---------------------------------------------'


     
***************************************************************************
*                  calculo del vector segundo miembro                     *
***************************************************************************     
       
      call semi ()
      
      if (iopdli.eq.0) then !todos los dominios son lineales.
        
***************************************************************************
*                bloqueo segundo miembro  (cond. Dirichlet)               *
***************************************************************************

         if (dirichlet_bc%numero. gt. 0) then
            call bloseg2d(b)
         end if
         

***************************************************************************
*                      resolucion del sistema                             *
***************************************************************************

         print*,'------------------------------------------------------'
         print*,'solving the linear system'
         print*,'------------------------------------------------------'

         call sols(c,b,nver,mua)
    
         do 4 i=1,nver
            sol(i) = b(i)
    4    continue

         print*,'the linear system has been solved'
         print*,'------------------------------------------------------'

   
    
   
      else  !algun dominio es no lineal, algoritmo iterativo
             
        do k=1,nel
          p(1,k)=0.d0
          p(2,k)=0.d0
        end do
             
        allocate(bvar(nver),stat=ierror)
        if (ierror.ne.0) then
          print*,'error: bvar cannot be allocated',nver
          stop 1
        endif
      
      
        print*,'starting iterations'

        bucle_iteraciones : do n=1,niter
       
          bvar=b
          
       
          call segvar(nel,nver,z,mm,p,bvar,ndnolin,idnolin,nsd)
       
     
***************************************************************************
*                bloqueo segundo miembro  (cond. Dirichlet)               *
***************************************************************************


          if (dirichlet_bc%numero .gt. 0) then
            call bloseg2d(bvar)
          end if

          


***************************************************************************
*                      resolucion del sistema                             *
***************************************************************************

   
!          print*,'Solving the linear system'
          print*,'----------------------------------------------------'

    
          call sols(c,bvar,nver,mua)
     

          do 3 i=1,nver
            sol(i) = bvar(i)
    3     continue

          p0=p

          call renovacion(p)
      
          errorabs=maxval(abs(p-p0))
          errorel=maxval(abs(p-p0)/(abs(p)+e))
      
          print*,'iteration ',n
          print*,'relative error=', errorel
          print*,'absolute error=', errorabs


       
          if (errorel.le.e) then 
           print*
           print*,'----------------------------------------------------'
           print*,'convergence at iteration: ', n
           print*,'----------------------------------------------------'
           print*

           exit        
          endif

        end do bucle_iteraciones
        
        if(n.ge.niter) then
        
          print*
          print*,'----------------------------------------------------'
          print*,'no convergence in ',niter,' iterations'
          print*,'----------------------------------------------------'
          print*
          
        endif
        
      endif
      
      call calrot()
      call calcampomag(p) 
      
   
      deallocate(c)
      deallocate(mua)

      return

      end
