    subroutine sol1()
    use electros_2D
    use malla_2DP1
    use derivados
    use nolineal
    use parametros_electros
    use tiempo
    
    implicit none
    
    integer :: n,i
    double precision :: errorabs,errorel,error_abs,error_rel,solexac
    

    ipat=0
    
    print*,'----------------------------------------------------------'
    print*, 'computing the initial condition'
    print*,'----------------------------------------------------------'

    
    allocate(mua1(nver+1),stat=ierror)
    if (ierror.ne.0) then
        print*,'error: mua1 cannot be allocated'
        stop
    endif

    call cmua(mua1,mm,nel,nver)

!    write(17,*) (mua1(i),i=1,nver+1) 

    allocate(c1(mua1(nver+1)),stat=ierror)
      
    if (ierror.ne.0) then
        print*,'error: c1 cannot be allocated'
        stop
    endif

    call matriz1()

    print*,'the initial matrix has been computed'
    print*,'---------------------------------------------'
    
    if (dirichlet_bc%numero.gt.0) then                   
        call blomat(c1,mua1)    
    end if
    print*,'the initial matrix has been blocked'
    print*,'----------------------------------------------'
    
    
    call chol(nver,mua1,c1)
    print*,'the initial matrix has been Choleski-factorized'
    print*,'----------------------------------------------'
    
    
    call semi1()   
    
    if (iopdli.eq.0) then 
        
      if (dirichlet_bc%numero.gt.0) then
         call bloseg2d(b)
      end if
        
      print*,'solving the linear system'
      print*,'-----------------------------------------------------'

      call sols(c1,b,nver,mua1)
    
      do  i=1,nver
        sol(i) = b(i)
      end do
      print*,'the linear system has been solved'
      print*,'-----------------------------------------------------'
    
    else
         
       p=0
         
       allocate(bvar(nver),stat=ierror)
       if (ierror.ne.0) then
         print*,'error: bvar cannot be allocated',nver
         stop
       endif
        
       print*,'starting iterations'
          
       do n=1,niter
         print*,'-------------------------------------------'
         print*,'iteration ',n
         print*,'-------------------------------------------'
       
         bvar=b
            
         call segvar(nel,nver,z,mm,bvar,ndnolin,idnolin,nsd)
            
         if (dirichlet_bc%numero .gt. 0) then
           call bloseg2d(bvar)
         end if
            
         call sols(c1,bvar,nver,mua1)
   
            
         p0=p

         call renovacion()
            
         errorabs=maxval(dabs(p-p0))
         errorel=maxval(dabs(p-p0)/(dabs(p)+e))
           

         print*,'relative error=', errorel
         print*,'absolute error=', errorabs


       
         if (errorel.le.e.or. errorabs.le.e)then
           print*
           print*,'----------------------------------------'
           print*,'convergence in ',n,' iterations'
           print*,'----------------------------------------'
           do  i=1,nver
             sol(i) = bvar(i)
           end do

           exit        
         endif
            
       end do 
        
       if(n.ge.niter) then
       
         print*
         print*,'--------------------------------------------------'
         print*,'no convergence in ',niter,' iterations'
         print*,'--------------------------------------------------'
          
       endif
          
     endif
     
     deallocate (mua1)
     deallocate (c1)
     
     call calrot()
     call calcampomag()
     

     return
     end subroutine       
           
