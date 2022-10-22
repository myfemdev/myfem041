!***************************************************************************
!*                 resolucion del problema de electrostatica               *
!***************************************************************************

      SUBROUTINE dc3D()
    
      use malla_3DP1
      use electros3D
      use external_electros3D
      use derivados3D
      use resolucion_sistema_lineal
     
      implicit double precision  (a-h,o-z)

!***************************************************************************
!*                      calculo de la matriz                               *
!***************************************************************************
         
      CALL arint3D ()

!       print*,'Calculando matriz del sistema'
      CALL matriz3D()

!       print*,'despues de matriz'
       
! Inicializacion del segundo miembro del sistema
         b=0.d0
   
***************************************************************************
*                  calculo del vector segundo miembro                     *
***************************************************************************
! estaba en ! 0000 . cambiado para poder testar b

!      print*,'Calculando segundo miembro'
      
      CALL semi3D()     

!      print*,'despues de subrutina semi'

      
!********************************
! test solucion existente / unica
!********************************

      ! if Newmann problem ... === if no Dirichlet ...
      if (iopblo.ne.1 .or. (nrd.le.0.and.blofron%numero.le.0)) then
       if (dabs(sum(b)).gt.1.e-12) then
        print*,'   '
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'     The problem has no solution'
        print*,'     Please, verify the data'
        print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print*,'   '
        stop 1
       else
        if (iopblo3.ne.1) then
         ! non debera de suceder, xa que se bloquea automaticamente
         print*,'   '
         print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         print*,'     Error:'
         print*,'     The solution of the problem is not unique'
         print*,'     and no blocking node was specified'
         print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         print*,'   '
         stop 1
        endif
       endif
      endif


!***************************************************************************
!            condiciones de contorno Dirichlet                             *
!***************************************************************************

! bloqueo de la matriz 

      opcion_bloqueo_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
           if (nrd.gt.0) then                   
               call blomamor(camor,nrd,irefd,b)          
           end if
!      print*,'en if de bloqueo dirichlet 1'
      endif opcion_bloqueo_funcion
!      print*,'despues de bloqueo dirichlet'
      
      opcion_bloqueo_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
        if (blofron%numero.gt.0) then
         call blomamorc(camor,blofron%numero,blofron%referencias,
     &                  blofron%valor,b) 
!         print*,'en if de bloqueo dirichlet 2'
        end if
      endif opcion_bloqueo_constantes
       
       
      opcion_bloqueo_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
        if (blopun%numero.gt.0) then
          call blomamorp(camor,blopun%numero,blopun%referencias,
     &                   blopun%valor,b)
!        print*,'en if de bloqueo dirichlet 3'
        end if
      endif opcion_bloqueo_puntos

!      print*,'despues de opcion bloqueo puntos'


! 0000

***************************************************************************
*                bloqueo segundo miembro  (cond. Dirichlet)               *
***************************************************************************

      opcion_bloq_funcion: if(iopblo.eq.1.and.iopblo1.eq.1) then
      if (nrd .gt. 0) then
          CALL bloseg3D(b,nrd,irefd)
      end if
      endif opcion_bloq_funcion
   

      opcion_bloq_constantes: if(iopblo.eq.1.and.iopblo2.eq.1) then
      if (blofron%numero.gt.0) then
      CALL bloseg3Dc(b,blofron%numero,blofron%referencias,blofron%valor)
      end if
      endif opcion_bloq_constantes

      opcion_bloq_puntos: if(iopblo.eq.1.and.iopblo3.eq.1) then
!      print*,'BLOQUEO EN PUNTOS'
      if (blopun%numero.gt.0) then
        do  i=1,blopun%numero
         b(blopun%referencias(i))=blopun%valor(i)
        enddo
      end if
      endif opcion_bloq_puntos
      

!**************************************************************************
!               Se resuelve el sistema lineal
!**************************************************************************
      allocate(xcg(nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error:no se ha podido reservar memoria para xcg',nver
      stop 1
      endif 
      allocate(preex(nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error:no se ha podido reservar memoria para work',nver
      stop 1
      endif  
         
      
      sol=0.d0  ! Inicializamos el vector solucion
      xcg=b
      preex=b


            
!**************************************************************************
!               Se resuelve el sistema lineal: METODO DIRECTO
!**************************************************************************       
      
      if (iopsl.eq.1) then


c
c     no implementado
c
   

      
      else
      
!**************************************************************************
!               Se resuelve el sistema lineal: METODO ITERATIVO
!**************************************************************************

 !Dimensionamientos para gestion dinamica de memoria
      allocate(camorf(ib(nver+1)),stat=ierror)
      if (ierror.ne.0) then
      print*,'no se ha podido reservar memoria para camorf',ib(nver+1)
      stop 1
      endif
     
      allocate(work(7*nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'error:no se ha podido reservar memoria para work',7*nver
      stop 1
      endif  
   
     
! Calculo del precondicionador
         !print*,'en electrostatica3D, camor',camor
        CALL conlud (nver,ib,jb,camor,camorf) 
!        print*,'------------------------------------------------'
       ! print*,'en electrostatica3D, camorf',camorf
        
! Resolucion del sistema con gradiente conjugado

      !  print*,'en electrostatica 3D, antes de cg'
       ! print*,'segundo miembro b=',b

        CALL cg(nver,b,sol,work,nver,nitcg,epscg,  
     $                info, camor, camorf,ib,jb)

       print*,'info,nitcg,epscg',info,nitcg,epscg
       
c Calculo el residuo

        call matvec(1d0,sol,0d0,xcg,camor,nver,ib,jb)

        suma=0d0
        do i=1,nver
          suma=suma+(xcg(i)-preex(i))*(xcg(i)-preex(i))
        enddo
        print*,'Residuo de solucion sistema (Norma 2)= ',dsqrt(suma)


        
        DEALLOCATE(camorf)
      endif


!     Se libera espacio en memoria

      DEALLOCATE(camor)
      DEALLOCATE(ib)
      DEALLOCATE(jb)
      DEALLOCATE(b)

      return

      END
