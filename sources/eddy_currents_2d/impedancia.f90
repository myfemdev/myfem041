      subroutine impedancia()

	 ! use parametros_electros 
      use electros_2D
      use malla_2DP1
      use derivados
      use voltage_drop
      use intensity_input
      use conductividad
      
      use dirichlet 
      
      use interf_impedancia
      
      implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      
      integer :: ii, i, k, j, nsdl,ind,nind,nsdi,iopblov,nxa
      integer :: l
	  double precision:: valor_rms, valor_phase
	  double complex:: aux, zi
	  integer:: iopconduc, ndom
	  
	  double complex, allocatable:: xa(:,:), xb(:), xy(:),baux(:),xintj(:),xintj_js(:),baux2(:) 
	  integer, allocatable:: ip(:), iq(:)
	  double complex, allocatable:: mataux(:,:),xbaux(:),res(:),xared(:,:)    

            
      
	  
	  zi = dcmplx(0.d0,1.d0)
	  
	  if(num_inputsv.gt.0 .and. num_inputsi.gt.0)then
!       En este caso será necesario bloquear algunos grados de libertad
!       al resolver A*V = I
!       Para calcular A es necesario resolver tantos sistemas como número de 
!       inductores totales (nind)	  
	    nind = num_inputsv + num_inputsi
	    nxa = num_inputsi
	    iopblov = 1
	  elseif(num_inputsv.eq.0 .and. num_inputsi.gt.0)then
	    nind = num_inputsi
	    nxa = num_inputsi
	    iopblov = 0
	  elseif(num_inputsi.eq.0)then
	     stop 'Error in impedancia, this routine is not necessary'
	  endif
	  
!     Dimensionamiento de distintos arrays  
	  
	  if (allocated(xa)) deallocate(xa) 
      allocate(xa(nind,nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xa'	
      
      if (allocated(co)) deallocate(co) 
      allocate(co(nind,nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array co'	
      
      if (allocated(xb)) deallocate(xb) 
      allocate(xb(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xb'
      
      if (allocated(xy)) deallocate(xy) 
      allocate(xy(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xy'
      
      if (allocated(xintj)) deallocate(xintj) 
      allocate(xintj(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xintj'
      
      if (allocated(xintj_js)) deallocate(xintj_js) 
      allocate(xintj_js(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xintj_js'		
      
      if (allocated(baux)) deallocate(baux) 
      allocate(baux(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array baux'	
      
      if (allocated(baux2)) deallocate(baux2) 
      allocate(baux2(nver), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array baux2'	
            
      
      if (allocated(ip)) deallocate(ip) 
      allocate(ip(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array ip' 
      
      if (allocated(iq)) deallocate(iq) 
      allocate(iq(nind), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array iq'
      
      if (allocated(mataux)) deallocate(mataux) 
      allocate(mataux(num_inputsi,num_inputsv), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array mataux'
      
      if (allocated(xbaux)) deallocate(xbaux) 
      allocate(xbaux(num_inputsv), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xbaux'
      
      if (allocated(res)) deallocate(res) 
      allocate(res(num_inputsi), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array res'
      
      if (allocated(xared)) deallocate(xared) 
      allocate(xared(num_inputsi,num_inputsi), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array xared'
      
!**************************************************************************
!     Primero se resuelve la parte de Stranded conductors                 *
!     para poder calcular la matriz de impedancias como matriz asociada   *
!     a una aplicación lineal (y no afín)                                 *
!     el segundo miembro fijo calculado hasta este momento está en "b"    *
!**************************************************************************

!**************************************************************************
!                      Bloqueo de b                                       *
!**************************************************************************
  
      if (dirichlet_bc%numero .gt. 0) then
            call bloseg2d(b)
      end if
         
!**************************************************************************
!                      resolucion del sistema                             *
!**************************************************************************

         print*,'------------------------------------------------------'
         print*,'solving the linear system (fixed part)                '
         print*,'------------------------------------------------------'

         call solsc(c,b,nver,mua) 
!**************************************************************************
!                      Se guarda esta solucion en baux2                   *
!**************************************************************************
         do i=1,nver
           baux2(i) = b(i)      
         enddo               
!**************************************************************************
!                      Bucle en el número de inductores                   *
!  para cálculo de la matriz de impedancias                               *
!**************************************************************************                    

      
      do ind = 1,nind
         baux = dcmplx(0.d0,0.d0)         
         co = dcmplx(0.d0,0.d0)
 
         co(ind,ind) = dcmplx(1.d0,0.d0)
         
        
         
          call semiaux(ind,baux) 
       
 !      call semiaux(ind ) - de forma que en semipot_aux se ensamblaria con 
 !      co(ii,ind) --- y hay que hacer el ensamblado tanto donde se conoce la intensidad 
 !      como donde se conoce el voltaje
       
!**************************************************************************
!                bloqueo segundo miembro  (cond. Dirichlet)               *
!**************************************************************************

         if (dirichlet_bc%numero .gt. 0) then
            call bloseg2d(baux)
         end if
         
!**************************************************************************
!                      resolucion del sistema                             *
!**************************************************************************

         print*,'------------------------------------------------------'
         print*,'solving the linear system'
         print*,'------------------------------------------------------'
         call solsc(c,baux,nver,mua)  
!      hai que calcular la intensidad total en cada inductor 
 
         call intensind_js(ind,xintj_js,baux2)      
           
         call intensind(ind,xintj,baux)
         
         do i=1,nind
           xa(i,ind) = xintj(i)
         enddo  
         
         
       
  
      enddo  
      
      !print*,'Matriz xa'
      !  do ind=1,nind
      !    do ii =1,nind
      !      print*,xa(ind,ii)
      !    enddo
      !  enddo
        
       if(iopblov.eq.1)then
          do ii =1,num_inputsi
            do ind=1,num_inputsv
                 mataux(ii,ind) = xa(ii,num_inputsi+ind)
                 valor_rms =  inputsv(ind)%vrms*dsqrt(2.d0)
		         valor_phase = inputsv(ind)%vphase
                 xbaux(ind) = valor_rms*(dcos(valor_phase*pi/180.d0) + zi*dsin(valor_phase*pi/180.d0))  
            enddo
         
          enddo
       endif
       
 
  
!Definicion del segundo miembro con las intensidades dadas para resolver
! xa*co = I
      do ind=1,num_inputsi
           valor_rms =  inputsi(ind)%vrms*dsqrt(2.d0)
		   valor_phase = inputsi(ind)%vphase
           xb(ind) = valor_rms*(dcos(valor_phase*pi/180.d0) + zi*dsin(valor_phase*pi/180.d0))-&
                     & xintj_js(ind)
      enddo
      
      
      if(iopblov.eq.1)then
         res = matmul(mataux,xbaux)
      
         do ii=1,nxa
           xb(ii) = xb(ii) - res(ii)
         enddo
     
        
      endif
      
          do ii=1,nxa
           do ind=1,nxa
             xared(ii,ind) = xa(ii,ind)
           enddo
         enddo
!   Factorización de xa
      call lupt(nxa,nxa,ip,iq,xared)  
      call sistluc(nxa,nxa,ip,iq,xared,xb,xy)
      
    
!xb contiene los verdaderos potenciales!

 !Se actualiza co! primero se pone a 0 y luego se actualiza la primera columna
 ! que será la utilizada en semi_aux
      do ind=1,nind
        do ii = 1,nind
          co(ii,ind) = dcmplx(0.d0,0.d0)
        enddo
      enddo

      do ind = 1,num_inputsi
       co(ind,1) = xb(ind)
      enddo
      
      do ind =num_inputsi+1,nind
         i = ind-num_inputsi
         valor_rms =  inputsv(i)%vrms*dsqrt(2.d0)
		 valor_phase = inputsv(i)%vphase
         co(ind,1) = valor_rms*(dcos(valor_phase*pi/180.d0) + zi*dsin(valor_phase*pi/180.d0))
      enddo
      
       call semiaux(1,baux) 
!**************************************************************************
!                bloqueo segundo miembro  (cond. Dirichlet)               *
!**************************************************************************

         if (dirichlet_bc%numero .gt. 0) then
            call bloseg2d(baux)
         end if
         
!**************************************************************************
!                      resolucion del sistema                             *
!**************************************************************************

         print*,'------------------------------------------------------'
         print*,'solving the linear system'
         print*,'------------------------------------------------------'

         call solsc(c,baux,nver,mua) 
         
         do i=1,nver
           b(i) = baux(i) + baux2(i)
           sol(i) = baux(i) + baux2(i)
         enddo
         
         call intensind(1,xintj,sol)
      !   print*,'Potenciales buenos eficaces'
      !   print*,(co(ii,1)/dsqrt(2.d0),ii=1,nind)

        if (allocated(pot_int)) deallocate(pot_int) 
        allocate(pot_int(num_inputsi), STAT = ierror)
        if (ierror .ne. 0) stop 'Error when allocating array pot_int'
        
        do i=1,num_inputsi
          pot_int(i) = co(i,1)/dsqrt(2.d0)
        enddo
        
        
        
        !deallocate(xa,xb,xy,ip,iq,baux,xbaux,mataux,res)
         
         
         return
         end        
   
   