***************************************************************************
*                 resolucion del problema de eddy currents                *
***************************************************************************

      SUBROUTINE eddy2d()
    
      use malla_2DP1
      use electros_2D
      use derivados
!  use parametros_electros
      use dirichlet
      use neumann
      use intensity_input
      use voltage_drop
      
      use interf_eddy
     
 
      implicit none
      
     
      
******************************************************************************   
      
      integer          ::  n,i,k,j 
      
    
*************************************************************************** 
*     Previous computations    
***************************************************************************  

      call preveddy()   
      
*************************************************************************** 
*     Computation of matrix    
***************************************************************************       


      call cmrds()
      
      print*,'the matrix has been computed'
      print*,'---------------------------------------------'
     
      
      ! bloqueo de la matriz 


      if (dirichlet_bc%numero.gt.0) then                    
        call blomat(c,mua)  
      end if
        
 
      print*,'the matrix has been blocked' 
      print*,'---------------------------------------------' 


***************************************************************************
*               factorizacion de Choleski de la matriz                    *
***************************************************************************
      call cholc(nver,mua,c) 
      
      print*,'the matrix has been Choleski-factorized'
      print*,'---------------------------------------------'

***************************************************************************
*                  calculo del vector segundo miembro                     *
***************************************************************************     
       
      call semi ()
      
      if(num_inputsi .eq.0 .and. num_inputsv.gt.0 )then
      
         call semipot ()
 
        elseif(num_inputsi.gt.0)then
           print*,'Calling impedance'
           call impedancia()
        else
          print*,'Only stranded conductors in the domain'
        endif
        
        
        
       if(num_inputsi .eq.0 .and. (num_inputsv.gt.0 .or. 
     &                  sourcevol%numero .gt. 0))then      
      
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

      
         call solsc(c,b,nver,mua)
         do 4 i=1,nver
            sol(i) = b(i)
    4    continue   
      
     
         print*,'the linear system has been solved'
         print*,'------------------------------------------------------'

       endif  
         !calculo de B y H

        call calrot()
        call calcampomag( ) 
   
         
        
      return

      end
