
************************************************************************
* Funcion densidad de carga volumica (segundo miembro)
* x,y,z: coordenadas del punto
* nsd  : numero de subdominio del elemento.
************************************************************************
 
      DOUBLE PRECISION FUNCTION f(x,y,z,nsd,indice)

      use derivados3D

      implicit double precision (a-h,o-z)
      integer, INTENT(IN)::indice ! indice para el array de cargas volumicas

      f=0.d0

      if (vol%fun(indice) == 1)     then ! 'Function defined by user'
      		f=0.d0
      endif

      return
      end


************************************************************************
*     Funcion solucion exacta  de los ejemplos
************************************************************************
 
      DOUBLE PRECISION FUNCTION fexac(x,y,z)

      use electros3D, only: iopej
      use derivados3D

      implicit double precision (a-h,o-z)
 
      fexac=0.d0
  
      !'Example 1: cylinder'    
!        if (dir%funs == 2) then  ! 'cylinder'
!           fexac=3.*z-6.  
!           
!        elseif (dir%funs == 4) then  ! 'ortotropy'
!           fexac=x+2.*y+3*z-6.
!      !'User defined: Function defined by user'
!         else  
!            fexac = 0.d0     
!         endif
 
      return
        end   
        
!     	-----------------------------------     	
      DOUBLE PRECISION FUNCTION evalta(nn,xt,yt,x)
 !     	----------------------------------- 
***** X****************************************************************X
*     funcion que evalua la tabla (xt,yt) que tiene
*                    nn datos en el punto x                                                    *
***** X****************************************************************X
      
	implicit double precision (a-h,o-z)
	integer, INTENT(IN)::nn
	double precision, INTENT(in) :: x
	dimension xt(*),yt(*)
c     comprobacion
      if (nn .lt. 2) stop 'La tabla no es valida'
c     extrapolacion a la izquierda
      if (x .le. xt(1)) then
        call punto1(xt(1),yt(1),xt(2),yt(2),x,y)
        evalta = y
        return
      endif
c     interpolacion
      do 1 ic=2,nn
        if (x .le. xt(ic)) then
          call punto1(xt(ic-1),yt(ic-1),xt(ic),
     &    yt(ic),x,y)
          evalta = y
          return
        endif
    1 continue
c     extrapolacion a la derecha
      call punto1(xt(nn-1),yt(nn-1),xt(nn),
     &yt(nn),x,y)
      evalta = y
      return

      end function evalta










