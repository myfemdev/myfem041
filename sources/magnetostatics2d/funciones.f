************************************************************************
*    User-defined functions
************************************************************************

************************************************************************
*   Volumic current density
*   itipo = 1 (current density)
*   itipo = 2 (current intensity)
*           modo = 1 (function)
*           modo = 2 (constant)
*   Current intensity only can be CONSTANT (not function)
************************************************************************
 
      DOUBLE PRECISION FUNCTION f(x,y,nsdk,indice,itipo,modo,valor,
     &                            etiqueta)

      use sourcevolumic
     
      double precision    :: x,y,valor
      integer, INTENT(IN) :: nsdk,indice,itipo, modo
      character*255       :: etiqueta
      
      if(modo.eq.1)then
        
        if(trim(etiqueta).eq.'User_Defined') then
        
          if(itipo.eq.1) then
            f=0.d0
          end if
        end if
        
      else if (modo.eq.2) then
          
        if (itipo.eq.1) then
          f=valor
        else
          f=valor/area(indice)
        end if
        
      end if
      
      return
      
      end function f
      

************************************************************************
*   Surface current density
*   itipo = 1 (current density)
*   itipo = 2 (current intensity)
*           modo = 1 (function)
*           modo = 2 (constant)
*   Current intensity only can be CONSTANT (not function)
************************************************************************      
      DOUBLE PRECISION FUNCTION  gs(x,y,iref,indice,itipo,modo,valor,
     &                              etiqueta)
     
      use sourcesurface
      
      double precision, INTENT(IN) :: x, y, valor
      integer, INTENT(IN)          :: iref
      integer, INTENT(IN)          :: indice, itipo, modo
      character*255                :: etiqueta 
      
      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'User_Defined') then
        
          if(itipo.eq.1) then
            gs=0.d0
          end if
          
        end if
      
        
      else if (modo.eq.2) then
      
        if (itipo.eq.1) then
          gs=valor
        else
          gs=valor/xlongsu(indice)
        end if
        
      end if

      end function gs
      
*************************************************************************
*   Dirichlet boundary condition
*
************************************************************************
      DOUBLE PRECISION FUNCTION  h(x,y,iref,indice,modo,valor,etiqueta)

      double precision, INTENT(IN) :: x, y, valor
      integer, INTENT(IN)          ::iref
      integer, INTENT(IN)          ::indice, modo
      character*255                ::etiqueta 
      
      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'User_Defined') h=0.d0
        
      else if (modo.eq.2) then
      
        h=valor
        
      end if

      end function h
      
*************************************************************************
*   Neumann boundary condition
*
************************************************************************      
      DOUBLE PRECISION FUNCTION  g(x,y,iref,indice,modo,valor,etiqueta)

      double precision, INTENT(IN) :: x, y, valor
      integer, INTENT(IN)          :: iref
      integer, INTENT(IN)          :: indice, modo
      character*255                :: etiqueta 
      
      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'User_Defined') g=0.d0
        
      else if (modo.eq.2) then
      
        g=valor
        
      end if

      end function g
      
*************************************************************************
*   Exact solution: not used in general 
*   It has been used to check examples with analytical solution
************************************************************************      
       DOUBLE PRECISION FUNCTION  solexac(x,y)

      implicit double precision (a-h,o-z)
      double precision, INTENT(IN) :: x, y
    
      r=dsqrt(x*x+y*y)
      
!      int=1401.d0*4.d0*dasin(1.d0)
!      perme0 = 1.2566370614e-6
!      if (r.le.1.d0) then
!        solexac=perme0*int*dlog(1.401d0)/(4.d0*dasin(1.d0))
!      else if (r.gt. 1.d0. and . r.le.1.401d0) then
!        solexac=perme0*int*dlog(1.401d0/r)/(4.d0*dasin(1.d0)) 
!      else
!        solexac=0.d0
!      end if
      
      
      xint=5000.89d0
      xjs=5000.89d0
      perme0 = 1.2566370614e-6
      permerela=3000.d0
      
      r1=1.d0
      r2=1.401d0
      alfa=2.d0*xjs/(2.d0*dasin(1.d0))
      xk=2.d0*dasin(1.d0)*(permerela-1.d0)*perme0/(2.d0*xjs)
      beta=xk*xint/(4.d0*dasin(1.d0))
      
      if(r.ge.0.d0.and.r.le.r1) then
        solexac=perme0*xint/(4.d0*dasin(1.d0))*dlog(r2)+
     &   alfa*(beta/2.d0*dlog(beta**2+r2**2)+r2*datan(beta/r2))-
     &   perme0*xint/(4.d0*dasin(1.d0))*dlog(r1)-
     &   alfa*(beta/2.d0*dlog(beta**2+r1**2)+r1*datan(beta/r1))
      else if(r.ge.r1.and.r.le.r2) then
        solexac=perme0*xint/(4.d0*dasin(1.d0))*dlog(r2)+
     &   alfa*(beta/2.d0*dlog(beta**2+r2**2)+r2*datan(beta/r2))-
     &   perme0*xint/(4.d0*dasin(1.d0))*dlog(r)-
     &   alfa*(beta/2.d0*dlog(beta**2+r**2)+r*datan(beta/r))
      else
        solexac=0.d0
      end if
      
   

      end function solexac














