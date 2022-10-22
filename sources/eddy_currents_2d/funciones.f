************************************************************************
*    User-defined functions
************************************************************************
 
      DOUBLE COMPLEX FUNCTION fjs(x,y,k,indice,itipo,modo,vrms,vphase,
     &                            etiqueta)

      use sourcevolumic
      use electros_2D
      
      double precision    :: x,y,vrms,vphase
      double complex      :: zi
      integer, INTENT(IN) :: k,indice,itipo, modo
      character*9         :: etiqueta
      
      zi = dcmplx(0.d0,1.d0)
      
      if(modo.eq.1)then
        
        if(trim(etiqueta).eq.'user') then
        
          if(itipo.eq.1) then
            fjs=0.d0
          else
            fjs=0.d0
          end if
        end if
        
      else if (modo.eq.2) then
        if (itipo.eq.1) then
          fjs=vrms*(dcos(vphase*pi/180.d0) + zi*dsin(vphase*pi/180.d0))
        else
          fjs=vrms*(dcos(vphase*pi/180.d0) + zi*dsin(vphase*pi/180.d0))
     &          /area(indice)
        end if
        
      end if
      
      return
      
      end function fjs
      
    
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      DOUBLE COMPLEX FUNCTION  h(x,y,iref,indice,modo,valor,etiqueta)

      double precision, INTENT(IN) :: x, y
      double complex               :: valor
      integer, INTENT(IN)          ::iref
      integer, INTENT(IN)          ::indice, modo
      character*9                  ::etiqueta 
      
      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'user') h=0.d0
        
      else if (modo.eq.2) then
      
        h=valor
        
      end if

      end function h
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      DOUBLE COMPLEX FUNCTION  g(x,y,iref,indice,modo,valor,etiqueta)

      double precision, INTENT(IN) :: x, y
      double complex               :: valor
      integer, INTENT(IN)          :: iref
      integer, INTENT(IN)          :: indice, modo
      character*9                  :: etiqueta 
      
      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'user') g=0.d0
        
      else if (modo.eq.2) then
      
        g=valor
        
      end if

      end function g
      
      
      
    













