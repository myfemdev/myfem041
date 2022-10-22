************************************************************************
*    User-defined functions
************************************************************************

************************************************************************
*   Volumic current density
*   itipo = 1 (current density)
*   itipo = 2 (current intensity)
*           modo = 1 (function)
*           modo = 2 (constant)
*   Current intensity may be a function because it may depend on time
************************************************************************
 
      DOUBLE PRECISION FUNCTION f(time,x,y,nsdk,indice,itipo,modo,valor,
     &                            etiqueta)

      use sourcevolumic
     
      double precision    :: time,x,y,valor
      integer, INTENT(IN) :: nsdk,indice,itipo, modo
      character*255       :: etiqueta
      
      if(modo.eq.1)then
        
        if(trim(etiqueta).eq.'User_Defined') then
        
          if(itipo.eq.1) then
            f=0.d0
          else
            f=0.d0 !in this case, f = function/areapo(indice)
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
*   itipo = 1 (surface current density)
*   itipo = 2 (surface current intensity)
*           modo = 1 (function)
*           modo = 2 (constant)
*   Current intensity may be a function because it may depend on time
************************************************************************
      
      DOUBLE PRECISION FUNCTION  gs(time,x,y,iref,indice,itipo,modo,
     &                              valor,etiqueta)
     
      use sourcesurface
      
      double precision, INTENT(IN) :: time, x, y, valor
      double precision pi
      integer, INTENT(IN)          :: iref
      integer, INTENT(IN)          :: indice, itipo, modo
      character*255                :: etiqueta
      

      if(modo.eq.1)then
      
        if(trim(etiqueta).eq.'User_Defined') then
        
           if(itipo.eq.1)then
             gs = 0.d0
           else
             gs = 0.d0 !in this case, gs = function/xlongsu(indice)
           endif
           
        elseif(trim(etiqueta).eq.'Example_3')then   
          
          if(itipo.eq.2) then
          
            if (iref.eq.4) then       
              pi=dacos(-1.d0)     
              gs=-3000.d0*dcos(2.d0*pi*time)/xlongsu(indice) 
            else if (iref.eq.5) then
              pi=dacos(-1.d0)     
              gs=3000.d0*dcos(2.d0*pi*time)/xlongsu(indice)
            end if
         
          endif
            
         endif
            
             
             
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
      
      
      DOUBLE PRECISION FUNCTION  h(time,x,y,iref,indice,modo,valor,
     &                             etiqueta)

      double precision, INTENT(IN) :: time, x, y, valor
      integer, INTENT(IN)          :: iref
      integer, INTENT(IN)          :: indice, modo
      character*255                :: etiqueta
      
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
      
      DOUBLE PRECISION FUNCTION  g(time,x,y,iref,indice,modo,valor,
     &                             etiqueta)

      double precision, INTENT(IN) :: time, x, y, valor
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
*   Electrical conductivity for volumic voltage drops
*
************************************************************************      
      
      double precision function sigma_vol(indice,modo,valor,
     &                            etiqueta)
     
      double precision    :: valor
      integer, INTENT(IN) :: indice, modo
      character*255       :: etiqueta
      
      if(modo.eq.1)then
        sigma_vol=0.d0
        if(trim(etiqueta).eq.'Material name') then
          sigma_vol=0.d0
        end if
        
      else if (modo.eq.2) then
      
        sigma_vol=valor
        
      end if
      
      
      
      return
      
      end function sigma_vol
*************************************************************************
*   Electrical conductivity for surface voltage drops
*
************************************************************************ 
      double precision function sigma_sur(indice,modo,valor,
     &                                etiqueta)
     
      implicit none
      double precision    :: valor
      integer, INTENT(IN) :: indice, modo
      character*255       :: etiqueta
      
      if(modo.eq.1)then
        sigma_sur=0.d0
        if(trim(etiqueta).eq.'Material name') then
          sigma_sur=0.d0
        end if
        
      else if (modo.eq.2) then
      
        sigma_sur=valor
        
      end if
      
      return
      
      end function sigma_sur
      
************************************************************************
*    Initial intensity for volumic voltage drop
************************************************************************     
      DOUBLE PRECISION FUNCTION fpo(indice,valor)
   
      use potenciales_vol
     
      double precision , INTENT(IN)   :: valor
      integer, INTENT(IN)             :: indice

        fpo=valor/areapo(indice)

      return
      
      end function fpo
      
*  ************************************************************************
*    Initial intensity for surface voltage drop
************************************************************************     
      DOUBLE PRECISION FUNCTION  gspo(indice,valor)
   
      use potenciales_sur
      
      double precision, INTENT(IN) :: valor
      integer, INTENT(IN)          :: indice
      
      gspo=valor/xlongpo(indice)
 
    
      end function gspo
      
************************************************************************
*    Functions C_i(t) volumic - voltage drops in single conductors
************************************************************************
 
      DOUBLE PRECISION FUNCTION pot_vol(time,indice,modo,valor,
     &                                  etiqueta)

      use potenciales_vol
      implicit double precision(a-h,o-z)

      character*255       :: etiqueta,etiqueta1
      if(modo.eq.1)then
        
           
        if(trim(etiqueta).eq.'Example_1') then
!Example_1: I(t) = 3000*sin(2pi50t)
!C_i(t): analytical solution from I(t)        
           pi=dacos(-1.d0)
           freq=50.d0
           perme0 = 1.2566370614e-6
         
           xint=3000*dsin(2*pi*freq*time)
           dint=3000*2*pi*freq*dcos(2*pi*freq*time)
           xjs=1.75d0
           permerela=5000.d0
           ne=1
     
           alfa=2.d0*xjs/pi
           
           delta=pi*(permerela-1.d0)*perme0/(2.d0*xjs)
           beta=delta*ne*xint/(2.d0*pi)
           
 
           r1=1.d0
           r2=1.401d0
     
         if(indice.eq.1) then
           modo1=potencial_dat_vol%modo1(indice)           !anhadido
           valor1=potencial_dat_vol%valor1(indice)         !anhadido
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   !anhadido
           espesor=potencial_dat_vol%valor0(indice)        !anhadido

           sigma1=sigma_vol(indice,modo1,valor1,           !anhadido
     &                                etiqueta1)           !anhadido

           pot_vol=xint*ne/(2.d0*pi*r1*sigma1)
     &           
           e=dlog((beta**2+r2**2)/(beta**2+r1**2))
           
           pot_vol=-pot_vol-ne*dint/(2.d0*pi)*(perme0*dlog(r2/r1)
     &             +delta*alfa*0.5d0*e)
         else if (indice.eq.2) then
                    modo1=potencial_dat_vol%modo1(indice)           !anhadido
           valor1=potencial_dat_vol%valor1(indice)         !anhadido
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   !anhadido
           espesor=potencial_dat_vol%valor0(indice)        !anhadido

           sigma2=sigma_vol(indice,modo1,valor1,           !anhadido
     &                                etiqueta1)           !anhadido
        
           pot_vol=xint*ne/(2.d0*pi*r2*sigma2)
           
         else
           
           print*,'error en funciones'
           stop
         end if       
         
!!!!!!!!!!!!!!!!!!!!!!!         
         elseif(trim(etiqueta).eq.'Example_2') then
         !Example_2: I(t) = 3000*cos(2pi50t)
         !C_i(t): analytical solution from I(t) 
       
           pi=dacos(-1.d0) 
           freq=50.d0
           perme0 = 1.2566370614e-6

           xint=3000*dcos(2*pi*freq*time)
           dint=-3000*2*pi*freq*dsin(2*pi*freq*time)
           xjs=1.75d0
           permerela=5000.d0
           ne=1
     
           alfa=2.d0*xjs/pi
           
           delta=pi*(permerela-1.d0)*perme0/(2.d0*xjs)
           beta=delta*ne*xint/(2.d0*pi)
           
 
           r1=1.d0
           r2=1.401d0
           

         if(indice.eq.1) then
           modo1=potencial_dat_vol%modo1(indice)           
           valor1=potencial_dat_vol%valor1(indice)        
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   
           espesor=potencial_dat_vol%valor0(indice)        

           sigma1=sigma_vol(indice,modo1,valor1,           
     &                                etiqueta1)               
           pot_vol=xint*ne/(2.d0*pi*r1*sigma1)
     &           
           e=dlog((beta**2+r2**2)/(beta**2+r1**2))
           
           pot_vol=-pot_vol-ne*dint/(2.d0*pi)*(perme0*dlog(r2/r1)
     &             +delta*alfa*0.5d0*e)
         else if (indice.eq.2) then
           modo1=potencial_dat_vol%modo1(indice)           
           valor1=potencial_dat_vol%valor1(indice)         
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   
           espesor=potencial_dat_vol%valor0(indice)       

           sigma2=sigma_vol(indice,modo1,valor1,           
     &                                etiqueta1)           
         
           pot_vol=xint*ne/(2.d0*pi*r2*sigma2)
           
         else
           
           print*,'error en funciones'
           stop
         end if     
         
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       elseif(trim(etiqueta).eq.'User_Defined') then

     
         if(indice.eq.1) then
           modo1=potencial_dat_vol%modo1(indice)           !anhadido
           valor1=potencial_dat_vol%valor1(indice)         !anhadido
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   !anhadido
           espesor=potencial_dat_vol%valor0(indice)        !anhadido

           sigma1=sigma_vol(indice,modo1,valor1,           !anhadido
     &                                etiqueta1)           !anhadido

           pot_vol=0.d0
     &           
       
         else if (indice.eq.2) then
           modo1=potencial_dat_vol%modo1(indice)           !anhadido
           valor1=potencial_dat_vol%valor1(indice)         !anhadido
           etiqueta1=potencial_dat_vol%etiqueta1(indice)   !anhadido
           espesor=potencial_dat_vol%valor0(indice)        !anhadido
           sigma2=sigma_vol(indice,modo1,valor1,           
     &                                etiqueta1)
   
        
           pot_vol=0.d0
           
         else
           
           print*,'error en funciones'
           stop
         end if                  
         endif
        
      else if (modo.eq.2) then
          
        pot_vol=valor

        
      end if
      
      return
      
      end function pot_vol
      
************************************************************************
*    Functions C_i(t) surface - surface voltage drops in single conductors
************************************************************************      
      DOUBLE PRECISION FUNCTION pot_sur(time,indice,modo,valor,
     &                            etiqueta)

      use potenciales_sur
     
      implicit double precision   (a-h,o-z)

      character*255       :: etiqueta,etiqueta1

      if(modo.eq.1)then
        
        if(trim(etiqueta).eq.'Example_1') then
        !I(t) = 3000*sin(2pi50t)
        !C_i(t): analytical solution from I(t) 
           pi=dacos(-1.d0)
           freq=50.d0
           perme0 = 1.2566370614e-6

           xint=3000*dsin(2*pi*freq*time)
           dint=3000*2*pi*freq*dcos(2*pi*freq*time)
           xjs=1.75d0
           permerela=5000.d0
           ne=1
     
           alfa=2.d0*xjs/pi
           
           delta=pi*(permerela-1.d0)*perme0/(2.d0*xjs)
           beta=delta*ne*xint/(2.d0*pi)
           
 
           r1=1.d0
           r2=1.401d0
           

         if(indice.eq.1) then
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

           sigma1=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
     
           pot_sur=xint*ne/(2.d0*pi*r1*sigma1*espesor)
     &           
           e=dlog((beta**2+r2**2)/(beta**2+r1**2))
           
           pot_sur=-pot_sur-ne*dint/(2.d0*pi)*(perme0*dlog(r2/r1)
     &             +delta*alfa*0.5d0*e)
           
         else if (indice.eq.2) then
         
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

         
           sigma2=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
         
           pot_sur=xint*ne/(2.d0*pi*r2*sigma2*espesor)
           
         else
           
           print*,'error en funciones'
           stop
         end if
    !!!!!!
    
      elseif(trim(etiqueta).eq.'Example_2') then
      !I(t) = 3000*cos(2pi50t)
       !C_i(t): analytical solution from I(t) 
           pi=dacos(-1.d0)
           freq=50.d0
           perme0 = 1.2566370614e-6
           
           xint=3000*dcos(2*pi*freq*time)
           dint=-3000*2*pi*freq*dsin(2*pi*freq*time)
            xjs=1.75d0
           permerela=5000.d0
           ne=1
     
           alfa=2.d0*xjs/pi
           
           delta=pi*(permerela-1.d0)*perme0/(2.d0*xjs)
           beta=delta*ne*xint/(2.d0*pi)
           
 
           r1=1.d0
           r2=1.401d0
         if(indice.eq.1) then
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

           sigma1=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
           pot_sur=xint*ne/(2.d0*pi*r1*sigma1*espesor)
     &           
           e=dlog((beta**2+r2**2)/(beta**2+r1**2))
           
           pot_sur=-pot_sur-ne*dint/(2.d0*pi)*(perme0*dlog(r2/r1)
     &             +delta*alfa*0.5d0*e)

         else if (indice.eq.2) then
         
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

         
           sigma2=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
           pot_sur=xint*ne/(2.d0*pi*r2*sigma2*espesor)
           
         else
           
           print*,'error en funciones'
           stop
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!
       elseif(trim(etiqueta).eq.'User_Defined') then
    
         if(indice.eq.1) then
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

           sigma1=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
           pot_sur = 0.d0

         else if (indice.eq.2) then
         
           modo1=potencial_dat_sur%modo1(indice)
           valor1=potencial_dat_sur%valor1(indice)
           etiqueta1=potencial_dat_sur%etiqueta1(indice)
           espesor=potencial_dat_sur%valor0(indice)

         
           sigma2=sigma_sur(indice,modo1,valor1,
     &                                etiqueta1)
           pot_sur=0.d0
           
         else
           
           print*,'error en funciones'
           stop
         end if       
       
        end if
        
      else if (modo.eq.2) then
          
        pot_sur=valor
        
      end if
      
      return
      
      end function pot_sur
      
************************************************************************
*    Functions V(t) volumic - voltage drop in a pair
************************************************************************
       DOUBLE PRECISION FUNCTION voltaje_vol(time,indice,modo,valor,
     &                                  etiqueta)

      use sine
      use cosine
      use potenciales_vol
      
      implicit none
      double precision    :: time,valor,pi,pot_vol
      integer, INTENT(IN) :: indice,modo
      integer             :: indice1,indice2
      character*255       :: etiqueta
      integer             :: i
      
      pi=dacos(-1.d0)
      
      if(modo.eq.1)then
        
       if (trim(etiqueta).eq.'sine') then
            voltaje_vol=amp_vol(indice)*dsin(2*pi*freq_vol(indice)
     &                                      *time)
       elseif (trim(etiqueta).eq.'cosine') then
            voltaje_vol=amp_vol_cos(indice)*dcos(2*pi*freq_vol_cos(indice)
     &                                      *time)     
            return
        else if(trim(etiqueta).eq.'User_Defined'.or. 
     &           trim(etiqueta).eq.'Example_1' .or. 
     &          trim(etiqueta).eq.'Example_2' ) then
            indice1=potencial_dat_vol%icouple(1,indice)
            indice2=potencial_dat_vol%icouple(2,indice)
            
            voltaje_vol= pot_vol(time,indice1,modo,valor,etiqueta)-
     &                   pot_vol(time,indice2,modo,valor,etiqueta)     
        
        end if
        
      else if (modo.eq.2) then
          
        voltaje_vol=valor
        
      end if
      
      return
      
      end function voltaje_vol
*     ************************************************************************
*    Functions V(t) surface - surface voltage drop in a pair
************************************************************************
       DOUBLE PRECISION FUNCTION voltaje_sur(time,indice,modo,valor,
     &                                  etiqueta)

      use sine
      use cosine
      use potenciales_sur
      use pwm
      
      implicit none
      double precision    :: time,valor,pi,pot_sur
      integer, INTENT(IN) :: indice,modo
      integer             :: indice1,indice2
      character*255       :: etiqueta
      integer             :: i
      
      pi=dacos(-1.d0)
      !lectura de datos PWM para Example_3
      call datapwm()
     
   
      if(modo.eq.1)then
        
        if (trim(etiqueta).eq.'sine') then
             voltaje_sur=amp_sur(indice)*dsin(2*pi*freq_sur(indice)
     &                                      *time)
    
            return
        elseif (trim(etiqueta).eq.'cosine') then
             voltaje_sur=amp_sur_cos(indice)*dcos(2*pi*freq_sur_cos(indice)
     &                                      *time)
            return        
         else if(trim(etiqueta).eq.'User_Defined' .or. 
     &           trim(etiqueta).eq.'Example_1' .or. 
     &            trim(etiqueta).eq.'Example_2' )then
            indice1=potencial_dat_sur%icouple(1,indice)
            indice2=potencial_dat_sur%icouple(2,indice)

            voltaje_sur= pot_sur(time,indice1,modo,valor,etiqueta)-
     &                   pot_sur(time,indice2,modo,valor,etiqueta)     
          elseif( trim(etiqueta).eq.'Example_3')then
            if(time.le.tk_sur(indice,1)) then
               voltaje_sur=-amp_pwm_sur(indice)
               return
            end if
            do i=1,nsaltos_sur(indice)-1
               if(time.gt.tk_sur(indice,i).and.
     &         time.le.tk_sur(indice,i+1))then 
                    voltaje_sur=(-1)**(i+1)*amp_pwm_sur(indice)
                    return
               endif
            enddo
            if(time.gt.tk_sur(indice,nsaltos_sur(indice))) then
               voltaje_sur=amp_pwm_sur(indice)
               return
            end if
     
        
        end if
        
      else if (modo.eq.2) then
          
        voltaje_sur=valor
        
      end if
      
      return
      
      end function voltaje_sur
      
      
      
 








