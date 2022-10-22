subroutine intensidades()
use intensidad
use tiempo
use derivados
use potenciales_vol
use potenciales_sur
use electros_2D

implicit none

integer :: i,modo,k,j,aux
character*255 etiqueta 
double precision :: valor, time, pot_vol, pot_sur,xintreal,voltaje_vol,voltaje_sur

 time=ipat*deltat+ti

 if (potencial_dat_vol%numero.gt.0)then 
    
    call integra_sa(sol)
    
    if (potencial_dat_vol%ncouples.gt.0)then
    
        do i=1,potencial_dat_vol%ncouples
        
            modo=potencial_dat_vol%modo4(i)
            valor=potencial_dat_vol%valor4(i)
            etiqueta=potencial_dat_vol%etiqueta4(i)
            
            xints(potencial_dat_vol%icouple(1,i),ipat)=(-voltaje_vol(time,i,modo,valor,etiqueta)&
         & -(xintsavol(potencial_dat_vol%icouple(1,i))-xintsavol0(potencial_dat_vol%icouple(1,i)))/deltat*(1.d0/xintsvol(potencial_dat_vol%icouple(1,i)))&
         & +(xintsavol(potencial_dat_vol%icouple(2,i))-xintsavol0(potencial_dat_vol%icouple(2,i)))/deltat*(1.d0/xintsvol(potencial_dat_vol%icouple(2,i))))&
         & *(1.d0/(1.d0/xintsvol(potencial_dat_vol%icouple(1,i))+1.d0/xintsvol(potencial_dat_vol%icouple(2,i))))
         
            xints(potencial_dat_vol%icouple(2,i),ipat)=-xints(potencial_dat_vol%icouple(1,i),ipat)
        
        end do
        
        do i=1,potencial_dat_vol%numero
            do j=1,potencial_dat_vol%ncouples
                if(i.eq.potencial_dat_vol%icouple(1,j).or.i.eq.potencial_dat_vol%icouple(2,j)) go to 1
            end do
            xints(i,ipat)=-pot_vol(time,i,modo,valor,etiqueta)*xintsvol(i)&
                           & -(xintsavol(i)-xintsavol0(i))/deltat
 1          continue
        end do            
            
    else
        
        do i=1,potencial_dat_vol%numero
          modo=potencial_dat_vol%modo2(i)
          valor=potencial_dat_vol%valor2(i)
          etiqueta=potencial_dat_vol%etiqueta2(i)
         
          xints(i,ipat)=-pot_vol(time,i,modo,valor,etiqueta)*xintsvol(i)&
          & -(xintsavol(i)-xintsavol0(i))/deltat
   
        end do
        
    end if
    
 end if
     
 if (potencial_dat_sur%numero.gt.0)then 
 
    call integra_sa_su(sol)
    
    if (potencial_dat_sur%ncouples.gt.0)then
    
        do i=1,potencial_dat_sur%ncouples
        
            modo=potencial_dat_sur%modo4(i)
            valor=potencial_dat_sur%valor4(i)
            etiqueta=potencial_dat_sur%etiqueta4(i)
            
            xints(potencial_dat_sur%icouple(1,i)+potencial_dat_vol%numero,ipat)=(-voltaje_sur(time,i,modo,valor,etiqueta)&
         & -(xintsasur(potencial_dat_sur%icouple(1,i))-xintsasur0(potencial_dat_sur%icouple(1,i)))/deltat*(1.d0/xintssur(potencial_dat_sur%icouple(1,i)))&
         & +(xintsasur(potencial_dat_sur%icouple(2,i))-xintsasur0(potencial_dat_sur%icouple(2,i)))/deltat*(1.d0/xintssur(potencial_dat_sur%icouple(2,i))))&
         & *(1.d0/(1.d0/xintssur(potencial_dat_sur%icouple(1,i))+1.d0/xintssur(potencial_dat_sur%icouple(2,i))))
         
            xints(potencial_dat_sur%icouple(2,i)+potencial_dat_vol%numero,ipat)=-xints(potencial_dat_sur%icouple(1,i)+potencial_dat_vol%numero,ipat)
            
!            write(37,*) time
!            write(38,*) voltaje_sur(time,i,modo,valor,etiqueta)
            print*,1,i,-voltaje_sur(time,i,modo,valor,etiqueta)
        end do
        
        do i=1,potencial_dat_sur%numero
            do j=1,potencial_dat_sur%ncouples
                if(i.eq.potencial_dat_sur%icouple(1,j).or.i.eq.potencial_dat_sur%icouple(2,j)) go to 2
            end do
            xints(i+potencial_dat_vol%numero,ipat)=-pot_sur(time,i,modo,valor,etiqueta)*xintssur(i)&
                & -(xintsasur(i)-xintsasur0(i))/deltat
            print*,2,i,xints(i+potencial_dat_vol%numero,ipat)
 2          continue
        end do                        
            
    else
        
        do i=1,potencial_dat_sur%numero
            modo=potencial_dat_sur%modo2(i)
            valor=potencial_dat_sur%valor2(i)
            etiqueta=potencial_dat_sur%etiqueta2(i)
         
            xints(i+potencial_dat_vol%numero,ipat)=-pot_sur(time,i,modo,valor,etiqueta)*xintssur(i)&
            & -(xintsasur(i)-xintsasur0(i))/deltat
            print*,3,i, xints(i+potencial_dat_vol%numero,ipat)
        end do
    
    end if
 
 end if
 print*, potencial_dat_vol%numero,potencial_dat_sur%numero
 print*, 'intensities:', xints(1:potencial_dat_vol%numero+potencial_dat_sur%numero,ipat)
 print*,'----------------------------------------------------------'
! print*, 'exact intensity:', xintreal(time)
 
return
end
