      subroutine resolvente(gamma,v,w,j)
      
      use derivados
  
      implicit none
      integer, intent(in)          :: j
      integer                      :: i, mnolin
      double precision, intent(in) ::  v(2), gamma
      double precision, intent(out)::  w(2)
      double precision             ::  xw, xv

      xv=dsqrt(v(1)*v(1)+v(2)*v(2))
      if(xv.lt.1.e-7) then
         w(1)=0.d0
         w(2)=0.d0
         return
      endif

      xw=(xv-gamma*rectas_dat%q(1,j))/(1.d0+gamma*rectas_dat%t(1,j))
      if (xw.le.permagrel%hb(j)%h(1)) then
        go to 1
      end if
      
      mnolin=size(permagrel%hb(j)%h,1)

      do i=2,mnolin
         xw=(xv-gamma*rectas_dat%q(i,j))/(1.d0+gamma*rectas_dat%t(i,j))
         if (xw.ge.permagrel%hb(j)%h(i-1).and.xw.le.permagrel%hb(j)%h(i)) then
           go to 1
         end if
      end do


      xw=(xv-gamma*rectas_dat%q(mnolin+1,j))/(1.d0+gamma*rectas_dat%t(mnolin+1,j))
      
      if (xw.lt.permagrel%hb(j)%h(mnolin)) then
         print*,'error in resolvente'
         stop 1
      end if

        
1     w(1)=xw*v(1)/xv
      w(2)=xw*v(2)/xv

      return

      end 

