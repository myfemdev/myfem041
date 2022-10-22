!---------------------------------------------------------------------------
!       ELEMENTAL MATRIX CONSTRUCTION
!---------------------------------------------------------------------------

subroutine matel(iop,n,nsdk,z,ab,bc,cd,de,det,condfun,a)	
     
  use conductividad, only: conduc	
  use dcurrent_2D,  only: teta

  implicit none
  integer, intent(in)           :: n(3)
  integer, intent(in)           :: nsdk,iop
  double precision, intent(in)  :: z(2,*)
  double precision, external    :: condfun
  double precision, intent(in)  :: ab,bc,cd,de,det
  double precision, intent(out) :: a(3,3)
  integer                       :: iopcond,ndom
  double precision              :: xbar,ybar,xint1,xint2,tbar,g11,g12,g22
  double precision              :: x12,y12,x23,y23,x31,y31,t12,t23,t31
  double precision, external    :: evalta
  integer                       :: i,j

  iopcond=0
  do i=1,conduc%numero
  	 if (nsdk.eq.conduc%referencias(i)) then	
        iopcond=conduc%iopcond(i)
        ndom=i
     endif
  enddo
  if(iopcond.eq.0) stop 'dominio sin asignar conductividad'

  if (iop.eq.1) then

     xbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))) / 3.d0
     ybar = (z(2,n(1)) + z(2,n(2)) + z(2,n(3))) / 3.d0
            
     if (iopcond.eq.1.or.iopcond.eq.2) then
        xint1 = condfun(xbar,ybar,nsdk,1)
        xint2 = condfun(xbar,ybar,nsdk,2)
     elseif(iopcond.eq.3) then
        tbar=(teta(n(1))+teta(n(2))+teta(n(3)))/3.d0
        xint1=evalta(conduc%ntab(ndom),conduc%teta(ndom,:),conduc%valtabx(ndom,:),tbar)
        xint2=evalta(conduc%ntab(ndom),conduc%teta(ndom,:),conduc%valtaby(ndom,:),tbar)
     endif 
 
  elseif (iop.eq.2) then

     if (iopcond.eq.1.or.iopcond.eq.2) then
        xint1 = ( condfun(z(1,n(1)),z(2,n(1)),nsdk,1) &
                + condfun(z(1,n(2)),z(2,n(2)),nsdk,1) &
                + condfun(z(1,n(3)),z(2,n(3)),nsdk,1) ) / 3.d0
        xint2 = ( condfun(z(1,n(1)),z(2,n(1)),nsdk,2) &
                + condfun(z(1,n(2)),z(2,n(2)),nsdk,2) &
                + condfun(z(1,n(3)),z(2,n(3)),nsdk,2) ) / 3.d0
     elseif(iopcond.eq.3) then
        xint1=(evalta(conduc%ntab(ndom), &
               conduc%teta(ndom,:),conduc%valtabx(ndom,:),teta(n(1)))+ &
               evalta(conduc%ntab(ndom),conduc%teta(ndom,:), &
               conduc%valtabx(ndom,:),teta(n(2)))+evalta(conduc%ntab(ndom), &
               conduc%teta(ndom,:),conduc%valtabx(ndom,:),teta(n(3))))/3.d0

        xint2=(evalta(conduc%ntab(ndom), &
               conduc%teta(ndom,:),conduc%valtaby(ndom,:),teta(n(1)))+ &
               evalta(conduc%ntab(ndom),conduc%teta(ndom,:), &
               conduc%valtaby(ndom,:),teta(n(2)))+evalta(conduc%ntab(ndom), &
               conduc%teta(ndom,:),conduc%valtaby(ndom,:),teta(n(3))))/3.d0
     endif 

  else 

     x12 = (z(1,n(1)) + z(1,n(2))) * 0.5d0
     y12 = (z(2,n(1)) + z(2,n(2))) * 0.5d0
     x23 = (z(1,n(2)) + z(1,n(3))) * 0.5d0
     y23 = (z(2,n(2)) + z(2,n(3))) * 0.5d0
     x31 = (z(1,n(3)) + z(1,n(1))) * 0.5d0
     y31 = (z(2,n(3)) + z(2,n(1))) * 0.5d0
     if (iopcond.eq.1.or.iopcond.eq.2) then
        xint1 = (condfun(x12,y12,nsdk,1) + condfun(x23,y23,nsdk,1) & 
               + condfun(x31,y31,nsdk,1)) /3.D0

        xint2 = (condfun(x12,y12,nsdk,2) + condfun(x23,y23,nsdk,2) &
               + condfun(x31,y31,nsdk,2)) /3.D0
     elseif(iopcond.eq.3) then
        t12=(teta(n(1))+teta(n(2)))/2.d0
        t23=(teta(n(3))+teta(n(2)))/2.d0
        t31=(teta(n(3))+teta(n(1)))/2.d0
        xint1=(evalta(conduc%ntab(ndom), &
              conduc%teta(ndom,:),conduc%valtabx(ndom,:),t12)+ &
              evalta(conduc%ntab(ndom),conduc%teta(ndom,:), &
              conduc%valtabx(ndom,:),t23)+evalta(conduc%ntab(ndom), &
              conduc%teta(ndom,:),conduc%valtabx(ndom,:),t31))/3.d0
        xint2=(evalta(conduc%ntab(ndom), &
              conduc%teta(ndom,:),conduc%valtaby(ndom,:),t12)+ &
              evalta(conduc%ntab(ndom),conduc%teta(ndom,:), &
              conduc%valtaby(ndom,:),t23)+evalta(conduc%ntab(ndom), &
              conduc%teta(ndom,:),conduc%valtaby(ndom,:),t31))/3.d0
     endif 
  end if
         
  g11=xint1*de**2+xint2*cd**2
  g12=-xint1*bc*de-xint2*ab*cd
  g22=xint1*bc**2+xint2*ab**2

  a(1,1) =  0.5d0 * (g11+2.*g12+g22) / det
  a(2,1) =  0.5d0 * (-g11-g12) / det
  a(2,2) =  0.5d0 * g11 / det
  a(3,1) =  0.5d0 * (-g12-g22) / det
  a(3,2) =  0.5d0 * g12 / det
  a(3,3) =  0.5d0 * g22 / det
  do i=1,2
     do j=i+1,3
        a(i,j) = a(j,i)
     end do
  end do
  
  return
end
