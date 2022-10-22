subroutine matel(iop,n,nsdk,z,ab,bc,cd,de,det,permi,a)
     
  use permitividad, only: permirel
  use electros_2D,  only: teta

  implicit none
  integer,          intent(in)  :: iop
  integer,          intent(in)  :: n(3)
  double precision, intent(in)  :: z(2,*)
  integer,          intent(in)  :: nsdk
  double precision, intent(in)  :: ab,bc,cd,de,det
  double precision, external    :: permi
  double precision, intent(out) :: a(3,3)
  integer                       :: iopermir,ndom
  double precision              :: xbar,ybar,xint1,xint2,tbar
  double precision              :: x12,x23,x31,y12,y23,y31,t12,t23,t31
  double precision              :: g11,g12,g22
  double precision,external     :: evalta
  integer                       :: i,j

  iopermir=0
  do i=1,permirel%numero
     if(nsdk.eq.permirel%referencias(i)) then
        iopermir=permirel%iopermir(i)
        ndom=i
     endif
  enddo
  if(iopermir.eq.0) stop 'there is no permitivity assigned to the domain'

  if (iop .eq. 1) then
     xbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))) / 3.D0
     ybar = (z(2,n(1)) + z(2,n(2)) + z(2,n(3))) / 3.D0
     if(iopermir.eq.1.or.iopermir.eq.2) then
        xint1 = permi(xbar,ybar,nsdk,1)
        xint2 = permi(xbar,ybar,nsdk,2)
     elseif(iopermir.eq.3) then
        tbar=(teta(n(1))+teta(n(2))+teta(n(3)))/3.d0
        xint1=evalta(permirel%ntab(ndom),&
          permirel%teta(ndom,:),permirel%valtabx(ndom,:),tbar)
        xint2=evalta(permirel%ntab(ndom),&
          permirel%teta(ndom,:),permirel%valtaby(ndom,:),tbar)
! FROM RELATIVE PERMITIVITY TO PERMITIVITY
        xint1=xint1*8.854e-12
        xint2=xint2*8.854e-12
     endif 
  else if (iop .eq. 2) then
     if(iopermir.eq.1.or.iopermir.eq.2) then
        xint1 = ( permi(z(1,n(1)),z(2,n(1)),nsdk,1)& 
          + permi(z(1,n(2)),z(2,n(2)),nsdk,1)& 
          + permi(z(1,n(3)),z(2,n(3)),nsdk,1) ) / 3.D0
        xint2 = ( permi(z(1,n(1)),z(2,n(1)),nsdk,2)& 
          + permi(z(1,n(2)),z(2,n(2)),nsdk,2)& 
          + permi(z(1,n(3)),z(2,n(3)),nsdk,2) ) / 3.D0
     elseif(iopermir.eq.3) then
        xint1=(evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtabx(ndom,:),teta(n(1)))+&
         evalta(permirel%ntab(ndom),permirel%teta(ndom,:),&
         permirel%valtabx(ndom,:),teta(n(2)))+evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtabx(ndom,:),teta(n(3))))/3.d0
        xint2=(evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtaby(ndom,:),teta(n(1)))+&
         evalta(permirel%ntab(ndom),permirel%teta(ndom,:),&
         permirel%valtaby(ndom,:),teta(n(2)))+evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtaby(ndom,:),teta(n(3))))/3.d0
! FROM RELATIVE PERMITIVITY TO PERMITIVITY
        xint1=xint1*8.854e-12
        xint2=xint2*8.854e-12
     endif 
  else 
     x12 = (z(1,n(1)) + z(1,n(2))) * 0.5D0
     y12 = (z(2,n(1)) + z(2,n(2))) * 0.5D0
     x23 = (z(1,n(2)) + z(1,n(3))) * 0.5D0
     y23 = (z(2,n(2)) + z(2,n(3))) * 0.5D0
     x31 = (z(1,n(3)) + z(1,n(1))) * 0.5D0
     y31 = (z(2,n(3)) + z(2,n(1))) * 0.5D0
     if(iopermir.eq.1.or.iopermir.eq.2) then
        xint1 = (permi(x12,y12,nsdk,1) + permi(x23,y23,nsdk,1)& 
          + permi(x31,y31,nsdk,1)) /3.D0
        xint2 = (permi(x12,y12,nsdk,2) + permi(x23,y23,nsdk,2)& 
          + permi(x31,y31,nsdk,2)) /3.D0
     elseif(iopermir.eq.3) then
        t12=(teta(n(1))+teta(n(2)))/2.d0
        t23=(teta(n(3))+teta(n(2)))/2.d0
        t31=(teta(n(3))+teta(n(1)))/2.d0
        xint1=(evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtabx(ndom,:),t12)+&
         evalta(permirel%ntab(ndom),permirel%teta(ndom,:),&
         permirel%valtabx(ndom,:),t23)+evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtabx(ndom,:),t31))/3.d0
        xint2=(evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtaby(ndom,:),t12)+&
         evalta(permirel%ntab(ndom),permirel%teta(ndom,:),&
         permirel%valtaby(ndom,:),t23)+evalta(permirel%ntab(ndom),&
         permirel%teta(ndom,:),permirel%valtaby(ndom,:),t31))/3.d0
! FROM RELATIVE PERMITIVITY TO PERMITIVITY
        xint1=xint1*8.854e-12
        xint2=xint2*8.854e-12
     endif 
  end if
         
  g11=xint1*de**2+xint2*cd**2
  g12=-xint1*bc*de-xint2*ab*cd
  g22=xint1*bc**2+xint2*ab**2

! COMPUTATION FO THE 3 x 3 MATRIX
  a(1,1) =  0.5D0 * (g11+2.*g12+g22) / det
  a(2,1) =  0.5D0 * (-g11-g12) / det
  a(2,2) =  0.5D0 * g11 / det
  a(3,1) =  0.5D0 * (-g12-g22) / det
  a(3,2) =  0.5D0 * g12 / det
  a(3,3) =  0.5D0 * g22 / det
  do i=1,2
     do j=i+1,3
        a(i,j) = a(j,i)
     enddo
  enddo

  return
end
