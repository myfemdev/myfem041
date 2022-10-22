!-----------------------------------------------------------------
!            COMPUTATION OF THE MATRIX OF THE PROBLEM 
!-----------------------------------------------------------------

subroutine matriz3D_orto()

  use malla_3DP1
  use electros3D
  use external_electros3D
  use permitividad
 
  implicit none 
  integer                    :: ndcc,mm1,mm2,mm3,mm4,nsdk
  double precision           :: xbar,ybar,zbar,tbar
  double precision           :: coefk(3),amat(4,4),cmat(4,4)
  double precision, external :: evalta
  integer                    :: i,k

  ndcc = ib(nver+1)
  do i=1,ndcc
     camor(i)=0.d0
  enddo 
    
  do k=1,nel
     mm1=mm(1,k)
     mm2=mm(2,k)
     mm3=mm(3,k)
     mm4=mm(4,k)
     nsdk=nsd(k)
! COMPUTATION OF iopermir FOR THE ELEMENT 
     iopermir=0
     do i=1,permirel%numero
        if (nsdk.eq.permirel%referencias(i)) then
           iopermir=permirel%iopermir(i)
           ndom=i
        endif
     enddo
     if(iopermir.eq.0) then
             print*,'There is no permitivity assigned to the domain',nsdk 
        stop 
     endif
! BARICENTRIC FORMULA 
     xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)+ z(1,mm4)) / 4.D0
     ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)+ z(2,mm4)) / 4.D0
     zbar = (z(3,mm1) + z(3,mm2) + z(3,mm3)+ z(3,mm4)) / 4.D0
     if (iopermir.eq.1.or.iopermir.eq.2) then
        coefk(1) = permi(xbar,ybar,zbar,nsdk,1)
        coefk(2) = permi(xbar,ybar,zbar,nsdk,2)
        coefk(3) = permi(xbar,ybar,zbar,nsdk,3)
     elseif(iopermir.eq.3) then
        tbar=(teta(mm1)+teta(mm2)+teta(mm3)+teta(mm4))/4.d0
        coefk(1)=evalta(permirel%ntab(ndom),&
          permirel%teta(ndom,:),permirel%valtabx(ndom,:),tbar)
        coefk(2)=evalta(permirel%ntab(ndom),&
          permirel%teta(ndom,:),permirel%valtaby(ndom,:),tbar)
        coefk(3)=evalta(permirel%ntab(ndom),&
          permirel%teta(ndom,:),permirel%valtabz(ndom,:),tbar)
! FROM RELATIVE PERMITIVITY TO PERMITIVITY 
        coefk=coefk*8.854e-12
     endif 
     call matlap_orto(amat,det(k),binv(1,1,k),0.d0,coefk,cmat)
     call ensacmor(amat,ib,jb,mm(1,k),camor)
  enddo

  return
end
