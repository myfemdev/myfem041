!------------------------------------------------------------------
! GOAL : computation of the current density (sigma*E)                    
!------------------------------------------------------------------

subroutine cd()
                   
  use malla_2DP1
  use dcurrent_2D
  use conductividad
      
  implicit none
  integer                    :: n(3)
  integer                    :: nsdk,iopcond,ndom
  double precision           :: xbar,ybar,tbar
  double precision, external :: evalta
  integer                    :: k,i,istat
  
  if(allocated(jc))deallocate(jc)
  allocate(jc(2,nel),stat=istat)
  if (istat.ne.0) stop 'Error al alojar e en ef'
      
  do k=1,nel
     jc(1,k)=0d0
     jc(2,k)=0d0 
  end do

  do k=1,nel
      
     n(1)=mm(1,k)
     n(2)=mm(2,k)
     n(3)=mm(3,k)
      
     xbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))) / 3.D0
     ybar = (z(2,n(1)) + z(2,n(2)) + z(2,n(3))) / 3.D0
      
     nsdk=nsd(k)
     iopcond=0
 	 do i=1,conduc%numero
     	if (nsdk.eq.conduc%referencias(i)) then	
           iopcond=conduc%iopcond(i)
           ndom=i
      	endif
     enddo
      
     if (iopcond.eq.1.or.iopcond.eq.2) then
        jc(1,k) = condfun(xbar,ybar,nsdk,1)*e(1,k)
        jc(2,k) = condfun(xbar,ybar,nsdk,2)*e(2,k)
     elseif(iopcond.eq.3) then
        tbar=(teta(n(1))+teta(n(2))+teta(n(3)))/3.d0
        jc(1,k)=evalta(conduc%ntab(ndom),conduc%teta(ndom,:),conduc%valtabx(ndom,:),tbar)*e(1,k)
        jc(2,k)=evalta(conduc%ntab(ndom),conduc%teta(ndom,:),conduc%valtaby(ndom,:),tbar)*e(2,k)
     endif 
        
  end do

  return
end
