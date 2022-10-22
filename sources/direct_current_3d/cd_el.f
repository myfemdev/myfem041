      subroutine cd()

******************************************************************
* objetivo : calcula la densidad de corriente                    *
******************************************************************
*                                                                *
*   IN:        modulos                                           *
*                                                                *
*                                                                *
*   OUT:                                                         *
*              cd: densidad de corriente= sigma*E                *
******************************************************************
*                   
      use malla_3DP1
      use electros3D
      use conductividad
      
      
      implicit double precision (a-h,o-z)
      DIMENSION n(4)

* Aloxamos as variables auxiliares
      allocate(jc(3,nel),STAT=istat)
      if (istat.ne.0) stop 'Error al alojar jc en ef'
      
      do 1 k=1,nel
        jc(1,k)=0d0
        jc(2,k)=0d0 
        jc(3,k)=0d0
1     continue

      do 2 k=1,nel
      
         n(1)=mm(1,k)
         n(2)=mm(2,k)
         n(3)=mm(3,k)
         n(4)=mm(4,k)
      xbar = (z(1,n(1)) + z(1,n(2)) + z(1,n(3))+ z(1,n(4))) / 4.D0
      ybar = (z(2,n(1)) + z(2,n(2)) + z(2,n(3))+ z(2,n(4))) / 4.D0
      zbar = (z(3,n(1)) + z(3,n(2)) + z(3,n(3))+ z(3,n(4))) / 4.D0
      
      nsdk=nsd(k)
      iopconductividad=0
 	    do i=1,conduc%numero
      			if(nsdk.eq.conduc%referencias(i)) then	
                iopconductividad=conduc%iopcond(i)
                ndom=i
      			endif
          enddo
          
        if(iopconductividad.eq.0) then
      	    print*,'domain',nsdk,' has not been assigned '
     &//'electrical conductivity'
      	    stop 'unassigned domain electrical conductivity'
        endif
      
      if(iopconductividad.eq.1) then  !por funcion
         jc(1,k) = condfun(xbar,ybar,zbar,nsdk,1)*e(1,k)
         jc(2,k) = condfun(xbar,ybar,zbar,nsdk,2)*e(2,k)
         jc(3,k) = condfun(xbar,ybar,zbar,nsdk,3)*e(3,k)      
      elseif(iopconductividad.eq.2) then ! por constante
          jc(1,k) = cond(xbar,ybar,zbar,nsdk,1)*e(1,k)
          jc(2,k) = cond(xbar,ybar,zbar,nsdk,2)*e(2,k)
          jc(3,k) = cond(xbar,ybar,zbar,nsdk,3)*e(3,k)
      elseif(iopconductividad.eq.3) then !por tablero
          tbar=(teta(n(1))+teta(n(2))+teta(n(3))+teta(n(4)))/4.d0
          jc(1,k)=evalta(conduc%ntab(ndom),
     &    conduc%teta(ndom,:),conduc%valtabx(ndom,:),tbar)*e(1,k)
          jc(2,k)=evalta(conduc%ntab(ndom),
     &    conduc%teta(ndom,:),conduc%valtaby(ndom,:),tbar)*e(2,k)
           jc(3,k)=evalta(conduc%ntab(ndom),
     &    conduc%teta(ndom,:),conduc%valtaby(ndom,:),tbar)*e(3,k)
      endif 
        
2     continue


      return
      end
