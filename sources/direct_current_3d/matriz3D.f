*****************************************************************
*           CALCULO DE LA MATRIZ DEL PROBLEMA TERMICO           *
*****************************************************************

      SUBROUTINE matriz3D()

      use malla_3DP1
      use electros3D
      use external_electros3D
      use conductividad
 
      implicit double precision (a-h,o-z)

!        Variables internas
         dimension amat(4,4),cmat(4,4)

!        Inicializacion del vector c
         ndcc = ib(nver+1)
         do 11 i=1,ndcc
            camor(i)=0.d0
   11    continue 
    
! bucle en elementos en el que calculamos la matriz elemental y el
! segundo miembro elemental y los ensamblamos
!       print*,'iop en matriz3D',iop
      do 1 k=1,nel
         mm1=mm(1,k)
         mm2=mm(2,k)
         mm3=mm(3,k)
         mm4=mm(4,k)
         nsdk=nsd(k)
c      calculo iopconductividad para este elemento
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
!calculo coef2 segun las correspondientes formulas de cuadratura

        IF (iop .EQ. 1) THEN

C           evaluamos en el baricentro

            xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)+ z(1,mm4)) / 4.D0
            ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)+ z(2,mm4)) / 4.D0
            zbar = (z(3,mm1) + z(3,mm2) + z(3,mm3)+ z(3,mm4)) / 4.D0
            
c  MODIFICADO MC 25-11-2009 .....COMIENZO
c   ojo habra que cambiar coef2 para ortotropia
            
             if(iopconductividad.eq.1) then ! por funcion
                coef2 = condfun(xbar,ybar,zbar,nsdk,1)
                coef2 = condfun(xbar,ybar,zbar,nsdk,2)
                coef2 = condfun(xbar,ybar,zbar,nsdk,3)
             elseif(iopconductividad.eq.2) then ! por constante
                coef2 = cond(xbar,ybar,zbar,nsdk,1)
                coef2 = cond(xbar,ybar,zbar,nsdk,2)
                coef2 = cond(xbar,ybar,zbar,nsdk,3)
             elseif(iopconductividad.eq.3) then ! por tablero
             tbar=(teta(mm1)+teta(mm2)+teta(mm3)+teta(mm4))/4.d0
             coef2=evalta(conduc%ntab(ndom),
     &        conduc%teta(ndom,:),conduc%valtabx(ndom,:),tbar)
             coef2=evalta(conduc%ntab(ndom),
     &        conduc%teta(ndom,:),conduc%valtaby(ndom,:),tbar)
             coef2=evalta(conduc%ntab(ndom),
     &        conduc%teta(ndom,:),conduc%valtabz(ndom,:),tbar)
             endif 
             

          ELSE IF (iop .EQ. 2) THEN

C           media en los vertices

            if(iopconductividad.eq.1) then ! por funcion
            coef2 = ( condfun(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,1) 
     &             + condfun(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,1) 
     &             + condfun(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,1)
     &             + condfun(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,1) ) / 4.D0
            coef2 = ( condfun(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,2) 
     &             + condfun(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,2) 
     &             + condfun(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,2)
     &             + condfun(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,2) ) / 4.D0
           coef2 = ( condfun(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,3) 
     &             + condfun(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,3) 
     &             + condfun(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,3)
     &             + condfun(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,3) ) / 4.D0
            elseif(iopconductividad.eq.2) then ! por constante
            coef2 = ( cond(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,1) 
     &             + cond(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,1) 
     &             + cond(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,1)
     &             + cond(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,1) ) / 4.D0
            coef2 = ( cond(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,2) 
     &             + cond(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,2) 
     &             + cond(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,2)
     &             + cond(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,2) ) / 4.D0
           coef2 = ( cond(z(1,mm1),z(2,mm1),z(3,mm1),nsdk,3) 
     &             + cond(z(1,mm2),z(2,mm2),z(3,mm2),nsdk,3) 
     &             + cond(z(1,mm3),z(2,mm3),z(3,mm3),nsdk,3)
     &             + cond(z(1,mm4),z(2,mm4),z(3,mm4),nsdk,3) ) / 4.D0
              elseif(iopconductividad.eq.3) then ! por tablero
             coef2=(evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabx(ndom,:),teta(mm1))+
     &evalta(conduc%ntab(ndom),conduc%teta(ndom,:),
     &conduc%valtabx(ndom,:),teta(mm2))+evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabx(ndom,:),teta(mm3))+
     &evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabx(ndom,:),teta(mm4)))/4.d0

             coef2=(evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtaby(ndom,:),teta(mm1))+
     &evalta(conduc%ntab(ndom),conduc%teta(ndom,:),
     &conduc%valtaby(ndom,:),teta(mm2))+evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtaby(ndom,:),teta(mm3))
     &+evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtaby(ndom,:),teta(mm4)))/4.d0
     
           coef2=(evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabz(ndom,:),teta(mm1))+
     &evalta(conduc%ntab(ndom),conduc%teta(ndom,:),
     &conduc%valtabz(ndom,:),teta(mm2))+evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabz(ndom,:),teta(mm3))
     &+evalta(conduc%ntab(ndom),
     &conduc%teta(ndom,:),conduc%valtabz(ndom,:),teta(mm4)))/4.d0
             endif 
  
 

         END IF

        call matlap(amat,det(k),binv(1,1,k),0.d0,coef2,cmat)
        call ensacmor(amat,ib,jb,mm(1,k),camor)
        


 1    continue



      return
      
      end
