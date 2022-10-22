!*********************************************************************
!     LECTURA DE DATOS                                                 
!*********************************************************************

      SUBROUTINE endat3D( )

      use fich_electros3D
      use parametros_electros3D
      use electros3D
      use cargavol
      use cargacur
      use cargapun
      use conductividad
      use bloqueo
      use derivados3D
      use auxiliar_cargas
      
      implicit none

!      character(len=funlen) :: tempfun
      integer :: temp, temp1, temp2, temp3, fnum, i, j
      
      ! inicializacion de variables (array)
      ! functions(1) == User defined / Function defined by user
      dir%fun = 1
      neu%fun = 1
      vol%fun = 1
      sup%fun = 1
      cur%fun = 1
 
        nrn = 0
        nri=0
        neuman%numero = 0 ! non imprescindible
        nrd = 0
        blofron%numero = 0 ! non imprescindible
        blopun%numero = 0 ! non imprescindible
        carvol%numero = 0 ! quitado
        carsup%numero = 0 ! quitado
        carcur%numero = 0 ! quitado
        ncarpun = 0 ! quitado ! non imprescindible
        conduc%numero = 0 ! non imprescindible
      
      print*,'Nombre del fichero de la malla electrica'
      read*,fichma
      print*,fichma
      print*,'Nombre del fichero de la solucion'
      read*,fichsol
      print*,fichsol
      print*,'Nombre del fichero de campo electrico'
      read*,fichElectricField
      print*,fichElectricField
      print*,'Nombre del fichero de la densidad de corriente'
      read*,fichCurrentDensity

      
      print*,'Opcion de bloqueo Dirichlet: 1:SI 0:NO'
      read*,iopblo
      opcion_bloqueo:  if (iopblo.eq.1) then
      	print*,'Opcion de entrada de bloqueo'
      	print*,'1 : Via function, 0:NO'
      	read*,iopblo1
      	print*,'Opcion de entrada de bloqueo'
      	print*,'1 : Via constantes por fronteras, 0:NO'
      	read*,iopblo2
      	print*,'Opcion de entrada de bloqueo'
      	print*,'1 : Via bloqueo puntual, 0:NO'
      	read*,iopblo3
      	
      	print*,'iopblo1,iopblo2,iopblo3',iopblo1,iopblo2,iopblo3

       entrada_por_funcion:	if (iopblo1.eq.1)	 then

        ! bucle para poder ponher varias condicions con varias funcions
        ! para sair hai que ponher 0 en nrd
        do while (.TRUE.)
        
            print*,'Numero de referencias Dirichlet por funcion'
            read*,temp
            
            if (temp <= 0) exit
            
            print*,'referencias Dirichlet'
            read*,(irefd(i),i=nrd+1,nrd+temp)
            print*,(irefd(i),i=nrd+1,nrd+temp)
            print*,'Type one of the function numbers below:'
            print*,1,': ',functions(1)
            read*, fnum
            dir%fun(nrd+1:nrd+temp) = fnum
            if (fnum<1 .or. fnum>2) stop 'incorrect function number'
            print*,fnum,functions(fnum)
            nrd = nrd + temp

        enddo
        


          endif entrada_por_funcion

       entrada_por_constantes:	if (iopblo2.eq.1)	 then

              print*,'Numero de referencias Dirichlet por constante'
              read*,blofron%numero
             print*,'blofron%numero',blofron%numero
             if(blofron%numero.gt.0) then
      		   do i=1,blofron%numero
      		print*,'Teclee numero de la frontera'
      		read*,blofron%referencias(i)
      		print*,'Teclee el valor de la solucion en dicha frontera'
      		read*,blofron%valor(i)
      		   enddo
             endif

      	endif entrada_por_constantes

       entrada_por_puntos:	if (iopblo3.eq.1)	 then

              print*,'Numero de  puntos bloqueo'
              read*,blopun%numero
             if(blopun%numero.gt.0) then
      		   do i=1,blopun%numero
      		print*,'Teclee numero del punto'
      		read*,blopun%referencias(i)
      		print*,'Teclee el valor de la solucion en dicho punto'
      		read*,blopun%valor(i)
      		   enddo
             endif
        endif	  entrada_por_puntos
        endif	  opcion_bloqueo
        print*,'blofron%numero=',blofron%numero
! empezar aqui a copiar nueva condicion        
      print*,'Opcion de referencias Neumann: 1:SI 0:NO'
      read*,iopneu
      opcion_neuman:  if (iopneu.eq.1) then
      	print*,'Opcion de entrada ref. Neumann'
      	print*,'1 : Via function, 0:NO'
      	read*,iopneu1
      	print*,'Opcion de entrada ref. Neumann'
      	print*,'1 : Via constantes por fronteras, 0:NO'
      	read*,iopneu2

       entrada_por_funcion_neuman:	if (iopneu1.eq.1)	 then

        ! bucle para poder ponher varias condicions con varias funcions
        ! para sair hai que ponher 0 en nrn
        do while (.TRUE.)
        
            print*,'Numero de referencias Neumann por funcion'
            read*,temp
            
            if (temp <= 0) exit
            
            print*,'referencias Neumann'
            read*,(irefn(i),i=nrn+1,nrn+temp)
            print*,(irefn(i),i=nrn+1,nrn+temp)
            print*,'Type one of the function numbers below:'
            print*,1,': ',functions(1)
            read*, fnum
            neu%fun(nrn+1:nrn+temp) = fnum
            if (fnum/=1 .and. fnum/=2) stop 'incorrect function number'
            print*,fnum,functions(fnum)
            nrn = nrn + temp
            
        enddo

       endif entrada_por_funcion_neuman

       entrada_por_constantes_neuman:	if (iopneu2.eq.1)	 then

              print*,'Numero de referencias Neumann por constante'
              read*,neuman%numero
             print*,'neuman%numero',neuman%numero
             if(neuman%numero.gt.0) then
      		   do i=1,neuman%numero
                print*,'Teclee numero de la frontera'
                read*,neuman%referencias(i)
                print*,'Teclee el valor de la der. normal&
     & en dicha frontera'
                read*,neuman%valor(i)
      		   enddo
             endif

      	endif entrada_por_constantes_neuman

        endif	  opcion_neuman
! acabar aqui de copiar nueva condicion        

!--------
    
      print*,'Opcion dato Intensidad de corriente: 1:SI 0:NO'
      read*,iopint
      opcion_intensidad:  if (iopint.eq.1) then
      	print*,'Opcion de entrada intensidad'
      	print*,'1 : Via function, 0:NO'
      	read*,iopint1
      	print*,'Opcion de entrada intensidad'
      	print*,'1 : Via constantes por fronteras, 0:NO'
      	read*,iopint2

       entrada_por_funcion_intensidad:	if (iopint1.eq.1)	 then

        ! bucle para poder ponher varias condicions con varias funcions
        ! para sair hai que ponher 0 en nri
        do while (.TRUE.)
        
            print*,'Numero de referencias para intensidad por funcion'
            read*,temp
            
            if (temp <= 0) exit
            
            print*,'referencias  intensidad de corriente'
            read*,(irefi(i),i=nri+1,nri+temp)
            print*,(irefi(i),i=nri+1,nri+temp)
            print*,'Type one of the function numbers below:'
            print*,1,': ',functions(1)
            read*, fnum
            intf%fun(nri+1:nri+temp) = fnum
            if (fnum<1 .or. fnum>2) stop 'incorrect function number'
            print*,fnum,functions(fnum)
            nri = nri + temp
            
        enddo

       endif entrada_por_funcion_intensidad

       entrada_por_constantes_intensidad:	if (iopint2.eq.1)	 then

         print*,'Numero de ref. donde se da la intensidad por constante'
              read*,inten%numero
             print*,'inten%numero',inten%numero
             if(inten%numero.gt.0) then
      		   do i=1,inten%numero
                print*,'Teclee numero de la frontera'
                read*,inten%referencias(i)
                print*,'Teclee el valor de la intensidad&
     &                  en dicha frontera'
                read*,inten%valor(i)
      		   enddo
             endif

      	endif entrada_por_constantes_intensidad

        endif	  opcion_intensidad
!---------





      print*,'Opcion formula de cuadratura matriz y segundo miembro'
      read*,iop
      print*,iop

      print*,'Opcion formula de cuadratura terminos frontera'
      read*,iopf
      print*,iopf
      
      
       print*,'Opcion de carga volumica: 1:SI 0:NO'
      read*,iopvol
      if (iopvol.eq.1) then
      	print*,'Opcion de entrada de carga volumica'
      	print*,'1: Via function ; 0: No'
        read*, temp1
      	print*,'1: Via tablero ; 0: No'
        read*, temp2
      	print*,'1: Via fichero ; 0: No'
        read*, temp3
      	
      	if (temp1.eq.1) then
      	  
        ! bucle para poder ponher varias fontes con varias funcions
        ! para sair hai que ponher 0 en temp
        do while (.TRUE.)
        
            print*,'Teclee numero de dominios con carga por funcion'
            read*,temp
            
            if (temp <= 0) exit
            
            print*,'Teclee numeros de referencia de los dominios'
            read*,(carvol%referencias(i),
     &          i=carvol%numero+1,carvol%numero+temp)
           	carvol%valor(carvol%numero+1:carvol%numero+temp)=0.d0 
           	carvol%constante(carvol%numero+1:carvol%numero+temp)=.FALSE.
            print*,'Type one of the function numbers below:'
            print*,1,': ',functions(1)
            read*, fnum
            vol%fun(carvol%numero+1:carvol%numero+temp) = fnum
            if (fnum/=1 .and. fnum/=2) stop 'incorrect function number'
            print*,fnum,functions(fnum)
            carvol%numero = carvol%numero + temp

        enddo

        endif
        
      	if (temp2.eq.1) then
      		   print*,'Teclee numero de cargas por constante'
      		   read*,temp
               if(temp.gt.0) then 
      		   do i=1,temp
      				print*,'Teclee numero de referencia del dominio'
      				read*,carvol%referencias(carvol%numero+i)
      				print*,'Teclee la carga asociada a ese dominio'
      				read*,carvol%valor(carvol%numero+i)
                    carvol%constante(carvol%numero+i) = .TRUE.
      		   enddo
               carvol%numero = carvol%numero + temp
               endif
      	end	if
      end if
      


      
      iopteta=0

      		print*,'Introduzca numero de subdominios distintos'
      		read*,conduc%numero
      		print*,'conduc%numero=',conduc%numero
      		do i=1,conduc%numero
      		 print*,'Teclee el numero del subdominino'
      		 read*,conduc%referencias(i)
     			 print*,'Teclee la opcion para conductividad electrica'
     			 print*,'1 --> Via funcion'
     			 print*,'2 --> Via constantes por dominio'
     			 print*,'3 --> Via tablero dependiente de la temperatura'
     			 read*,conduc%iopcond(i)
             if(conduc%iopcond(i).eq.1) then
                print*,'Teclee el numero de funcion de conductividad'
     &//' electrica'
             print*,'size(functions_cond,1)',size(functions_cond,1)
                do j=1,size(functions_cond,1)
                    print *, j,': ', functions_cond(j)
                enddo
                read*,conduc%fun(i)
                if (conduc%fun(i)<1 .or. conduc%fun(i)>
     &              size(functions_cond,1))
     &              stop 'incorrect function number'
             elseif(conduc%iopcond(i).eq.2) then
                print*,'Teclee la conductividad (x,y,z)'
                read*,conduc%valorx(i),conduc%valory(i),conduc%valorz(i)
             elseif(conduc%iopcond(i).eq.3)then
      		    iopteta=1
                read*,conduc%ntab(i)
                do j=1,conduc%ntab(i)
                read*,conduc%teta(i,j),conduc%valtabx(i,j),
     &          conduc%valtaby(i,j),conduc%valtabz(i,j)
                enddo
             else
                stop 'opcion de conductividad electrica incorrecta:&
     & solo 1 , 2 , 3'
      		 endif
      		enddo
c
c     ponemos iopteta a 1 para indicar que tenemos que leer 
c     temperatura una vez que leamos la malla

       if(iopteta.eq.1) then
      print*,'Nombre del fichero de la temperatura'
      read*,fichteta
      print*,fichteta
      endif

      print*,'opcion para resolucion del sistema lineal'
      print*,'1: Metodo directo, 0 : Gradiente conjugado'
      read*,iopsl
      
      if(iopsl.eq.0) then
      print*,'error de convergencia de CG'
      read* ,epscg
      print*,'error de convergencia de CG:',epscg

      print*,'numero de iteraciones maximo de CG'
      read*,nitcg
      print*,'iteraciones maximas de CG:',nitcg
      endif
     

!      Escritura por pantalla
     
      print*,'las opciones para la ejecucion del programa son:'
      print*,'iopblo=',iopblo
      if(iopblo.eq.1) then
      print*,'iopblo1=',iopblo1
      print*,'iopblo2=',iopblo2
      print*,'iopunblo3=',iopblo3
      endif
      print*,'iopneu=',iopneu
      if(iopneu.eq.1) then
      print*,'iopneu1=',iopneu1
      print*,'iopneu2=',iopneu2
      endif
      print*,'iopvol=',iopvol
!      if(iopvol.eq.1) then
!      print*,'iopinvol=',iopinvol
!      endif
      print*,'iopsup=',iopsup
!      if(iopsup.eq.1) then
!      print*,'iopinsup=',iopinsup
!      endif
      print*,'iopcur=',iopcur
!      if(iopcur.eq.1) then
!      print*,'iopincur=',iopincur
!      endif
      print*,'ioppun=',ioppun
      print*,'iop=',iop
      print*,'iopf=',iopf
      print*,'iopej=',iopej
      print*,'iopsl=',iopsl

      print*,'sali de endat'
      
! alojamiento de variables

      allocate(ncaras(nri+inten%numero),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para ncaras'
      stop 1
      endif      
      
      allocate(nodc1(nri+inten%numero,ndcaras), 
     &          nodc2(nri+inten%numero,ndcaras), 
     &          nodc3(nri+inten%numero,ndcaras),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nodc1,2 o 3'
      stop 1
      endif
      
      allocate(naristas(carcur%numero),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para naristas'
      stop 1
      endif
      
      allocate(nod1(carcur%numero,ndar), 
     &         nod2(carcur%numero,ndar),stat=ierror)
      if (ierror.ne.0) then
      print*,'error: no se ha podido reservar memoria para nod1 o nod2'
      stop 1
      endif

 
  
      RETURN

      END
