!
        subroutine arint3D ()

! CALCULA:
! número de aristas asociadas a la referencia de carga curvilinea y sus correspondientes nodos. 
! (esto en corriente continua no se utiliza)
! número de caras asociadas a la referencia de carga superficial y sus correspondientes nodos.
! (esto en corriente continua no se utiliza)
! 
! Esto es lo que realmente se utiliza
! área de las fronteras donde se da la intensidad de corriente como dato.
 
! IN:
! OUT: naristas , nod1, nod2

        !nelem,ensd: novos arrays     
        !nsd: necesario
        use  malla_3DP1             ,  only :
     &      nel, mm, nra, nrc,z,
     &      nelem, ensd, nsd
        use  cargacur 	            
        use  cargasup
        use  parametros_electros3D	,  only : ndar
        use  electros3D            
        use  auxiliar_cargas
        use  derivados3D
        
        implicit double precision (a-h,o-z)
        dimension vnu(3)


!!!!!!!! inicio: precalcular lista de elementos que corresponden con cada indice de carga volumica
!!!!!!!! utilizado no principio de semi3D.f
        
        allocate(nelem(carvol%numero),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para nelem'
            stop
        endif
        allocate(ensd(carvol%numero,nel),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para ensd'
            stop
        endif
        
        nelem = 0; ensd = 0
        
        if (carvol%numero > 0) then
            do k = 1, nel
                do i = 1, carvol%numero
                    if (carvol%referencias(i) == nsd(k)) then
                        ensd(i,nelem(i)+1) = k
                        nelem(i) = nelem(i) + 1
                        exit
                    endif
                enddo
            enddo
        endif  

!!!!!!!! fin: precalcular lista de elementos que corresponden con cada indice de carga volumica


        if (carcur%numero.gt.0) then

      	nod1(1,1)=0
      	nod2(1,1)=0

      	donumcar: do i= 1,carcur%numero


      		naristas(i)   = 0
     

      		do 1 k=1,nel

      		doaristas:	do  j=1,6

      				nref=nra(j,k)
      				nov1=mm(inda(1,j),k)
      				nov2=mm(inda(2,j),k)
      				if (nref.eq.carcur%referencias(i)) then
      					
      					do 3 ii=1, naristas(i)

      			if ((nod1(i,ii).eq.nov1.and.nod2(i,ii).eq.nov2)
     &			   .or.(nod1(i,ii).eq.nov2.and.nod2(i,ii).eq.nov1)) then

!     			  Esto significa que la arista ya esta contabilizada

      					cycle doaristas
      			endif
3     					continue

!     			 Arista todavia no contabilizada

      			 naristas(i) = naristas(i) + 1
      			 nod1(i,naristas(i)) = nov1
      		     nod2(i,naristas(i)) = nov2
                 
      				endif

     			enddo doaristas
 1    		continue
       if(naristas(i).eq.0) then
       print*,'ATENCION: no se ha encontrado ninguna arista de        
     &         referencia carcur%referencia'
       endif
      	enddo donumcar
        
        endif	
      
 ! Tableros asociados a las cargas superficiales nodc1, nodc2, nodc3, ncaras
    
        if (carsup%numero.gt.0) then

      	nodc1(1,1)=0
      	nodc2(1,1)=0
      	nodc3(1,1)=0

      	donumcarsup: do i= 1,carsup%numero


      		ncaras(i)   = 0
     

      		do 11 k=1,nel

      		docaras: do  j=1,4

      				nref=nrc(j,k)
      				nov1=mm(indc(1,j),k)
      				nov2=mm(indc(2,j),k)
      				nov3=mm(indc(3,j),k)
      				
      				if (nref.eq.carsup%referencias(i)) then
      					
      					do 33 ii=1, ncaras(i)

                           if ( (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov2).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov2) ) then

!     			  Esto significa que la cara ya esta contabilizada

      					  cycle docaras
      			          endif
33     					continue

!     			Cara todavia no contabilizada

      			 ncaras(i) = ncaras(i) + 1
      			 nodc1(i,ncaras(i)) = nov1
      		     nodc2(i,ncaras(i)) = nov2
      		     nodc3(i,ncaras(i)) = nov3
                 
      				endif

     			enddo docaras
 11    		continue
 
         if(ncaras(i).eq.0) then
           print*,'ATENCION: no se ha encontrado ninguna cara de        
     &         referencia carsup%referencia'
         endif
      enddo donumcarsup
        
        endif
        
 !---------
 
 
 ! Tableros asociados al dato intensidad de corriente 
    
        if (inten%numero.gt.0) then
        
        
      	nodc1(1,1)=0
      	nodc2(1,1)=0
      	nodc3(1,1)=0

        
      	donumcarasinten: do i= 1,inten%numero
            inten%area(i)=0.d0
      		ncaras(i)   = 0
     

      		do 111 k=1,nel

      		docaras1: do  j=1,4

      				nref=nrc(j,k)
      				nov1=mm(indc(1,j),k)
      				nov2=mm(indc(2,j),k)
      				nov3=mm(indc(3,j),k)
      				
      				if (nref.eq.inten%referencias(i)) then
      					
      					do 333 ii=1, ncaras(i)

                           if ( (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov2).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov2) ) then

!     			  Esto significa que la cara ya esta contabilizada

      					  cycle docaras1
      			          endif
333     					continue

!     			Cara todavia no contabilizada

      			 ncaras(i) = ncaras(i) + 1
      			 nodc1(i,ncaras(i)) = nov1
      		     nodc2(i,ncaras(i)) = nov2
      		     nodc3(i,ncaras(i)) = nov3
      		     
      		     call calvnoru(z(1,nov1),z(1,nov2),z(1,nov3),vnu,dnor)
                 inten%area(i)=inten%area(i)+dnor/2.d0
                 
      				endif

     			enddo docaras1
!     			print*,'area=',inten%area(i),inten%referencias(i)
 111    		continue
 
         if(ncaras(i).eq.0) then
           print*,'ATENCION: no se ha encontrado ninguna cara de        
     &         referencia inten%referencia'
         endif
      enddo donumcarasinten
        
        endif   
        
        
        
        
        
        
       if (nri.gt.0) then
        print*,'hola1'
      	nodc1(1,1)=0
      	nodc2(1,1)=0
      	nodc3(1,1)=0
        print*,'hola2'
        print*,'nri',nri
        
      	donumcarasinten2: do i= 1,nri
            areafi(i)=0.d0
      		ncaras(i)   = 0
     

      		do 12 k=1,nel

      		docaras2: do  j=1,4

      				nref=nrc(j,k)
      				nov1=mm(indc(1,j),k)
      				nov2=mm(indc(2,j),k)
      				nov3=mm(indc(3,j),k)
      				
      				if (nref.eq.irefi(i)) then
      					
      					do 34 ii=1, ncaras(i)

                           if ( (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov2).or.
     &			              (nodc1(i,ii).eq.nov3.and.
     &                         nodc2(i,ii).eq.nov2.and.
     &                         nodc3(i,ii).eq.nov1).or.
     &			              (nodc1(i,ii).eq.nov2.and.
     &                         nodc2(i,ii).eq.nov1.and.
     &                         nodc3(i,ii).eq.nov3).or.
     &			              (nodc1(i,ii).eq.nov1.and.
     &                         nodc2(i,ii).eq.nov3.and.
     &                         nodc3(i,ii).eq.nov2) ) then

!     			  Esto significa que la cara ya esta contabilizada

      					  cycle docaras2
      			          endif
34     					continue

!     			Cara todavia no contabilizada

      			 ncaras(i) = ncaras(i) + 1
      			 nodc1(i,ncaras(i)) = nov1
      		     nodc2(i,ncaras(i)) = nov2
      		     nodc3(i,ncaras(i)) = nov3
      		     
      		     call calvnoru(z(1,nov1),z(1,nov2),z(1,nov3),vnu,dnor)
                 areafi(i)=areafi(i)+dnor/2.d0
                 
      				endif

     			enddo docaras2
     			
 12    		continue
           print*,'area=',areafi(i),i
 
         if(ncaras(i).eq.0) then
           print*,'ATENCION: no se ha encontrado ninguna cara de        
     &         referencia irefi'
         endif
      enddo donumcarasinten2
      
      
      endif    
        
       
      end
      
      
      
               