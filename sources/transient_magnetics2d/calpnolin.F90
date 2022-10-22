 !Subrutina de calculos previos necesarios para el nolineal
      subroutine calpnolin ()
 
      use nolineal
      use permeabilidad
      use derivados
 
      implicit none
      integer :: j, nmedmax,ierrort

      nmedmax=0     
      do j=1, ndnolin
         nmedmax=max(nmedmax,size(permagrel%hb(j)%h,1))
      end do
         
      allocate(rectas_dat%t(nmedmax+1,ndnolin),stat=ierrort)
      if (ierrort.ne.0) then 
         print*,'error: t cannot be allocated'
         stop
      endif
      
      allocate(rectas_dat%q(nmedmax+1,ndnolin),stat=ierrort)
      if (ierrort.ne.0) then 
         print*,'error: q cannot be allocated'
         stop
      endif 
        


      do j= 1,ndnolin
      
        omega(j)=sum(permagrel%hb(j)%h)/sum(permagrel%hb(j)%b)
        lambda(j)= 1.d0/(2.d0*omega(j))
          
 ! Calculo de las rectas para interpolar la tabla del material
 
        call rectas (permagrel%hb(j)%b,permagrel%hb(j)%h,rectas_dat%t(:,j),&
     &               rectas_dat%q(:,j),size(permagrel%hb(j)%h,1))
            
      end do
      
      return
      end subroutine calpnolin
   