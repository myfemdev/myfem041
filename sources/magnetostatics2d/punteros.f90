      subroutine punteros
      
      use dirichlet
      use neumann
      use sourcesurface
      use malla_2DP1
        
! calculos para fuentes volumicas

	  call tabvol ()  
        
        
!calculos para bloqueo
     
      if (dirichlet_bc%numero.gt.0) then
     
     	call tabbloq(nel,mm,nra,dirichlet_bc%numero,dirichlet_bc%referencias,&
     &	             nvdi,nvd,nrvd,ndvdi,indblo)
        
      endif
     
!calculos para neumann    
     
      if (neumann_bc%numero.gt.0) then
     
     	call tabnesu(nel,mm,nra,z,neumann_bc%numero,nrane,neumann_bc%referencias,&
     &	             ntane,nvane,indblone,xlongne)
        
      endif
     
!calculos para fuentes superficiales
 
      if (sourcesur%numero.gt.0) then
      
     	call tabnesu(nel,mm,nra,z,sourcesur%numero,nrasu,sourcesur%referencias,&
     &	             ntasu,nvasu,indblosu,xlongsu)
     
      endif
    

      return
      end