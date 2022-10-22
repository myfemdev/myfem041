      subroutine punteros
      
      use dirichlet
      use neumann
      use malla_2DP1
      use derivados
      use voltage_drop
      use intensity_input
      use electros_2D
      implicit double precision(a-h,o-z)
      
      imaxnsd =maxval(permagrel%referencias(:))
        
      
      allocate(domains%jsdata(imaxnsd),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para domains%jsdata'
            stop 1
        endif
        
        domains%jsdata = .false.

		allocate(domains%vdata(imaxnsd),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para domains%vdata'
            stop 1
        endif
        
        domains%vdata = .false.
        
        
        allocate(domains%idata(imaxnsd),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para domains%idata'
            stop 1
        endif
        
        domains%idata = .false.




      
        
! calculos para fuentes volumicas
      if(sourcevol%numero.gt.0)then
         call tabvol ()  
	  endif
	  
	  if(num_inputsv.gt.0)then
	    call tabpot()
	  endif
	  
	  
	  if(num_inputsi.gt.0)then
	    call tabcurint()
	  endif
	  
        
        
!calculos para bloqueo
     
      if (dirichlet_bc%numero.gt.0) then
     
     	call tabbloq(nel,mm,nra,dirichlet_bc%numero,dirichlet_bc%referencias,&
     &	             nvdi,nvd,nrvd,indblo)
        
      endif
     
!calculos para neumann    
     
      if (neumann_bc%numero.gt.0) then
     
     	call tabnesu(nel,mm,nra,z,neumann_bc%numero,nrane,neumann_bc%referencias,&
     &	             ntane,nvane,indblone,xlongne)
        
      endif
     
  

      return
      end
