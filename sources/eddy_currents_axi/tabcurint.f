*******
        subroutine tabcurint ()
        
      

      ! Subrutina para calcular los tableros asociados a las intensidades de entrada
     

      ! variable de salida: ensdint, nelemint

         
        use  parametros_electros
        use derivados
        use malla_2DP1
        use intensity_input
        
        implicit double precision (a-h,o-z)
     
        allocate(nelemint(num_inputsi),stat=ierror)
        if (ierror.ne.0) then
          print*,'error: no se ha podido reservar memoria para nelemint'
          stop 1
        endif
        allocate(ensdint(num_inputsi,nel),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para ensdint'
           stop 1
        endif
        
         allocate(areaint(num_inputsi),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para areaint'
           stop 1
        endif
        
        
        nelemint = 0; ensdint= 0; areaint = 0;
        do k=1,nel
          if(num_inputsi.gt.0) then               
               do i = 1, num_inputsi  
                 do l = 1,inputsi(i)%nsubdo
                   if (inputsi(i)%subdo_references(l) == nsd(k))then          
                        domains%idata(nsd(k)) = .true.  
                        ensdint(i,nelemint(i)+1) = k
                        nelemint(i) = nelemint(i) + 1
                       
                        
                        mm1 = mm(1,k)
                        mm2 = mm(2,k)
                        mm3 = mm(3,k)
            
                        ab = z(1,mm2) - z(1,mm1)
                        bc = z(2,mm2) - z(2,mm1)
                        cd = z(1,mm3) - z(1,mm1)
                        de = z(2,mm3) - z(2,mm1)

                        det = ab*de - bc*cd  
                        
                        areaint(i) = areaint(i)+det/2.d0
                        exit
                       
                    endif 
                  enddo 
                enddo
              end if
              
            enddo
        
        
           
       
        
         return
         end
