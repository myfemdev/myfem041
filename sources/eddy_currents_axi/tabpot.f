*******
        subroutine tabpot ()
        
      

      ! Subrutina para calcular los tableros asociados a las fuerzas voltage
     

      ! variable de salida: ensdpo, nelempo

         
        use  parametros_electros
        use derivados
        use malla_2DP1
        use voltage_drop

        implicit double precision (a-h,o-z)
     
        allocate(nelempo(num_inputsv),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para nelempo'
           stop 1
        endif
        allocate(ensdpo(num_inputsv,nel),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para ensdpo'
            stop 1
        endif
        
         allocate(areapo(num_inputsv),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para areapo'
            stop 1
        endif
        
        
        nelempo = 0; ensdpo= 0; areapo = 0;
        do k=1,nel
          if(num_inputsv.gt.0) then               
               do i = 1, num_inputsv  
                 do l = 1,inputsv(i)%nsubdo
                   if (inputsv(i)%subdo_references(l) == nsd(k))then          
                        domains%vdata(nsd(k)) = .true.  
                        ensdpo(i,nelempo(i)+1) = k
                        nelempo(i) = nelempo(i) + 1
                       
                        
                        mm1 = mm(1,k)
                        mm2 = mm(2,k)
                        mm3 = mm(3,k)
            
                        ab = z(1,mm2) - z(1,mm1)
                        bc = z(2,mm2) - z(2,mm1)
                        cd = z(1,mm3) - z(1,mm1)
                        de = z(2,mm3) - z(2,mm1)

                        det = ab*de - bc*cd  
                        
                        areapo(i) = areapo(i)+det/2.d0
                        exit
                       
                    endif 
                  enddo   
                enddo
              end if
              
            enddo
        
           
       
        
         return
         end
