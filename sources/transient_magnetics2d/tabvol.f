*******
        subroutine tabvol ()
        
      

      ! Subrutina para calcular los tableros asociados a las fuerzas volúmicas
     

      ! variable de salida: ensd, nelem

         
        use parametros_electros
        use derivados
        use malla_2DP1
        use sourcevolumic
        use potenciales_vol

        implicit double precision (a-h,o-z)
     
        
        allocate(nelem(sourcevol%numero),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para nelem'
           stop
        endif
        allocate(ensd(sourcevol%numero,nel),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para ensd'
           stop
        endif
        
        allocate(nelempo(potencial_dat_vol%numero),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para nelempo'
           stop
        endif
        allocate(ensdpo(potencial_dat_vol%numero,nel),stat=ierror)
        if (ierror.ne.0) then
           print*,'error: no se ha podido reservar memoria para ensdpo'
           stop
        endif
        
        
        nelem = 0; ensd = 0; nelempo = 0; ensdpo= 0; area= 0; areapo=0
            do k = 1, nel
              if(sourcevol%numero.gt.0) then
                do i = 1, sourcevol%numero
                    if (sourcevol%referencias(i) == nsd(k)) then
                    
                        ensd(i,nelem(i)+1) = k
                        nelem(i) = nelem(i) + 1
                        
                        mm1 = mm(1,k)
                        mm2 = mm(2,k)
                        mm3 = mm(3,k)
            
                        ab = z(1,mm2) - z(1,mm1)
                        bc = z(2,mm2) - z(2,mm1)
                        cd = z(1,mm3) - z(1,mm1)
                        de = z(2,mm3) - z(2,mm1)

                        det = ab*de - bc*cd  
                        
                        area(i) = area(i)+det/2.d0
                        exit
                    endif
                enddo
              end if
                
              if(potencial_dat_vol%numero.gt.0) then               
                do i = 1, potencial_dat_vol%numero            
                    if (potencial_dat_vol%referencias(i) == nsd(k)) then
                    
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
              end if
              
            enddo
            
            
          

         return
         end
