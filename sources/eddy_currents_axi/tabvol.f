*******
        subroutine tabvol ()
        
      

      ! Subrutina para calcular los tableros asociados a las fuerzas volúmicas
     

      ! variable de salida: ensd, nelem

         
        !use  parametros_electros
        use derivados
        use malla_2DP1
        use sourcevolumic
        use electros_2D
        
        implicit double precision(a-h,o-z)
        character(len=255) :: cad
       
     

        if(sourcevol%numero.eq.0) return
        
        allocate(nelem(sourcevol%numero),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para nelem'
            stop 1
        endif
        allocate(ensd(sourcevol%numero,nel),stat=ierror)
        if (ierror.ne.0) then
            print*,'error: no se ha podido reservar memoria para ensd'
            stop 1
        endif
   
        
        nelem = 0; ensd = 0; area= 0
            do k = 1, nel

               do i = 1, sourcevol%numero
                   if (sourcevol%referencias(i) == nsd(k)) then
                     domains%jsdata(nsd(k)) = .true.
              
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
            enddo
            
     
        
         return
         end
