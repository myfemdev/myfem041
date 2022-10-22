*******
        subroutine refsourcevol ()
        
      

      ! Subrutina para calcular los tableros asociados a las fuerzas volúmicas
     

      ! variable de salida: ensd, nelem

        use  malla_2DP1           ,  only : nel, mm, nra,nsd
        use sourcevolumic
        
         
        use  parametros_electros	,  only : ndar
        use derivados

        implicit double precision (a-h,o-z)
     

        if(sourcevol%numero.eq.0) return
        
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
        
        nelem = 0; ensd = 0
            do k = 1, nel
                do i = 1, sourcevol%numero
                    if (sourcevol%referencias(i) == nsd(k)) then
                        ensd(i,nelem(i)+1) = k
                        nelem(i) = nelem(i) + 1
                        exit
                    endif
                enddo
            enddo

!!!!!!!! fin: precalcular lista de elementos que corresponden con cada indice de carga volumica

         return
         end
