************                                 ************
******subrutina para calculo de la matriz elemental******
****** del segundo  miembro,corresp. al elemento k ******
************                                 ************


      subroutine matelmasa()
     
      use malla_2DP1
      use electros_2D
      use potenciales_vol
      use potenciales_sur
      use tiempo
      use derivados

      implicit double precision (a-h,o-z)
      character*255 etiqueta
      integer aux      


C        OPCION PARA FUENTES VOLUMICAS
!        forma de calcular utilizando los arrays de sourcevol

      do i = 1, potencial_dat_vol%numero
      
        modo=potencial_dat_vol%modo1(i)
        valor=potencial_dat_vol%valor1(i)
        etiqueta=potencial_dat_vol%etiqueta1(i)
        condel=sigma_vol(i,modo,valor,etiqueta)

        do j1 = 1, nelempo(i)
        
          k1 = ensdpo(i,j1)    
          mm1 = mm(1,k1)
          mm2 = mm(2,k1)
          mm3 = mm(3,k1)
                    
C                  CALCULOS PREVIOS

          ab = z(1,mm2) - z(1,mm1)
          bc = z(2,mm2) - z(2,mm1)
          cd = z(1,mm3) - z(1,mm1)
          de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

          det1 = ab*de - bc*cd  
          
          do j2=1,nelempo(i)  
          
             k2 = ensdpo(i,j2)    
             mm1 = mm(1,k2)
             mm2 = mm(2,k2)
             mm3 = mm(3,k2)
                    
C                  CALCULOS PREVIOS

             ab = z(1,mm2) - z(1,mm1)
             bc = z(2,mm2) - z(2,mm1)
             cd = z(1,mm3) - z(1,mm1)
             de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

             det2 = ab*de - bc*cd
             
             if (potencial_dat_vol%ncouples.gt.0) then
           
               do j=1, potencial_dat_vol%ncouples
                 if((i.eq.potencial_dat_vol%icouple(1,j)).or.(i.eq.
     &             potencial_dat_vol%icouple(2,j))) then
               

                   xmasa= det1*det2*condel
     &                    /xintsvol(i)
     &                    /36.d0/deltat/areapo(i)/
     &                   (1.d0/xintsvol(potencial_dat_vol%icouple(1,j))
     &                   +1.d0/xintsvol(potencial_dat_vol%icouple(2,j)))
                   go to 1
                 end if
               end do 
                
               xmasa=det1*det2*condel/36.d0/deltat/areapo(i)
              
              
             else
            
               xmasa=det1*det2*condel/36.d0/deltat/areapo(i)
            
             endif
             
   1         continue
                      
             do n1=1,3
               do n2=1,3
                 if(mm(n2,k2).le.mm(n1,k1)) then
                   l=mua(mm(n1,k1)+1)-mm(n1,k1)+mm(n2,k2)
                   c(l)=c(l)+xmasa
                 end if
               end do
             end do
             
           end do
         enddo
       enddo
          
          

      if(potencial_dat_sur%numero.gt.0) then
        
        do j1=1,ntapo
                 
          ipo1=indblopo(j1)  ! numero de orden
          modo=potencial_dat_sur%modo1(ipo1)
          valor=potencial_dat_sur%valor1(ipo1)
          etiqueta=potencial_dat_sur%etiqueta1(ipo1)
          espesor=potencial_dat_sur%valor0(ipo1)
          condel=sigma_sur(ipo1,modo,valor,etiqueta)*espesor
                

C                       EXTREMO DE LA ARISTA
                       
          nov1 = nvapo(1,j1)
          nov2 = nvapo(2,j1)
                                

C                       LONGITUD DE LA ARISTA

          delta1 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
          do j2=1,ntapo
                 
            ipo2=indblopo(j2)  !numero de orden de subdominio superficial
            
            if(ipo1.eq.ipo2) then

C                       EXTREMOS DE LA ARISTA
                       
              nov1 = nvapo(1,j2)
              nov2 = nvapo(2,j2)
                      

C                       LONGITUD DE LA ARISTA

              delta2 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
     
              if (potencial_dat_sur%ncouples.gt.0) then
            
              
                do i=1, potencial_dat_sur%ncouples
                  if((ipo1.eq.potencial_dat_sur%icouple(1,i)).or.(ipo1
     &               .eq.potencial_dat_sur%icouple(2,i))) then

                     xmasa= delta1*delta2*0.25d0*condel
     &                   /xintssur(ipo1)/deltat/xlongpo(ipo1)/
     &                   (1.d0/xintssur(potencial_dat_sur%icouple(1,i))
     &                   +1.d0/xintssur(potencial_dat_sur%icouple(2,i)))
                     go to 2
                  end if
                end do
              
                xmasa=delta1*delta2*0.25d0*condel/deltat/xlongpo(ipo1)
              
              
              else
            
                xmasa=delta1*delta2*0.25d0*condel/deltat/xlongpo(ipo1)
            
              endif
              
2             continue
              
     
              do n1=1,2
                do n2=1,2
                  if(nvapo(n2,j2).le.nvapo(n1,j1)) then
                    l=mua(nvapo(n1,j1)+1)-nvapo(n1,j1)+nvapo(n2,j2)
                    c(l)=c(l)+xmasa
                  end if
                end do
              end do
            end if
          end do
        end do        
      end if
     
 ! introduccion de los operadores fuera de la diagonal principal
 
      if(potencial_dat_vol%ncouples.gt.0) then
      
        do j= 1, potencial_dat_vol%ncouples
        
          i1=potencial_dat_vol%icouple(1,j)
          modo=potencial_dat_vol%modo1(i1)
          valor=potencial_dat_vol%valor1(i1)
          etiqueta=potencial_dat_vol%etiqueta1(i1)
          condel1=sigma_vol(i1,modo,valor,etiqueta)
          
          i2=potencial_dat_vol%icouple(2,j)
          modo=potencial_dat_vol%modo1(i2)
          valor=potencial_dat_vol%valor1(i2)
          etiqueta=potencial_dat_vol%etiqueta1(i2)
          condel2=sigma_vol(i2,modo,valor,etiqueta)
          
 
          do j1 = 1,nelempo(potencial_dat_vol%icouple(1,j))
        
            k1 = ensdpo(potencial_dat_vol%icouple(1,j),j1)    
            mm1 = mm(1,k1)
            mm2 = mm(2,k1)
            mm3 = mm(3,k1)
                    
C                  CALCULOS PREVIOS

            ab = z(1,mm2) - z(1,mm1)
            bc = z(2,mm2) - z(2,mm1)
            cd = z(1,mm3) - z(1,mm1)
            de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

            det1 = ab*de - bc*cd  
          
            do j2=1,nelempo(potencial_dat_vol%icouple(2,j))         
                           
              k2 = ensdpo(potencial_dat_vol%icouple(2,j),j2)    
              mm1 = mm(1,k2)
              mm2 = mm(2,k2)
              mm3 = mm(3,k2)
                    
C                  CALCULOS PREVIOS

              ab = z(1,mm2) - z(1,mm1)
              bc = z(2,mm2) - z(2,mm1)
              cd = z(1,mm3) - z(1,mm1)
              de = z(2,mm3) - z(2,mm1)

C           JACOBIANO DE LA MATRIZ DE PASO AL ELEM. DE REFERENCIA

              det2 = ab*de - bc*cd
!                            
!              xmasa=-det1*det2/36.d0/deltat/
!     &                (areapo(potencial_dat_vol%icouple(1,j))*
!     &                 areapo(potencial_dat_vol%icouple(2,j)))/
!     &                (1.d0/xintsvol(potencial_dat_vol%icouple(1,j))+
!     &                 1.d0/xintsvol(potencial_dat_vol%icouple(2,j)))

                            
              xmasa1=-det1*det2/36.d0/deltat*condel2/
     &                areapo(potencial_dat_vol%icouple(1,j))
     &                /xintsvol(potencial_dat_vol%icouple(2,j))/
     &                (1.d0/xintsvol(potencial_dat_vol%icouple(1,j))+
     &                 1.d0/xintsvol(potencial_dat_vol%icouple(2,j)))
     
              xmasa2=-det1*det2/36.d0/deltat*condel1/
     &                areapo(potencial_dat_vol%icouple(2,j))
     &                /xintsvol(potencial_dat_vol%icouple(1,j))/
     &                (1.d0/xintsvol(potencial_dat_vol%icouple(1,j))+
     &                 1.d0/xintsvol(potencial_dat_vol%icouple(2,j)))
     
              do n1=1,3
                do n2=1,3
                  if(mm(n2,k2).lt.mm(n1,k1)) then
                    l=mua(mm(n1,k1)+1)-mm(n1,k1)+mm(n2,k2)
                    c(l)=c(l)+xmasa1
                  else if (mm(n1,k1).lt.mm(n2,k2)) then
                    l=mua(mm(n2,k2)+1)-mm(n2,k2)+mm(n1,k1)
                    c(l)=c(l)+xmasa2
                  else
                    print*,'error in matelmasa'
                    stop
                  end if
                end do
              end do
               
            end do
          end do
        end do
      end if
      
      if(potencial_dat_sur%ncouples.gt.0) then
         !print*,'ntapo',ntapo

        do j1=1,ntapo
                 
          ipo1=indblopo(j1)
               
          do j2=1,ntapo
                 
            ipo2=indblopo(j2)
            
            if(ipo1.ne.ipo2) then
            ! print*,'ipo1 ne ipo2',ipo1,ipo2
            ! print*,'j1,j2',j1,j2
      
              do i=1,potencial_dat_sur%ncouples
     
                if(ipo1.eq.potencial_dat_sur%icouple(1,i).and.
     &              ipo2.eq.potencial_dat_sur%icouple(2,i)) then
 
                  modo=potencial_dat_sur%modo1(ipo2)
                  valor=potencial_dat_sur%valor1(ipo2)
                  etiqueta=potencial_dat_sur%etiqueta1(ipo2)
                  espesor=potencial_dat_sur%valor0(ipo2)
                  condel2=sigma_sur(ipo2,modo,valor,etiqueta)*espesor
     
     
C                       EXTREMO DE LA ARISTA j1
                       
                  nov1 = nvapo(1,j1)
                  nov2 = nvapo(2,j1)
                  
                  !print*,'primeiro IF j1,nov1,nov2',j1,nov1,nov2
                              

C                       LONGITUD DE LA ARISTA j1

                  delta1 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
              
C                       EXTREMOS DE LA ARISTA j2
                       
                  nov1 = nvapo(1,j2)
                  nov2 = nvapo(2,j2)                      
                 ! print*,'j2,nov1,nov2',j2,nov1,nov2

!                       LONGITUD DE LA ARISTA j2

                  delta2 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
                  xmasa1=-delta1*delta2*0.25d0*condel2/deltat
     &                   /xintssur(ipo2)/xlongpo(ipo1)/
     &                   (1.d0/xintssur(ipo1)+1.d0/xintssur(ipo2))
                       
                  do n1=1,2
                    do n2=1,2
                      if(nvapo(n2,j2).lt.nvapo(n1,j1)) then
                        !print*,'ensamblado de xmasa1'
                        !print*,'vertices',nvapo(n2,j2),nvapo(n1,j1)
                       ! print*,'ipo1,ipo2,j1,j2',ipo1,ipo2,j1,j2
                        l=mua(nvapo(n1,j1)+1)-nvapo(n1,j1)+nvapo(n2,j2)
                        c(l)=c(l)+xmasa1
                        
                      end if
                    end do
                  end do
                end if
                
                if(ipo1.eq.potencial_dat_sur%icouple(2,i).and.
     &              ipo2.eq.potencial_dat_sur%icouple(1,i)) then
     
                  modo=potencial_dat_sur%modo1(ipo1)
                  valor=potencial_dat_sur%valor1(ipo1)
                  etiqueta=potencial_dat_sur%etiqueta1(ipo1)
                  espesor=potencial_dat_sur%valor0(ipo1)
                  condel1=sigma_sur(ipo1,modo,valor,etiqueta)*espesor
                   
C                       EXTREMO DE LA ARISTA j1
                       
                  nov1 = nvapo(1,j1)
                  nov2 = nvapo(2,j1)
                  !print*,'no outro if',ipo1,ipo2
                  !print*,'j1,nov1,nov2',j1,nov1,nov2
                              

C                       LONGITUD DE LA ARISTA j1

                  delta1 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
              
C                       EXTREMOS DE LA ARISTA j2
                       
                  nov1 = nvapo(1,j2)
                  nov2 = nvapo(2,j2)                
                  !print*,'j2,nov1,nov2',j2,nov1,nov2      

!                       LONGITUD DE LA ARISTA j2

                  delta2 = DSQRT((z(1,nov2) - z(1,nov1))**2 +
     &                (z(2,nov2) - z(2,nov1))**2)
     
                  xmasa2=-delta1*delta2*0.25d0*condel1/deltat
     &              /xintssur(ipo1)/xlongpo(ipo2)/
     &              (1.d0/xintssur(ipo1)+1.d0/xintssur(ipo2))
     

     
                  do n1=1,2
                    do n2=1,2
                    !  if(nvapo(n1,j1).lt.nvapo(n2,j2)) then
                    !    l=mua(nvapo(n2,j2)+1)-nvapo(n2,j2)+nvapo(n1,j1)
                        if(nvapo(n2,j2).lt.nvapo(n1,j1)) then
                        l=mua(nvapo(n1,j1)+1)-nvapo(n1,j1)+nvapo(n2,j2) 
                         !print*,'ensamblado de xmasa2'
                        !print*,'vertices',nvapo(n1,j1),nvapo(n2,j2)
                       ! print*,'ipo1,ipo2,j1,j2',ipo1,ipo2,j1,j2
                        c(l)=c(l)+xmasa2
                      end if
                    end do
                  end do
                end if
                
                
                
                
              end do
            end if
          end do
        end do
      end if

      return
      end

