      SUBROUTINE CMUA2(MUA,MM,NEL,NVER)
      
      use potenciales_vol
      use potenciales_sur
      use derivados
      
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION MUA(1),MM(3,1),mino(100)
      
      DO 1 I=2,NVER+1
1     MUA(I)=I-1
*
      DO 2  K=1,NEL

      MI=MIN0(MM(1,K),MM(2,K),MM(3,K))
      DO 2 J=1,3
2     MUA(MM(J,K)+1)=MIN0(MUA(MM(J,K)+1),MI)

      if(potencial_dat_vol%numero.gt.0) then
        do i=1,potencial_dat_vol%numero
      
           mino(i)=1000000000
           do l=1,nelempo(i)
             k=ensdpo(i,l)
             mino(i)=MIN0(MM(1,K),MM(2,K),MM(3,K),mino(i))
           end do
         
        end do
        
         
        if (potencial_dat_vol%ncouples.gt.0) then
            do i=1, potencial_dat_vol%ncouples
             mino(potencial_dat_vol%icouple(1,i))=
     &           MIN0(mino(potencial_dat_vol%icouple(1,i)),
     &                mino(potencial_dat_vol%icouple(2,i)))
             mino(potencial_dat_vol%icouple(2,i))=
     &        mino(potencial_dat_vol%icouple(1,i))
            enddo
        endif
                            
     
            
        
      
        do i=1,potencial_dat_vol%numero
      
          do l=1,nelempo(i)
            k=ensdpo(i,l)
            do j=1,3
              MUA(MM(J,K)+1)=MIN0(MUA(MM(J,K)+1),mino(i))
            end do
          end do
        
        end do
      end if
      
      
      
      if(potencial_dat_sur%numero.gt.0) then
        do i=1,potencial_dat_sur%numero
           mino(i)=1000000000
        end do
       
        do l=1,ntapo
          i=indblopo(l)
          mino(i)=MIN0(nvapo(1,l),nvapo(2,l),mino(i))
        end do
        
        if (potencial_dat_sur%ncouples.gt.0) then
            do i=1, potencial_dat_sur%ncouples
             mino(potencial_dat_sur%icouple(1,i))=
     &           MIN0(mino(potencial_dat_sur%icouple(1,i)),
     &                mino(potencial_dat_sur%icouple(2,i)))
             mino(potencial_dat_sur%icouple(2,i))=
     &        mino(potencial_dat_sur%icouple(1,i))
            enddo
        endif
 
        do l=1,ntapo
          i=indblopo(l)
          do j=1,2
            MUA(nvapo(j,l)+1)=MIN0(MUA(nvapo(j,l)+1),mino(i))
          end do
        end do
      end if
      
      !write(19,*),(mua(i+1), i=1,nver)

      MUA(1)=0
      DO 3 I=3,NVER+1
3     MUA(I)=MUA(I-1)+I-MUA(I)

      RETURN
      END
