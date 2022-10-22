      subroutine tabnesu(nel,mm,nra,z,nref,nranesu,iref,nta,nva,indblo,
     &           xlong)

      implicit double precision (a-h,o-z)
      dimension mm(3,*),nra(3,*),nranesu(*),iref(*),nva(2,*),indblo(*),
     &          xlong(*),z(2,*)


*********************************************************************
* creacion del tablero de vertices neumann o de fuente superficial  * 
*********************************************************************
      nta=0
      if(nref.gt.0) then
        do  i=1,nref
          xlong(i)=0.d0
        end do
        
        do  i=1,nref
          do  k=1,nel
            do  l=1,3
              if (nra(l,k).eq.iref(i)) then
                ik1=l;ik2=mod(l,3)+1
                                
                if(nta.gt.0) then
                  do  ll=1,nta
                    if((mm(ik1,k).eq.nva(1,ll).and.mm(ik2,k).
     &                  eq.nva(2,ll)).or.(mm(ik1,k).eq.nva(2,ll).
     &                  and.mm(ik2,k).eq.nva(1,ll))) then 
                      go to 14 
                    endif
                  end do
                end if
              
                nta=nta+1
                nva(1,nta)=mm(ik1,k)  
                nva(2,nta)=mm(ik2,k)  
                nranesu(nta)=nra(l,k)
	          indblo(nta) = i
	        
	          nov1 = nva(1,nta)
                nov2 = nva(2,nta)

C                       LONGITUD DE LA CURVA

                delta = dsqrt((z(1,nov2) - z(1,nov1))**2 +
     &                      (z(2,nov2) - z(2,nov1))**2)
                xlong(i)=xlong(i)+delta

  14            continue 
              end if

            end do
          end do    
        end do
         
      end if

      return
      end
