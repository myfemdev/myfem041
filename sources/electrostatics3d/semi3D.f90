!-----------------------------------------------------------------
!                 CONSTRUCTION OF THE SECOND MEMBER 
!-----------------------------------------------------------------

subroutine semi3D()
      
  use malla_3DP1
  use electros3D
  use external_electros3D
  use cargavol
  use cargacur
  use cargapun 
  use cargasup
  use neumann
  use auxiliar_cargas

  implicit none
  double precision           :: vnu(3)  
  double precision           :: xnod11,xnod22,xnod1,xnod2,dnor,area,gcons
  double precision           :: x1,y1,z1,x2,y2,z2,delta,bar1,bar2,bar3,bar4
  double precision, external :: f
  integer                    :: mm1,mm2,mm3,mm4,iref,iv1,iv2,iv3,nov1,nov2
  integer                    :: itetr
  integer                    :: i,j,k,ii

  if (iopf .eq. 1) then
     xnod11 = -0.577350269189626
     xnod22 =  0.577350269189626
  else if (iopf .eq. 2) then
     xnod11 = -1.
     xnod22 =  1.
  end if
  xnod1 = (1. + xnod11) * 0.5D0
  xnod2 = (1. + xnod22) * 0.5D0
        
  if (carvol%numero > 0 .and. iopvol == 1 .and. iop == 2) then
     do i = 1, carvol%numero
        do j = 1, nelem(i)
           k = ensd(i,j)    
           mm1 = mm(1,k)
           mm2 = mm(2,k)
           mm3 = mm(3,k)
           mm4 = mm(4,k) 
           if (carvol%constante(i)) then
              b(mm1)=b(mm1)+fc(nsd(k),i)*det(k)/24d0
              b(mm2)=b(mm2)+fc(nsd(k),i)*det(k)/24d0
              b(mm3)=b(mm3)+fc(nsd(k),i)*det(k)/24d0
              b(mm4)=b(mm4)+fc(nsd(k),i)*det(k)/24d0
           else
              b(mm1)=b(mm1)+f(z(1,mm1),z(2,mm1),z(3,mm1),nsd(k),i)*det(k)/24d0
              b(mm2)=b(mm2)+f(z(1,mm2),z(2,mm2),z(3,mm2),nsd(k),i)*det(k)/24d0
              b(mm3)=b(mm3)+f(z(1,mm3),z(2,mm3),z(3,mm3),nsd(k),i)*det(k)/24d0
              b(mm4)=b(mm4)+f(z(1,mm4),z(2,mm4),z(3,mm4),nsd(k),i)*det(k)/24d0
           endif 
        enddo
     enddo
  endif
        
  bucle_elementos: do k=1,nel

! INTEGRATION IN THE NEUMANN BOUNDARY
     opcion_cc_neumann: if(iopneu.eq.1) then

        opcion_cc_neuman_fun: if(iopneu1.eq.1) then
           if (nrn .gt. 0) then
              bucle1: do j=1,4
                 do i=1,nrn
                    if (nrc(j,k) .eq. irefn(i)) then
                       iref = irefn(i)
                       call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),&
                                     z(1,mm(indc(3,j),k)),vnu,dnor)
                       iv1=mm(indc(1,j),k)
                       iv2=mm(indc(2,j),k)
                       iv3=mm(indc(3,j),k)
                       area=dnor/2.d0
                       b(iv1)=b(iv1)+g(z(1,iv1),z(2,iv1),z(3,iv1),iref,i)*area/3.d0
                       b(iv2)=b(iv2)+g(z(1,iv2),z(2,iv2),z(3,iv2),iref,i)*area/3.d0
                       b(iv3)=b(iv3)+g(z(1,iv3),z(2,iv3),z(3,iv3),iref,i)*area/3.d0
                       cycle bucle1
                    end if
                 enddo   
              enddo bucle1
           end if
        endif opcion_cc_neuman_fun

        opcion_cc_neuman_cons: if(iopneu2.eq.1) then
           if (neuman%numero .gt. 0) then
              bucle2: do j=1,4
                 do i=1,neuman%numero
                    if (nrc(j,k) .eq. neuman%referencias(i)) then
                       iref = neuman%referencias(i)
                       call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),&
                                     z(1,mm(indc(3,j),k)),vnu,dnor)
                       iv1=mm(indc(1,j),k)
                       iv2=mm(indc(2,j),k)
                       iv3=mm(indc(3,j),k)
                       area=dnor/2.d0
                       gcons=neuman%valor(i)
                       b(iv1)=b(iv1)+gcons*area/3.d0
                       b(iv2)=b(iv2)+gcons*area/3.d0
                       b(iv3)=b(iv3)+gcons*area/3.d0
                       cycle bucle2
                    endif
                 enddo
              enddo bucle2
           endif
        endif opcion_cc_neuman_cons

     endif opcion_cc_neumann
      
  enddo bucle_elementos 

! INTEGRATION OF SUPERFICIAL CHARGES

  opcion_cargas_sup: if (iopsup .eq. 1) then
     docargas_sup: do i=1,carsup%numero
        docaras: do ii=1,ncaras(i)
           iv1 = nodc1(i,ii)
           iv2 = nodc2(i,ii)
           iv3 = nodc3(i,ii)
           call calvnoru(z(1,iv1),z(1,iv2),z(1,iv3),vnu,dnor)
           area=dnor/2.d0
           iref = carsup%referencias(i)
           if (carsup%constante(i)) then
              b(iv1)=b(iv1)+gcst(iref,i)*area/3.d0
              b(iv2)=b(iv2)+gcst(iref,i)*area/3.d0
              b(iv3)=b(iv3)+gcst(iref,i)*area/3.d0
           else
              b(iv1)=b(iv1)+gcs(z(1,iv1),z(2,iv1),z(3,iv1),iref,i)*area/3.d0
              b(iv2)=b(iv2)+gcs(z(1,iv2),z(2,iv2),z(3,iv2),iref,i)*area/3.d0
              b(iv3)=b(iv3)+gcs(z(1,iv3),z(2,iv3),z(3,iv3),iref,i)*area/3.d0
           end if
        end do docaras
     end do docargas_sup
  end if opcion_cargas_sup

! INTEGRATION OF CURVILINEAR CHARGES

  opcion_cargas_cur: if (iopcur .eq. 1) then
     docargas: do  i=1,carcur%numero
        doaristas: do ii=1,naristas(i)
           nov1 = nod1(i,ii)
           nov2 = nod2(i,ii)
           x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
           y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
           z1=  z(3,nov1) + (z(3,nov2)-z(3,nov1)) * xnod1
           x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
           y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
           z2 = z(3,nov1) + (z(3,nov2)-z(3,nov1)) * xnod2
           delta = dsqrt((z(1,nov2) - z(1,nov1))**2 + &
             (z(2,nov2) - z(2,nov1))**2+ (z(3,nov2)-z(3,nov1))**2)
           iref = carcur%referencias(i)
           if (carcur%constante(i)) then
              b(nov1) = b(nov1) + delta *&  
               (gcct(iref,i)*xnod2 + gcct(iref,i)*xnod1)*0.5d0
              b(nov2) = b(nov2) + delta *&
               (gcct(iref,i)*xnod1 + gcct(iref,i)*xnod2)*0.5d0
           else
              b(nov1) = b(nov1) + delta *&  
               (gcc(x1,y1,z1,iref,i)*xnod2 + gcc(x2,y2,z2,iref,i)*xnod1)*0.5d0
              b(nov2) = b(nov2) + delta *&
               (gcc(x1,y1,z1,iref,i)*xnod1 + gcc(x2,y2,z2,iref,i)*xnod2)*0.5d0
           end if
        end do doaristas
     end do docargas
  end if opcion_cargas_cur
   
! INCORPORATION OF PUNTUAL CHARGES  
  opcion_carga_pun: if (ioppun.eq.1) then
     bucle_cargas_puntuales: do i=1,ncarpun
        call scarpun(xcarpun(i),ycarpun(i),zcarpun(i),itetr,bar1,bar2,bar3,bar4)
        mm1=mm(1,itetr)
        mm2=mm(2,itetr)
        mm3=mm(3,itetr)
        mm4=mm(4,itetr)
        b(mm1) = b(mm1) + bar1 * carpun(i)
        b(mm2) = b(mm2) + bar2 * carpun(i)
        b(mm3) = b(mm3) + bar3 * carpun(i)
        b(mm4) = b(mm4) + bar4 * carpun(i)
     end do bucle_cargas_puntuales
  end if opcion_carga_pun

end
