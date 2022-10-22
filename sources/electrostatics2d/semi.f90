!-----------------------------------------------------------------
!                 CONSTRUCTION OF THE SECOND MEMBER 
!-----------------------------------------------------------------

subroutine semi()

  use malla_2DP1
  use electros_2D
  use external_electros
  use cargavol
  use cargacur
  use cargapun 
  use neumann

  implicit none
  double precision          :: xnod11,xnod22,xnod1,xnod2
  integer                   :: mm1,mm2,mm3,iref,nov1,nov2,itria
  double precision          :: ab,bc,cd,de,det,xbar,ybar,xint
  double precision          :: x12,x23,x31,y12,y23,y31,xint12,xint23,xint31
  double precision          :: x1,x2,y1,y2,delta,g1,g2,bar1,bar2,bar3
  double precision,external :: f
  integer                   :: i,j,k
      
  if (iopf .eq. 1) then
     xnod11 = -0.577350269189626
     xnod22 =  0.577350269189626
  else if (iopf .eq. 2) then
     xnod11 = -1.
     xnod22 =  1.
  end if
  xnod1 = (1. + xnod11) * 0.5d0
  xnod2 = (1. + xnod22) * 0.5d0
   
  do i=1,nver
     b(i) = 0.d0
  enddo

  do k=1,nel

     opcion_carga_vol: if (iopvol.eq.1) then
        mm1 = mm(1,k)
        mm2 = mm(2,k)
        mm3 = mm(3,k)
        ab = z(1,mm2) - z(1,mm1)
        bc = z(2,mm2) - z(2,mm1)
        cd = z(1,mm3) - z(1,mm1)
        de = z(2,mm3) - z(2,mm1)
        det = ab*de - bc*cd
        if (iop .eq. 1) then
           xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.d0
           ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.d0
           if (iopinvol.eq.1) then
              xint = f(xbar,ybar,nsd(k)) * det / 6.d0
           else if (iopinvol.eq.2) then
              xint = fc(nsd(k)) * det / 6.d0
           end if
           b(mm1) = b(mm1) + xint
           b(mm2) = b(mm2) + xint
           b(mm3) = b(mm3) + xint
        else if (iop .eq. 2) then
           if (iopinvol.eq.1) then
              b(mm1) = b(mm1) + f(z(1,mm1),z(2,mm1),nsd(k))*det / 6.d0
              b(mm2) = b(mm2) + f(z(1,mm2),z(2,mm2),nsd(k))*det / 6.d0
              b(mm3) = b(mm3) + f(z(1,mm3),z(2,mm3),nsd(k))*det / 6.d0
           else if (iopinvol.eq.2) then
              b(mm1) = b(mm1) + fc(nsd(k))*det / 6.d0
              b(mm2) = b(mm2) + fc(nsd(k))*det / 6.d0
              b(mm3) = b(mm3) + fc(nsd(k))*det / 6.d0
           end if
        else
           x12 = (z(1,mm1) + z(1,mm2)) * 0.5d0
           x23 = (z(1,mm2) + z(1,mm3)) * 0.5d0
           x31 = (z(1,mm3) + z(1,mm1)) * 0.5d0
           y12 = (z(2,mm1) + z(2,mm2)) * 0.5d0
           y23 = (z(2,mm2) + z(2,mm3)) * 0.5d0
           y31 = (z(2,mm3) + z(2,mm1)) * 0.5d0
           if (iopinvol.eq.1) then
              xint12 = (f(x12,y12,nsd(k))) * det 
              xint23 = (f(x23,y23,nsd(k))) * det
              xint31 = (f(x31,y31,nsd(k))) * det
           else if (iopinvol.eq.2) then
              xint12 = (fc(nsd(k))) * det 
              xint23 = (fc(nsd(k))) * det
              xint31 = (fc(nsd(k))) * det
           end if
           b(mm1) = b(mm1) + (xint12 + xint31) / 12d0
           b(mm2) = b(mm2) + (xint12 + xint23) / 12d0 
           b(mm3) = b(mm3) + (xint23 + xint31) / 12d0 
        end if
     end if opcion_carga_vol    

  ! INTEGRATION IN THE NEUMANN BOUNDARY
    opcion_cc_neumann: if(iopneu.eq.1) then
  
       opcion_cc_neuman_fun:   if(iopinneu1.eq.1) then
          if (nrn .gt. 0) then
             bucle1: do j=1,3
                do i=1,nrn
                   if (nra(j,k) .eq. irefn(i)) then
                      iref = irefn(i)
                      nov1 = mm(j,k)
                      nov2 = mm(1+mod(j,3),k)                    
                      x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
                      y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
                      x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
                      y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      delta = dsqrt((z(1,nov2) - z(1,nov1))**2 +&
                                    (z(2,nov2) - z(2,nov1))**2)
                      b(nov1) = b(nov1) + delta *&  
                       (g(x1,y1,iref)*xnod2 + g(x2,y2,iref)*xnod1)*0.5d0
                      b(nov2) = b(nov2) + delta *&
                       (g(x1,y1,iref)*xnod1 + g(x2,y2,iref)*xnod2)*0.5d0
                      cycle bucle1 
                   end if
                enddo
             enddo bucle1
          end if
       endif opcion_cc_neuman_fun
   
       opcion_cc_neuman_cons:   if(iopinneu2.eq.1) then
          if (neuman%numero .gt. 0) then
             bucle2: do j=1,3
                do i=1,neuman%numero
                   if (nra(j,k) .eq. neuman%referencias(i)) then
                      nov1 = mm(j,k)
                      nov2 = mm(1+mod(j,3),k)
                      x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
                      y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
                      x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
                      y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
                      delta = dsqrt((z(1,nov2) - z(1,nov1))**2 +&
                                    (z(2,nov2) - z(2,nov1))**2)
                      g1=neuman%valor(i)
                      g2=neuman%valor(i)
                      b(nov1) = b(nov1)+delta*(g1*xnod2 + g2*xnod1)*0.5d0
                      b(nov2) = b(nov2)+delta*(g1*xnod1 + g2*xnod2)*0.5d0
                      cycle bucle2 
                   end if
                enddo
             enddo bucle2
          end if
       endif opcion_cc_neuman_cons

    endif opcion_cc_neumann

 ! INTEGRATION OF CURVILINEAR CHARGES
    opcion_cargas_cur: if (iopcur .eq. 1) then
       doaristas: do  j=1,3
          iref = nra(j,k)
          if (iref.eq.0) then  
             cycle doaristas
          endif
          nov1 = mm(j,k)
          nov2 = mm(1+mod(j,3),k)
          x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
          y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
          x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
          y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2                      
          delta = dsqrt((z(1,nov2) - z(1,nov1))**2 +&
                        (z(2,nov2) - z(2,nov1))**2)
          if (iopincur.eq.1) then
             b(nov1) = b(nov1) + delta*&  
              (gcc(x1,y1,iref)*xnod2 + gcc(x2,y2,iref)*xnod1)*0.5d0
             b(nov2) = b(nov2) + delta *&
              (gcc(x1,y1,iref)*xnod1 + gcc(x2,y2,iref)*xnod2)*0.5d0
          else if (iopincur.eq.2) then
             b(nov1) = b(nov1) + delta *& 
              (gcct(iref)*xnod2 + gcct(iref)*xnod1)*0.5d0
             b(nov2) = b(nov2) + delta *&
              (gcct(iref)*xnod1 + gcct(iref)*xnod2)*0.5d0
          end if
       end do  doaristas
    end if opcion_cargas_cur

  enddo

! INCORPORATION OF PUNTUAL CHARGES  
  opcion_carga_pun:if (ioppun.eq.1) then
     do i=1,ncarpun
        call scarpun(xcarpun(i),ycarpun(i),itria,bar1,bar2,bar3)
        mm1=mm(1,itria)
        mm2=mm(2,itria)
        mm3=mm(3,itria)
        b(mm1) = b(mm1) + bar1 *  carpun(i)
        b(mm2) = b(mm2) + bar2 *  carpun(i)
        b(mm3) = b(mm3) + bar3 *  carpun(i)
     enddo
  end if opcion_carga_pun

end
