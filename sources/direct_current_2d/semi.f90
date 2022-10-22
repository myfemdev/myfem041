!---------------------------------------------------------------------------
!      SECOND MEMBER CONSTRUCTION
!---------------------------------------------------------------------------

subroutine semi()
      
  use malla_2DP1
  use dcurrent_2D
  use external_electros
  use neumann
  use derivados
  use parametros_electros

  implicit none
  double precision           :: xnod11,xnod22,xnod1,xnod2,delta,g1,g2,gcons
  double precision           :: ab,bc,cd,de,det,xbar,ybar,xint,x1,y1,x2,y2
  double precision           :: x12,x23,x31,y12,y23,y31,xint12,xint23,xint31
  integer                    :: nov1,nov2,mm1,mm2,mm3,iref
  double precision, external :: f
  integer                    :: i,j,k
      
  if (iopf .eq. 1) then
     xnod11 = -0.577350269189626
     xnod22 =  0.577350269189626
  else if (iopf .eq. 2) then
     xnod11 = -1.
     xnod22 =  1.
  end if
  xnod1 = (1. + xnod11) * 0.5D0
  xnod2 = (1. + xnod22) * 0.5D0
 
  do i=1,nver
     b(i) = 0.D0
  end do

  do i=1,ndri
     areafi(i) =0.d0 
  end do

  if (iopint.eq.1) then           
     do k=1,nel
        do j=1,3
           nov1 = mm(j,k)
           nov2 = mm(1+mod(j,3),k)
           delta = dsqrt((z(1,nov2)-z(1,nov1))**2+(z(2,nov2)-z(2,nov1))**2)
           if (iopint2.eq.1) then
              do i=1,inten%numero
                 if (nra(j,k).eq.inten%referencias(i)) then
                    areafi(i)=areafi(i)+ delta
                 end if
              end do
           end if
        end do
     end do
  end if

  do k=1,nel

     mm1 = mm(1,k)
     mm2 = mm(2,k)
     mm3 = mm(3,k)

     ab = z(1,mm2) - z(1,mm1)
     bc = z(2,mm2) - z(2,mm1)
     cd = z(1,mm3) - z(1,mm1)
     de = z(2,mm3) - z(2,mm1)

     det = ab*de - bc*cd

     if (iop .eq. 1) then

        xbar = (z(1,mm1) + z(1,mm2) + z(1,mm3)) / 3.D0
        ybar = (z(2,mm1) + z(2,mm2) + z(2,mm3)) / 3.D0

        xint = f(xbar,ybar,nsd(k)) * det / 6.D0

        b(mm1) = b(mm1) + xint
        b(mm2) = b(mm2) + xint
        b(mm3) = b(mm3) + xint

     else if (iop .eq. 2) then

        b(mm1) = b(mm1) + f(z(1,mm1),z(2,mm1),nsd(k))*det / 6.D0
        b(mm2) = b(mm2) + f(z(1,mm2),z(2,mm2),nsd(k))*det / 6.D0
        b(mm3) = b(mm3) + f(z(1,mm3),z(2,mm3),nsd(k))*det / 6.D0

     else
     
        x12 = (z(1,mm1) + z(1,mm2)) * 0.5D0
        x23 = (z(1,mm2) + z(1,mm3)) * 0.5D0
        x31 = (z(1,mm3) + z(1,mm1)) * 0.5D0
        y12 = (z(2,mm1) + z(2,mm2)) * 0.5D0
        y23 = (z(2,mm2) + z(2,mm3)) * 0.5D0
        y31 = (z(2,mm3) + z(2,mm1)) * 0.5D0

        xint12 = (f(x12,y12,nsd(k))) * det 
        xint23 = (f(x23,y23,nsd(k))) * det
        xint31 = (f(x31,y31,nsd(k))) * det

        b(mm1) = b(mm1) + (xint12 + xint31) / 12D0
        b(mm2) = b(mm2) + (xint12 + xint23) / 12D0 
        b(mm3) = b(mm3) + (xint23 + xint31) / 12D0 

     end if

     opcion_cc_neumann: if(iopneu.eq.1) then

        opcion_cc_neuman_fun:   if(iopinneu1.eq.1) then

           if (nrn .gt. 0) then
              bucle: do j=1,3
                 do i=1,nrn
                    if (nra(j,k) .eq. irefn(i)) then
                       iref = irefn(i)
       
                       nov1 = mm(j,k)
                       nov2 = mm(1+mod(j,3),k)
                    
                       x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
                       y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
                       x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
                       y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2
      
                       delta = dsqrt((z(1,nov2)-z(1,nov1))**2 +(z(2,nov2)-z(2,nov1))**2)

                       b(nov1) = b(nov1)+delta*(g(x1,y1,iref)*xnod2+g(x2,y2,iref)*xnod1)*0.5d0
                       b(nov2) = b(nov2)+delta*(g(x1,y1,iref)*xnod1+g(x2,y2,iref)*xnod2)*0.5d0

                       cycle bucle
                    end if
                 end do
              end do bucle
           end if
        endif opcion_cc_neuman_fun

        opcion_cc_neuman_cons: if(iopinneu2.eq.1) then

           if (neuman%numero .gt. 0) then
              bucle2: do j=1,3
                 do i=1,neuman%numero
                    if (nra(j,k) .eq. neuman%referencias(i)) then
                       iref = neuman%referencias(i)
       
                       nov1 = mm(j,k)
                       nov2 = mm(1+mod(j,3),k)
                    
                       x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
                       y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
                       x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
                       y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2

                       delta = dsqrt((z(1,nov2)-z(1,nov1))**2+(z(2,nov2)-z(2,nov1))**2)
                       g1=neuman%valor(i)
	                   g2=neuman%valor(i)

                       b(nov1) = b(nov1) + delta * (g1*xnod2 + g2*xnod1)*0.5d0
                       b(nov2) = b(nov2) + delta * (g1*xnod1 + g2*xnod2)*0.5d0

                       cycle bucle2
                    end if
                 end do
              end do bucle2
           end if

        endif opcion_cc_neuman_cons

     endif opcion_cc_neumann


!---------------------------------------------------------------------------
! NEUMANN BC WITH INTENSITY AS GIVEN DATA
!---------------------------------------------------------------------------

     opcion_cc_intensidad: if(iopint.eq.1) then

        opcion_cc_intensidad_cons: if(iopint2.eq.1) then

           if (inten%numero .gt. 0) then
              bucle3: do j=1,3
                 do i=1,inten%numero
                    if (nra(j,k) .eq. inten%referencias(i)) then
                       iref = inten%referencias(i)
               
                       nov1 = mm(j,k)
                       nov2 = mm(1+mod(j,3),k)
                   
                       x1 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod1
                       y1 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod1
                       x2 = z(1,nov1) + (z(1,nov2)-z(1,nov1)) * xnod2
                       y2 = z(2,nov1) + (z(2,nov2)-z(2,nov1)) * xnod2

                       delta = dsqrt((z(1,nov2)-z(1,nov1))**2+(z(2,nov2)-z(2,nov1))**2)

                       gcons=inten%valor(i)

                       b(nov1) = b(nov1) + delta * (gcons*xnod2 + gcons*xnod1)*0.5d0/(areafi(i)*inten%thickness(i))
                       b(nov2) = b(nov2) + delta * (gcons*xnod1 + gcons*xnod2)*0.5d0/(areafi(i)*inten%thickness(i))

                       cycle bucle3
                    end if
                 end do
              end do bucle3
           end if

        endif opcion_cc_intensidad_cons

     endif opcion_cc_intensidad     

  end do 


end
