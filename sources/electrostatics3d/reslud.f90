!-----------------------------------------------------------------------
!                     reslud
!                    --------
!-----------------------------------------------------------------------
! but : ce sp descend le systeme triangulaire inferieur:
! -----                     (( l ca )) (z) = (r)
!       remonte le systeme triangulaire superieur:
!                           (( u ca )) (z) = (z)
!
! INPUT:
!  
!   nivo    : option of the subroutine 
!             1 : descent only
!             2 : ascent only
!   ntdl    : system's order
!   mat4    : pointer amat4 to the diagonal coefficients
!   mat5    : pointer amat5 to the columns
!   ca      : matrice de conditionnement stockee suivant la sd amat
!   r       : second member 
!
! OUTPUT:
! 
!   z       : solution vector
!-----------------------------------------------------------------------

subroutine reslud(nivo,ntdl,mat4,mat5,ca,r,z)

  implicit none 
  integer,          intent(in)  :: nivo,ntdl
  integer,          intent(in)  :: mat4(*),mat5(*)
  double precision, intent(in)  :: r(*),ca(*)
  double precision, intent(out) :: z(*)
  integer                       :: k1,k2,icol
  double precision              :: s
  integer                       :: i,k,i0

  if (nivo.eq.1) then
! DESCENT  ( l ca )  (z) = (r)
     do i=1,ntdl
        k1=mat4(i)+1
        k2=mat4(i+1)
        s= 0.d0
        bucle2: do k=k1,k2
           icol=mat5(k)
           if ((sign(1,icol-i)-1).eq.0)then
              z(i)=r(i)-s
              exit bucle2
           else
              s=s+ca(k)*z(icol)
           endif
        end do bucle2
     enddo    
    
     return

  elseif(nivo.eq.2)then
! ASCENT   ( u ca )  (z) = (r)
     i = ntdl
     do i0 = 1,ntdl
        k1 = mat4(i)+1
        k2 = mat4(i+1)
        s  = 0.d0
        bucle16: do k=k1,k2
           icol = mat5(k)
           if ((sign(1,i-icol)-1).eq.0)then
              cycle bucle16
           else
              s=s+ca(k)*z(icol)
           endif
        enddo bucle16
        z(i)=(r(i)-s)/ca(k2)
        i=i-1
     enddo

     return
  
  else

     write (*,*)'Error in reslud: nivo= ',nivo
     stop

  endif

end
