!-------------------------------------------------------------------
! GOAL:    Calculus, with the point coordinates, of the tetrahedron
!          where the point is and it's barycentric coordinates.       
!          It's an alternative to car13d.                     
!-------------------------------------------------------------------
! IN:    commons
!        xp(l), l=1,2 --> coordinates of the point xp 
!
! OUT:   itetr ---------> tetrahedron where the point xp is 
!        bxp(l), l=1,3 -> barycentric coordinates of the point
!                         xp with respecto to the itetr tetrahedron
!-------------------------------------------------------------------

subroutine baric(xp,itetr,bxp,nel,mmtot,ztot)

  implicit none
  double precision, intent(in)  :: xp(*),ztot(2,*)
  integer,          intent(in)  :: mmtot(3,*)
  integer,          intent(in)  :: nel
  integer,          intent(out) :: itetr
  double precision, intent(out) :: bxp(*)
  double precision              :: a(2,3),xkmin,xkmax,ykmin,ykmax,suma
  integer                       :: ires
  double precision, parameter   :: epsil=1d-6
  integer                       :: k,l,i

  bucle: do k=1,nel
     xkmin=min(ztot(1,mmtot(1,k)),ztot(1,mmtot(2,k)),&
      ztot(1,mmtot(3,k)))
     xkmax=max(ztot(1,mmtot(1,k)),ztot(1,mmtot(2,k)),&
      ztot(1,mmtot(3,k)))
     ykmin=min(ztot(2,mmtot(1,k)),ztot(2,mmtot(2,k)),&
      ztot(2,mmtot(3,k)))
     ykmax=max(ztot(2,mmtot(1,k)),ztot(2,mmtot(2,k)),&
      ztot(2,mmtot(3,k)))
     if (xp(1).lt.xkmin-epsil.or.xp(1).gt.xkmax+epsil.or.&
         xp(2).lt.ykmin-epsil.or.xp(2).gt.ykmax+epsil) cycle bucle 
     do l=1,2
        do i=1,2
           a(l,i)=ztot(l,mmtot(i,k))-ztot(l,mmtot(3,k))
        enddo
        a(l,3)=xp(l)-ztot(l,mmtot(3,k))
     enddo
     call gauspp(a,2,3,1d-6,ires)
     if (ires.eq.1) then
        stop 'Null pivot in gauspp of baric'
     endif
     suma=0d0
     do l=1,2
        if (a(l,3).lt.-epsil) then
           cycle bucle 
        else if (a(l,3).lt.epsil) then
           a(l,3)=0d0
        endif
        suma=suma+a(l,3)
     enddo
   
     if (suma.gt.1d0+epsil) then
        cycle bucle 
     else if (suma.gt.1d0-epsil) then
        suma=1d0
     endif
     itetr=k
     do l=1,2
        bxp(l)=a(l,3)
     enddo
     bxp(3)=1d0-suma
     return
  enddo bucle

  print*
  print*,'Last xp',(xp(i),i=1,2)
  stop 'Error in baric'

end

