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

subroutine baric(xp,itetr,bxp,nel,mm,z)

  implicit none 
  double precision, intent(in)  :: xp(*),z(3,*)
  integer,          intent(in)  :: mm(4,*)
  integer,          intent(in)  :: nel
  integer,          intent(out) :: itetr
  double precision, intent(out) :: bxp(*)
  double precision              :: a(3,4)
  double precision, parameter   :: epsil=1d-6
  double precision              :: suma,xkmin,xkmax,ykmin,ykmax,zkmin,zkmax
  integer                       :: ires
  integer                       :: k,l,i

  bucle1: do k=1,nel
     xkmin=min(z(1,mm(1,k)),z(1,mm(2,k)),z(1,mm(3,k)),z(1,mm(4,k)))
     xkmax=max(z(1,mm(1,k)),z(1,mm(2,k)),z(1,mm(3,k)),z(1,mm(4,k)))
     ykmin=min(z(2,mm(1,k)),z(2,mm(2,k)),z(2,mm(3,k)),z(2,mm(4,k)))
     ykmax=max(z(2,mm(1,k)),z(2,mm(2,k)),z(2,mm(3,k)),z(2,mm(4,k)))
     zkmin=min(z(3,mm(1,k)),z(3,mm(2,k)),z(3,mm(3,k)),z(3,mm(4,k)))
     zkmax=max(z(3,mm(1,k)),z(3,mm(2,k)),z(3,mm(3,k)),z(3,mm(4,k)))
     if (xp(1).lt.xkmin-epsil.or.xp(1).gt.xkmax+epsil.or.&
         xp(2).lt.ykmin-epsil.or.xp(2).gt.ykmax+epsil.or.&
         xp(3).lt.zkmin-epsil.or.xp(3).gt.zkmax+epsil) then
        cycle bucle1
     endif
     do l=1,3
        do i=1,3
           a(l,i)=z(l,mm(i,k))-z(l,mm(4,k))
        enddo
        a(l,4)=xp(l)-z(l,mm(4,k))
     enddo
     call gauspp(a,3,4,1d-6,ires)
     if (ires.eq.1) then
        stop 'Null pivot in gauspp of baric'
     endif
     suma=0d0
     do l=1,3
        if (a(l,4).lt.-epsil) then
           cycle bucle1
        else if (a(l,4).lt.epsil) then
           a(l,4)=0d0
        endif
        suma=suma+a(l,4)
     enddo
     if (suma.gt.1d0+epsil) then
        cycle bucle1
     else if (suma.gt.1d0-epsil) then
        suma=1d0
     endif
     itetr=k
     do l=1,3
        bxp(l)=a(l,4)
     enddo
     bxp(4)=1d0-suma

     return
  enddo bucle1

  print*
  print*,'Last xp',(xp(i),i=1,3)
  stop 'Error in baric'

end

