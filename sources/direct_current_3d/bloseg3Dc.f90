      subroutine bloseg3Dc(b,nrb,irb,tablo)
      
  ! bloqueo del segundo miembro via tablero interfaz
  ! b: segundo miembro
  ! nrb: numero de referencias a bloquear
  ! irb: valor de las referencias a bloquear

      use bloqueo
      use malla_3DP1

      implicit none
      integer      :: i, n, nrb,j,iv
      integer,  INTENT(IN)  :: irb(*)
      real(DOUBLE),  INTENT (INOUT) :: b(*)
      real(DOUBLE),  INTENT (IN)    :: tablo(*)
      
      
      do 1 i=1,nrb
       do 1 j=1,nvrebc(irb(i))
         iv=ivrebc(irb(i),j)
         b(iv)=tablo(i)
 1    continue

!        do 1 i=1,nver
!        do 2 n=1,nrb
!          if (nrvg(i).eq.irb(n)) then
!             b(i)=tablo(n)
!             goto 1
!          endif
! 2      continue
! 1    continue


      return
      end
