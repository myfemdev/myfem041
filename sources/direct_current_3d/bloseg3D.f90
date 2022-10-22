      subroutine bloseg3D(b,nrb,irb)
      
  ! bloqueo del segundo miembro via funcion
  ! b: segundo miembro
  ! nrb: numero de referencias a bloquear
  ! irb: valor de las referencias a bloquear

      use bloqueo
      use malla_3DP1
    

      implicit none
      integer      :: i, n, nrb,j,iv,nr
      integer, INTENT(IN)  :: irb(*)
      real(DOUBLE), INTENT (INOUT) :: b(*)
      
     do 1 i=1,nrb
       do 1 j=1,nvrebf(irb(i))
         iv=ivrebf(irb(i),j)
         nr=nrvg(iv)
         b(iv)=h(z(1,iv),z(2,iv),z(3,iv),nr,i)
 1    continue
       
 
      return
      end
