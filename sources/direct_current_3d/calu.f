      subroutine calu(phi,u,mmorig)
***********************************************************************
* GOAL:   Calculamos la velocidad como u=grad(phi) de forma que este
*         tablero que se cree sirva como condicion inicial para la
*         aerodinamica.
*****
* IN:     phi -----> tablero con la funcion phi
*
* OUT:    u -------> velocidad
*********************************************************************** 
!      include 'commons'
      
      use malla_3DP1
      use electros3D, only: ivb
      
      implicit double precision(a-h,o-z)
      
      dimension u(3,*),phi(*), mmorig(4,*)
      
      allocate(ivb(nver),stat=ierror)
      if (ierror.ne.0) then
      print*,'no se ha podido reservar memoria para ivb en calu',nver
      stop
      endif
      
*----------------------------------------------------------------------
* Primero se calcula la velocidad en cada elemento de la forma:
*       Bk-t * [D^P^] * (phi)k
*
      do 1 k=1,nel
        phi1=phi(mmorig(1,k))-phi(mmorig(4,k))
        phi2=phi(mmorig(2,k))-phi(mmorig(4,k))
        phi3=phi(mmorig(3,k))-phi(mmorig(4,k))
        u(1,nver+k)=binv(1,1,k)*phi1+binv(2,1,k)*phi2+binv(3,1,k)*phi3
        u(2,nver+k)=binv(1,2,k)*phi1+binv(2,2,k)*phi2+binv(3,2,k)*phi3
        u(3,nver+k)=binv(1,3,k)*phi1+binv(2,3,k)*phi2+binv(3,3,k)*phi3
 1    continue

* Ahora hay que calcular las velocidades en los vertices de la malla a
* partir del dato de las velocidades en cada uno de los elementos. Para
* ello en cada vertice se hace un promedio de los valores en los
* elementos en los que esta ese vertice.
* Es por ello que debo introducir un contador para saber en cuantos
* elementos esta cada uno de los vertices.
      do 2 i=1,nver
        ivb(i)=0
        do 10 j=1,3
          u(j,i)=0d0
 10     continue
 2    continue

      do 3 k=1,nel
        do 4 i=1,4
          nv=mmorig(i,k)
          ivb(nv)=ivb(nv)+1
          do 5 ir=1,3
            u(ir,nv)=u(ir,nv)+u(ir,nver+k)
 5        continue
 4      continue
 3    continue
	
* Ahora solo falta dividir la suma que hice en cada vertice por el
* numero de veces que conte ese vertice
      do 6 i=1,nver
        do 7 j=1,3
          u(j,i)=u(j,i)/ivb(i)
 7      continue
 6    continue
*----------------------------------------------------------------------
      return
      end
