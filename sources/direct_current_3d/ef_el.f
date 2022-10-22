      subroutine ef
***********************************************************************
* GOAL:   Calculamos el campo electrico e=-grad(sol) por elementos
*****
* IN:     sol -----> tablero con el potencial
*
* OUT:    e -------> campo electrico
*********************************************************************** 
      
      use malla_3DP1
      use electros3D
      
      implicit double precision(a-h,o-z)
*      Aloxamos as variables auxiliares
      allocate(e(3,nel),STAT=istat)
      if (istat.ne.0) stop 'Error al alojar e en ef'
      
      do 1 k=1,nel
        e(1,k)=0d0
        e(2,k)=0d0 
        e(3,k)=0d0
1     continue
      
      
      
*----------------------------------------------------------------------
* Primero se calcula la velocidad en cada elemento de la forma:
*       Bk-t * [D^P^] * (sol)k
*
      do 2 k=1,nel
        sol1=sol(mm(1,k))-sol(mm(4,k))
        sol2=sol(mm(2,k))-sol(mm(4,k))
        sol3=sol(mm(3,k))-sol(mm(4,k))
        e(1,k)=binv(1,1,k)*sol1+binv(2,1,k)*sol2+binv(3,1,k)*sol3
        e(2,k)=binv(1,2,k)*sol1+binv(2,2,k)*sol2+binv(3,2,k)*sol3
        e(3,k)=binv(1,3,k)*sol1+binv(2,3,k)*sol2+binv(3,3,k)*sol3
 2    continue

      do 3 k=1,nel
        e(1,k)=-e(1,k)
        e(2,k)=-e(2,k)
        e(3,k)=-e(3,k)
3     continue
*----------------------------------------------------------------------
      return
      end
