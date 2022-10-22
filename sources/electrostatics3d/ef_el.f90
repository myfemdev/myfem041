!------------------------------------------------------------------
! goal : computation of the electric field                     
!------------------------------------------------------------------
!                                                             
!   IN:  modules                                               
!                                                         
!   OUT: e (electric field=-grad sol)                             
!                                                                 
!------------------------------------------------------------------

subroutine ef

  use malla_3DP1, only : nel,mm,binv
  use electros3D, only : e,sol
      
  implicit none

  double precision :: sol1,sol2,sol3
  integer          :: istat
  integer          :: k

  if(allocated(e))deallocate(e)
  allocate(e(3,nel),stat=istat)
  if (istat.ne.0) stop 'Error while allocating e in ef' 
      
  do k=1,nel
    e(1,k)=0d0
    e(2,k)=0d0 
    e(3,k)=0d0
  enddo    
     
! FIRST WE COMPUTATE THE VELOCITY IN EACH ELEMENT:
!       Bk-t * [D^P^] * (sol)k

  do k=1,nel
    sol1=sol(mm(1,k))-sol(mm(4,k))
    sol2=sol(mm(2,k))-sol(mm(4,k))
    sol3=sol(mm(3,k))-sol(mm(4,k))
    e(1,k)=binv(1,1,k)*sol1+binv(2,1,k)*sol2+binv(3,1,k)*sol3
    e(2,k)=binv(1,2,k)*sol1+binv(2,2,k)*sol2+binv(3,2,k)*sol3
    e(3,k)=binv(1,3,k)*sol1+binv(2,3,k)*sol2+binv(3,3,k)*sol3
  enddo

  do k=1,nel
    e(1,k)=-e(1,k)
    e(2,k)=-e(2,k)
    e(3,k)=-e(3,k)
  enddo

  return
end
