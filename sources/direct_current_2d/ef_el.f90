!------------------------------------------------------------------
! goal : computation of the electric field                     
!------------------------------------------------------------------
!                                                             
!   IN:  modules                                               
!                                                               
!                                                                
!   OUT: e (electric field=-grad V)                             
!                                                                 
!------------------------------------------------------------------
                   
subroutine ef()

  use malla_2DP1,  only : nel,mm,z
  use dcurrent_2D, only : e,sol
      
  implicit none
  double precision :: a(3)
  double precision :: g1,g2
  integer          :: istat,no1,no2,no3
  integer          :: k

  if(allocated(e))deallocate(e)
  allocate(e(2,nel),stat=istat)
  if (istat.ne.0) stop 'Error while allocating array e in ef'
      
  do k=1,nel
     e(1,k)=0d0
     e(2,k)=0d0 
  enddo

  do k=1,nel
     no1=mm(1,k)
     no2=mm(2,k)
     no3=mm(3,k)
     a(1)=sol(no1)
     a(2)=sol(no2)
     a(3)=sol(no3)
! COMPUTATION OF THE GRADIENT 
     call matelgr(z(1,no1),z(1,no2),z(1,no3),a,g1,g2)
! CONSTRUCTION OF THE ELECTRIC FIELD 
     e(1,k)=-g1
     e(2,k)=-g2
  enddo        

  return
end
