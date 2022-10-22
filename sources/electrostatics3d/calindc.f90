!------------------------------------------------------------------
! GOAL:   definition of the numbers of the vertices that belong
!         to each face taking into account the Modulef order
!------------------------------------------------------------------
! OUT:  indc(1,i) -> array that contains the 3 numbers (of 4 
!                    possibilities) corresponding to the vertices
!                    belonging to the face i
!       inda(i,j) -> vertex i of the edge j, i=1,2, j=1,6
! For each tetrahedron, this are fixed numbers that are obtained
! form Modulef when doing the reconv
!------------------------------------------------------------------

subroutine calindc(indc,inda)

  implicit none 
  integer, intent(out) :: indc(3,*),inda(2,*)

  indc(1,1)=1
  indc(2,1)=3
  indc(3,1)=2
  indc(1,2)=1
  indc(2,2)=4
  indc(3,2)=3
  indc(1,3)=1
  indc(2,3)=2
  indc(3,3)=4
  indc(1,4)=2
  indc(2,4)=3
  indc(3,4)=4
      
! EDGES 
  inda(1,1)=1
  inda(2,1)=2
  inda(1,2)=2
  inda(2,2)=3
  inda(1,3)=3
  inda(2,3)=1
  inda(1,4)=1
  inda(2,4)=4
  inda(1,5)=2
  inda(2,5)=4
  inda(1,6)=3
  inda(2,6)=4      

  return
end
