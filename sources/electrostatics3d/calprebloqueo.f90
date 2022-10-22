!-----------------------------------------------------------------
! GOAL:    Calculations before the strong imposition of the 
!          Dirichlet conditions. We calculate the vertices which
!          correspond to the differnt parts of the boundary.
! nref: number of Dirichlet boundaries
! iref(i): number of the i Dirichlet boundary
!-----------------------------------------------------------------

subroutine calprebloqueof (nref,iref)

  use malla_3DP1
  use bloqueo
         
  implicit none
  integer, intent(in) :: nref
  integer, intent(in) :: iref(*)
  integer             :: nv,ire
  integer             :: i,j,k,n,m 
         
  do j=1,nref
     nvrebf(iref(j))=0
  enddo

  do k=1,nel
     bucle1: do i=1,4
        nv=mm(i,k)
        do n=1,nref
           if (nrvg(nv).eq.iref(n)) then
              ire=iref(n)
              do m=1,nvrebf(ire)
                 if (nv.eq.ivrebf(ire,m)) cycle bucle1
              enddo      
              nvrebf(ire)=nvrebf(ire)+1
              ivrebf(ire,nvrebf(ire))=nv
              cycle bucle1
           endif
        enddo
     enddo bucle1
  enddo

  return
end


!-----------------------------------------------------------------
! GOAL:    Calculations before the strong imposition of the 
!          Dirichlet conditions. We calculate the vertices which
!          correspond to the differnt parts of the boundary.
! nref: number of Dirichlet boundaries
! iref(i): number of the i Dirichlet boundary
!-----------------------------------------------------------------

subroutine calprebloqueoc (nref,iref)

  use malla_3DP1
  use bloqueo
         
  implicit none
  integer, intent(in) :: nref
  integer, intent(in) :: iref(*)
  integer             :: nv,ire
  integer             :: j,k,i,n,m
         
  do j=1,nref
     nvrebc(iref(j))=0
  enddo

  do k=1,nel
     bucle2: do i=1,4
        nv=mm(i,k)
        do n=1,nref
           if (nrvg(nv).eq.iref(n)) then
              ire=iref(n)
              do m=1,nvrebc(ire)
                 if (nv.eq.ivrebc(ire,m)) cycle bucle2 
              enddo 
              nvrebc(ire)=nvrebc(ire)+1
              ivrebc(ire,nvrebc(ire))=nv
              cycle bucle2 
           endif
        enddo 
     enddo bucle2
  enddo  

  return
end
