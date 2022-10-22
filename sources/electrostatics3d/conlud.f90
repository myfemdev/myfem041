!---------------------------------------------------------------------------
!                       conlud
!                      --------
!---------------------------------------------------------------------------
!   GOAL : computation of the conditioning matrix associated to the
!          incomplete gauss factorization A=LU or A=L+U with the same
!          structure than A
!
!   IN   : 
!     ntdl  -  system size
!     mat4  -  pointer amat4 to the diagonal coefficients
!     mat5  -  pointer amat5 to the columns
!     a     -  matrix stored in the subroutine amat
!
!   OUT  :
!     ca    -  conditioning matrix
!---------------------------------------------------------------------------

  subroutine conlud(ntdl,mat4,mat5,a,ca)

  implicit none
  double precision, intent(in)  :: a(*)
  integer,          intent(in)  :: mat4(*),mat5(*)
  double precision, intent(out) :: ca(*)
  integer                       :: ntdl,ncak,s
  double precision              :: cak
  integer                       :: i,j,k,k3,l3,n,k1,k2,mij,kj,j3,l1,l2

  cak = 1.d0
  ncak = 0
  n=mat5(ntdl+1)
  do i=1,n
     ca(i)=0.d0
  enddo
  bucle1: do i=1,ntdl
     k1=mat4(i)+1
     k2=mat4(i+1)
     bucle2: do k=k1,k2
        s=0.d0
        j=mat5(k)
        mij=min(i,j)
        kj=mat4(j+1)
        bucle3: do k3=k1,k2
           j3=mat5(k3)
           if (sign(1,(j3-mij))-1.eq.0) then
              if ((i-j).eq.0) then
                 ca(k)=a(k)-s
                 if (dabs(ca(k)).lt.1d-08) then
                    ncak=ncak+1
                    ca(k)=cak
                    cycle bucle1
                 else
                    cak=ca(k)
                    cycle bucle1
                 endif
              else if ((sign(1,(i-j))-1).eq.0) then
                 ca(k)=(a(k)-s)/ca(kj)
                 cycle bucle2
              else
                 ca(k)=a(k)-s
                 cycle bucle2
              endif                    
           else
              if(dabs(ca(k3)).lt.1.d-08) cycle bucle3
              l1=mat4(j3)+1
              l2=mat4(j3+1)
              bucle4: do l3=l1,l2
                 if (mat5(l3)-j.eq.0) then
                    s=s+ca(k3)*ca(l3)
                    cycle bucle3
                 else
                    cycle bucle4
                 endif
              enddo bucle4
            end if
         enddo bucle3
      enddo bucle2
  enddo bucle1

end
