!--------------------------------------------------------------------
!                    SOLS SUBROUTINE 
!--------------------------------------------------------------------
!     GOAL
!     ----     resolution of a l.s.e. where the matriz is factorized 
!              in a vectorial form (sky-line) by cholesky's metod 
!--------------------------------------------------------------------
!   parameters
!   ----------
!    input     c     vectorial matrix 
!              b     second member array
!              n     dimension of the second member
!              mua   pointers array (sky-line method)
!    output    b     solution array
!--------------------------------------------------------------------
!    programador    p. quintela , j. trastoy
!    -----------    universidad de santiago.spain
!                   (ene.1982)
!--------------------------------------------------------------------

subroutine sols(c,b,n,mua)

  implicit none
  double precision, intent(in)    :: c(*)
  integer, intent(in)             :: mua(*)
  integer, intent(in)             :: n
  double precision, intent(inout) :: b(*)
  integer                         :: i1,i2,i3,ik,il
  integer                         :: i,j,k

  do i=2,n
     i1=mua(i)+1
     i2=mua(i+1)
     i3=i2-i1
     if(i3.eq.0) cycle 
     do j=1,i3
        b(i)=b(i)-c(i2-j)*b(i-j)
     enddo
  enddo
  do k=1,n
     ik=mua(k+1)
     b(k)=b(k)/c(ik)
  enddo
  do k=n,2,-1
     i2=mua(k+1)
     i3=i2-mua(k)-1
     if(i3.eq.0) cycle 
     do il=1,i3
        b(k-il)=b(k-il)-c(i2-il)*b(k)
     enddo
  enddo
 
  return
end
