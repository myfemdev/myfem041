!--------------------------------------------------------------------
!                   CHOL SUBROUTINE 
!--------------------------------------------------------------------
!      GOAL : cholseky factorization of a symmetric matrix given
!             in it's vectorial form (sky-line method)
!--------------------------------------------------------------------
!   parameters :
!    input     n     matrix dimension in normal form
!              mua   pointers array (sky-line metod)
!              c     vector that contains the matrix 
!    output    c     matricial vector factorized
!--------------------------------------------------------------------

subroutine chol(n,mua,c)

  implicit none
  integer,          intent(in)    :: mua(*)
  integer,          intent(in)    :: n
  double precision, intent(inout) :: c(*)
  integer                         :: lii,lsi,kcj,lskcj,nfkcj,nsum,i1,kl
  integer                         :: i,j,k

  do i=2,n
     lii=mua(i)+1
     lsi=mua(i+1)
     bucle: do j=lii,lsi
        kcj=i-lsi+j
        lskcj=mua(kcj+1)
        nfkcj=lskcj-mua(kcj)
        nsum=min(j-lii,nfkcj-1)
        if(nsum.gt.0d0)then
           do k=1,nsum
              i1=kcj+1-k
              kl=mua(i1)
              c(j)=c(j)-c(j-k)*c(lskcj-k)*c(kl)
           enddo
        endif
        if((j-lsi).ne.0d0)then 
           c(j)=c(j)/c(lskcj)
        else
           cycle bucle
        endif
     enddo bucle
  enddo 

  return
end
      
