subroutine gauspp (a,n,np,eps,ires)

  implicit none
  integer,          intent(in)    :: n,np
  double precision, intent(in)    :: eps
  double precision, intent(inout) :: a(n,np)
  integer,          intent(out)   :: ires
  double precision, parameter     :: zero=1d-30
  double precision                :: v,epsr,u
  integer                         :: i,j,k,l,i0,k1,nm1,n1,i1

  v=0.
  do i=1,n
    do j=1,n
      v=max(v,abs(a(i,j)))
    enddo
  enddo
  if(v.le.zero) then 
    ires=1
    write (6,*) 'gauspp : there is a zero pivot (< eps) in a '
    return
  endif

  epsr=eps*v
  do k=1,n-1
    v=abs(a(k,k))
    i0=k
    k1=k+1
    do i=k1,n
      if(v.gt.abs(a(i,k))) cycle 
      v=abs(a(i,k))
      i0=i
    enddo
    if(v.lt.epsr) then 
      ires=1
      write (6,*) 'gauspp : there is a zero pivot (< eps) in a '
      return
    endif
    do j=k,np
      v=a(k,j)
      a(k,j)=a(i0,j)
      a(i0,j)=v
    enddo
    do i=k1,n
      v=a(i,k)/a(k,k)
      do j=k1,np
        a(i,j)=a(i,j)-a(k,j)*v
      enddo
    enddo
  enddo

  nm1=n-1
  u=a(n,n)
  n1=n+1
  do l=n1,np
    a(n,l)=a(n,l)/u
    do i=1,nm1
      i1=n-i+1
      v=a(n-i,l)
      do j=i1,n
        v=v-a(n-i,j)*a(j,l)
      enddo
      a(n-i,l)=v/a(n-i,n-i)
    enddo
  enddo

  ires=0
  
  return
end
