      subroutine cmalla(nver,z,xx,yy,zz,nx,ny,nz,ndx,ndy,
     &                  ipunt,ind)
      implicit double precision (a-h,o-z)
      parameter(eps=1d-3)
      dimension z(3,*),xx(*),yy(*),zz(*),ipunt(ndx,ndy,*),ind(3,*)
          xx(1)=z(1,1)
          yy(1)=z(2,1)
          zz(1)=z(3,1)
          nx=1
          ny=1
          nz=1

      do 1 i=2,nver
          do 2 jx=1,nx
              if(dabs(z(1,i)-xx(jx)).le.eps) go to 3
  2       continue
          nx=nx+1
          xx(nx)=z(1,i)
  3       continue
          do 4 jy=1,ny
              if(dabs(z(2,i)-yy(jy)).le.eps) go to 5
  4       continue
          ny=ny+1
          yy(ny)=z(2,i)
  5       continue
          do 6 jz=1,nz
              if(dabs(z(3,i)-zz(jz)).le.eps) go to 7
  6       continue
          nz=nz+1
          zz(nz)=z(3,i)
  7       continue
  1   continue

c     print*,'nx,ny,nz : ',nx,ny,nz

      call ordena (nx,xx)
      call ordena (ny,yy)
      call ordena (nz,zz)

      do 8 i=1,nx
        do 8 j=1,ny
          do 8 k=1,nz
            do 9 l=1,nver
              if(dabs(xx(i)-z(1,l))+dabs(yy(j)-z(2,l))+
     &          dabs(zz(k)-z(3,l)).lt.eps) then

                  ipunt(i,j,k)=l

                  ind(1,l)=i
                  ind(2,l)=j
                  ind(3,l)=k

              go to 8
              end if
  9         continue
  8   continue

      return
      end
