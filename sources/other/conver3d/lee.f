      subroutine lee(malladat)
      include 'fcommon'
      character*(*) malladat  
*--------------------------------------------------------------------
*                     lectura de la malla
*
! malla con formato
      open(75,file=malladat,form='formatted')
      rewind(75)
      read(75,*) nel,nnod, nver
      read(75,*) ((mm(i,j),i=1,4),j=1,nel),
     &         ((nrc(i,j),i=1,4),j=1,nel),
     &         ((nra(i,j),i=1,6),j=1,nel),
     &         ((nrv(i,j),i=1,4),j=1,nel),
     &         ((z(i,j),i=1,3),j=1,nver)
      read(75,*) (nsd(k),k=1,nel)
      close(75)

! malla sin formato
!      open(75,file=malladat,form='unformatted',status='old')
!      rewind(75)
!      read(75) nel,nnod, nver
!      read(75) ((mm(i,j),i=1,4),j=1,nel),
!     &         ((nrc(i,j),i=1,4),j=1,nel),
!     &         ((nra(i,j),i=1,6),j=1,nel),
!     &         ((nrv(i,j),i=1,4),j=1,nel),
!     &         ((z(i,j),i=1,3),j=1,nver)
!      read(75) (nsd(k),k=1,nel)
!      close(75)

*
!      open(75,file='malla_formateada',form='formatted')
!      rewind(75)
!      write(75,*) nel,nnod, nver
!      write(75,*) ((mm(i,j),i=1,4),j=1,nel),
!     &         ((nrc(i,j),i=1,4),j=1,nel),
!     &         ((nra(i,j),i=1,6),j=1,nel),
!     &         ((nrv(i,j),i=1,4),j=1,nel),
!     &         ((z(i,j),i=1,3),j=1,nver)
!      write(75,*) (nsd(k),k=1,nel)
!      close(75)
*--------------------------------------------------------------------
      return
      end
