      module module_conver3d_source
      !-----------------------------------------------------------------------
      ! module to create auxiliary array for the mesh (old source code)
      !
      ! Last update: 17/10/2011
      ! Programmers: fran.pena@usc.es
      !
      ! conver: constructs
      !    det(nel), determinant
      !    binv(3,3,nel), inverse
      !    ib(nver+1), row  storage pointer (Morse)
      !    jb(nver), column storage pointer (Morse)
      !-----------------------------------------------------------------------
      include 'fcommon'

      contains

      subroutine conver()
*     ******************************************************************
*     GOAL:  Convierte la nopo de modulef 3D de tetraedos de forma
*        que pueda ser leida por los programas de simulacion
*     ******************************************************************
      parameter (lnf=2000000,mnref=100,mnvv=400)
*     include 'fcommon'
      character*300 malladat, mallasal
      dimension mf(4,mnel),kk(2,lnf),nf(3,lnf),ns(mnver)
      dimension ivref(mnref),iref(mnref)
      dimension ivec(mnver,mnvv),nnove(mnver)
*     ******************************************************************
*                     lectura de la malla
*     ******************************************************************
*
*           primero leemos el nombre del fichero en donde estan
*          los datos de la malla reconvertidos
*
c      if (command_argument_count() == 0) then
c          print*,'  --------------      DATOS      --------------  '
c          print*,'                      -----                      '
c         print*,'fichero de datos de la malla (entrada)'
c          read(5,'(a)') malladat
c         print*,'fichero de datos de la malla (salida)'
c          read(5,'(a)') mallasal
c      else
c          call readxml(malladat)
c          mallasal = 'mesh-temp-3d.bin'
c      end if

*     Leemos unos parametros necesarios de la malla para calcular 
*     nead, it y bx
c      call lee(malladat)
c      print*,'nel',nel
c      print*,'nver',nver
      if (nel.gt.mnel.or.mnver.gt.mnver) then
        print*,'Insufficient fcommon dimensions:'
        print*,'nel: ', nel, ', maximum: ',mnel
        print*,'nver: ', nver, ', maximum: ',mnver
        stop 1
      endif

*     Calculamos el tablero nead
*        call nelad3(mf,kk,nf,ns,lnf)

*     Calculamos los tableros it y bx
*        call calit

*     Calculamos los tableros det y binv
      call matbinv

*     Calculamos los punteros ib, jb y nemm
*        call puntmori
*     Los calculo aqui de forma mas eficiente
      call puntmorse2(nel,nver,mm,ib,jb,nnove,ivec,mnver)
      nemm=ib(nver+1)
      print*,'nemm',nemm
      if (nemm.gt.mbm) then
        print*,'Dimensiones de fcommon incorrectas'
        print*,'nemm < mbm => ',nemm,mbm
        stop
      endif

*     Escribimos esos tableros en el fichero malladat
c      open(76,file=mallasal,form='unformatted',status='unknown')
c      write(76) nel,nver,nemm
c      write(76) ((mm(i,j),i=1,4),j=1,nel),
c     &          ((nrc(i,j),i=1,4),j=1,nel),
c     &          ((nra(i,j),i=1,6),j=1,nel),
c     &          ((nrv(i,j),i=1,4),j=1,nel),
c     &          ((z(i,j),i=1,3),j=1,nver),
c     &          (det(k),k=1,nel),
c     &          (((binv(i,j,k),i=1,3),j=1,3),k=1,nel),
c     &          (ib(i),i=1,nver+1),(jb(i),i=1,nemm)
c      write(76) (nsd(k),k=1,nel)
c      close(76)

*     Calculamos un vertice en cada referencia
c      nref=100
c      do 13 i=1,nref
c        iref(i)=i
c 13   continue
c      call calivref(ivref,nref,iref)

c      ncf=4*((nx-1)*(ny-1)+(nx-1)*(nz-1)+(ny-1)*(nz-1))
c      nvf=nx*nz*2+ny*nz*2+nx*ny
c      nsp=4*nemm+25*nver+nel+100
c      npts=1300
c      nref=100
*      print*,'ncf -> ',ncf
*      print*
*      print*,'MEMORIA REQUERIDA : ',8*(6*nemm+nemm/2+
*     &                              18*nel+12*nel/2+60*nver+6*nver/2+
*     &                              nref*nvf/2+1*ncf+14*ncf/2+
*     &                              nsp)+250000
*     ---------------------------------------------------------------
c      stop 'conver END'
      end subroutine
      end module
