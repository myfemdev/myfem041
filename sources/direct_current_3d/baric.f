      subroutine baric(xp,itetr,bxp,nel,mm,z)
 
      
***************************************************************
* GOAL:    Calcula, a partir de las coordenadas del punto, el
*          tetraedro en el que esta y sus coordenadas
*          baricentricas. Es una alternativa a car13d.
***
* IN:    commons
*        xp(l), l=1,3 --> coordenadas del punto xp
*
* OUT:   itetr ---------> tetraedro en el que esta el punto xp
*        bxp(l), l=1,4 -> coordenadas baricentricas del punto
*                         xp con respecto al tetraedro itetr
***************************************************************

      implicit double precision (a-h,o-z)
      dimension xp(*),bxp(*),a(3,4),mm(4,*),z(3,*)
      data epsil/1d-6/
      do 2 k=1,nel
      xkmin=dmin1(z(1,mm(1,k)),z(1,mm(2,k)),z(1,mm(3,k)),z(1,mm(4,k)))
      xkmax=dmax1(z(1,mm(1,k)),z(1,mm(2,k)),z(1,mm(3,k)),z(1,mm(4,k)))
      ykmin=dmin1(z(2,mm(1,k)),z(2,mm(2,k)),z(2,mm(3,k)),z(2,mm(4,k)))
      ykmax=dmax1(z(2,mm(1,k)),z(2,mm(2,k)),z(2,mm(3,k)),z(2,mm(4,k)))
      zkmin=dmin1(z(3,mm(1,k)),z(3,mm(2,k)),z(3,mm(3,k)),z(3,mm(4,k)))
      zkmax=dmax1(z(3,mm(1,k)),z(3,mm(2,k)),z(3,mm(3,k)),z(3,mm(4,k)))
      if (xp(1).lt.xkmin-epsil.or.xp(1).gt.xkmax+epsil.or.
     &    xp(2).lt.ykmin-epsil.or.xp(2).gt.ykmax+epsil.or.
     &    xp(3).lt.zkmin-epsil.or.xp(3).gt.zkmax+epsil) then
        goto 2
      endif
      do 3 l=1,3
        do 4 i=1,3
          a(l,i)=z(l,mm(i,k))-z(l,mm(4,k))
4       continue
        a(l,4)=xp(l)-z(l,mm(4,k))
3     continue
      call gauspp(a,3,4,1d-6,ires)
      if (ires.eq.1) then
        stop 'pivote nulo en gauspp de baric'
      endif
      sum=0d0
      do 5 l=1,3
        if (a(l,4).lt.-epsil) then
          goto 2
        else if (a(l,4).lt.epsil) then
          a(l,4)=0d0
        endif
        sum=sum+a(l,4)
5     continue
      if (sum.gt.1d0+epsil) then
        goto 2
      else if (sum.gt.1d0-epsil) then
        sum=1d0
      endif
      itetr=k
      do 6 l=1,3
        bxp(l)=a(l,4)
6     continue
      bxp(4)=1d0-sum
*
      return
2     continue
      print*
      print*,'ultimo xp',(xp(i),i=1,3)
      stop 'en baric por salirse del dominio'
      end

