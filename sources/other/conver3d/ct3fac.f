c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine ct3fac (nf,nbf,ns,mf,lnfo)
*--------------------------------------------------------------------
*   in:   mm       tableau des tetraedres
*         nel      nombre de tetraedres
*         nver     nombre de sommets
*         lnf      nombre de mots du tableau nf (nf(lnf)
*
*   out:
*         nf       tableau des face
*                   nf(1:3,jf) les 3 sommets de la face jf
*
*         nbf      nombre de faces
*         lnf      nombre de mots
*         mf       tableau donnant les faces de l'element
*                   mf(1: 4,it)  les 4 faces  de element it
*--------------------------------------------------------------------
      include 'fcommon'
      integer nbf,lnf,nf(3,*),ns(*),mf(4,*)
      integer is,nbpp,llf,ik,jf,ip,ipp,isgp
      integer nf1(4),nf2(4),nf3(4),k(3)
      data nf1,nf2,nf3/2,1,1,1 ,3,4,2,3, 4,3,4,2/
*   ----------   Bloque de calculo  ----------
      lnf=lnfo
      do 10 is=1,nver
       ns(is)=0
10    continue
      nbpp=0
      llf=0
      do 100 ik=1,nel
       do 90 jf=1,4
        k(1)=mm(nf1(jf),ik)
        k(2)=mm(nf2(jf),ik)
        k(3)=mm(nf3(jf),ik)
        call ordsg3 (k,isgp)
        ip=ns(k(1))
        ipp=ip-1
c       recherche si la face existe deja
20      if(ip.le.0) goto 50
        if(ipp.ge.ip.or.ip.gt.nbpp) goto 20000
        if(k(2).ne.abs(nf(2,ip))) goto 30
        if(k(3).ne.nf(3,ip)) goto 30
        nf(3,ip)=-nf(3,ip)
        mf(jf,ik)=ip
        goto 90
30      continue
        ipp=ip
        ip=nf(1,ip)
        goto 20
50      continue
        nbpp=nbpp+1
        llf=llf+3
        if(llf.gt.lnf) goto 10000
        mf(jf,ik)=nbpp
        nf(1,nbpp)=0
        nf(2,nbpp)=sign(k(2),isgp)
        nf(3,nbpp)=k(3)
        if(ipp.eq.-1) then
         ns(k(1))=nbpp
        else
         nf(1,ipp)=nbpp
        end if
90    continue
100   continue
      nbf=nbpp
       lnf=llf
      do 200 is=1,nver
       ip=ns(is)
       ipp=ip-1
110     if(ip.eq.0) goto 200
        if(ipp.ge.ip.or.ip.gt.nbf) goto 20000
        ipp=ip
        ip=nf(1,ip)
        if(nf(2,ipp).lt.0) then
         nf(1,ipp)=-nf(2,ipp)
         nf(2,ipp)=is
        else
         nf(1,ipp)=is
        endif
        goto 110
200   continue
      return
*
10000 stop 'ct3fac: pas assez de place memoire'
20000 stop 'ct3fac: pointeur de cons des face ne sont pas croissant'
*
      end
