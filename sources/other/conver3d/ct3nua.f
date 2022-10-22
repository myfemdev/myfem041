c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine ct3nua (mf,nua,k,nbf)
*--------------------------------------------------------------------
*     but : construction du tableau nua des elements adjacents
*            a un element
*--------------------------------------------------------------------
*     in :
*        mf      tableau des faces des elements
*        nbfe=4  nb de face d'un l'element
*        nel     nombre d'elements
*        k       tableau de travail dimensionner a 2*nbf
*        nbf     nb de faces de la geometrie
*
*      out:
*        nua (i,it) = numero de l'element adjacent a la ieme face de
*                      l'element it (si c'est la frontiere,on a 0)
*--------------------------------------------------------------------
      include 'fcommon'
      integer nbf,mf(4,*),nua(4,*),k(2,*)
      integer i,ik,ifa,lta,ita
*
      do 10 i=1,nbf
       k(1,i)=0
10     k(2,i)=0
      do 20 ik=1,nel
       do 20 lta=1,4
        ifa=mf(lta,ik)
        ita=k(1,ifa)
        if(ita.eq.0) then
          nua(lta,ik)=0
          k(1,ifa)=ik
          k(2,ifa)=lta
        else
          nua(k(2,ifa),ita)=ik
          nua(lta,ik)=ita
        end if
20     continue
*
       return
       end
