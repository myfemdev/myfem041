      subroutine conlud(ntdl,mat4,mat5,a,ca)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c                    s.p. conlud
c                    -----------
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c but : ce sp calcule la  matrice de conditionnement
c ----  associee a la factorisation de gauss incomplete
c       a = l.u  ou l+u a la meme structure que a
 
c
c   parametres d'entree:
c   -------------------
c   ntdl    : ordre du systeme
c   mat4    : pointeur amat4 sur les coefficients diagonaux
c   mat5    : pointeur amat5 sur les colonnes
c   a       : matrice stockee suivant la sd amat
c
c    parametre de sortie:
c    -------------------
c   ca      : matrice   de conditionnement
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c   programmeur : p.joly       labo analyse numerique paris mars 1982
c.......................................................................
      implicit double precision (a-h,o-z)
      dimension a(*),ca(*)
      dimension mat4(*),mat5(*)
      common /unites/ lecteu,imprim,filuni(30)
      common/trava1/nx(28),lonree,impre,ifill(3)
 1000 format(' sp conlud :',i6,'eme coefficient diagonal nul',g15.6)
 1001 format(' sp conlud :',i6,' coefficients diagonaux sur',i6,
     &' sont nuls')
c
      cak = 1.d0
      ncak = 0
      n=mat5(ntdl+1)
      do 10 i=1,n
   10 ca(i)=0.d0
      do 1 i=1,ntdl
      k1=mat4(i)+1
      k2=mat4(i+1)
      do 2 k=k1,k2
      s=0.d0
      j=mat5(k)
      mij=min0(i,j)
      kj=mat4(j+1)
      do 3 k3=k1,k2
      j3=mat5(k3)
      if(j3-mij) 9,11,11
    9 if(dabs(ca(k3)).lt.1.d-08) go to 3
      l1=mat4(j3)+1
      l2=mat4(j3+1)
      do 4 l3=l1,l2
      if(mat5(l3)-j) 4,5,4
    5 s=s+ca(k3)*ca(l3)
      go to 3
    4 continue
    3 continue
   11 if(i-j) 8,7,6
c                        calcul de l(i,j)
    6 ca(k)=(a(k)-s)/ca(kj)
      go to 2
c                        calcul de u(i,i)
    7 ca(k)=a(k)-s
      if(dabs(ca(k)).lt.1.d-08) go to 12
      cak=ca(k)
      go to 1
   12 if(impre.ge.7) write (imprim,1000) i,ca(k)
      ncak=ncak+1
      ca(k)=cak
      go to 1
c                        calcul de u(i,j)
    8 ca(k)=a(k)-s
    2 continue
    1 continue
      if(ncak.gt.0.and.impre.ge.5) write (imprim,1001) ncak,ntdl
      end
