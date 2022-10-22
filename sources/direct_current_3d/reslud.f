      subroutine reslud(nivo,ntdl,mat4,mat5,ca,r,z)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c                    s.p. reslud
c                    -----------
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c but : ce sp descend le systeme triangulaire inferieur:
c -----                     (( l ca )) (z) = (r)
c       remonte le systeme triangulaire superieur:
c                           (( u ca )) (z) = (z)
c       version reelle double precision
c
c   parametres d'entree:
c   -------------------
c   nivo    : option du sous-programme
c             1 : descente seulement
c             2 : remontee seulement
c   ntdl    : ordre du systeme
c   mat4    : pointeur amat4 sur les coefficients diagonaux
c   mat5    : pointeur amat5 sur les colonnes
c   ca      : matrice de conditionnement stockee suivant la sd amat
c   r       : second membre
c
c    parametre de sortie:
c    -------------------
c   z       : vecteur solution
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c   programmeur : p.joly       labo analyse numerique paris mars 1982
c.......................................................................
      implicit double precision (a-h,o-z)
      dimension ca(*),z(*),r(*)
      dimension mat4(*),mat5(*)
      common /unites/ lecteu,imprim,filuni(30)
 1000 format('0erreur dans sp reslud : nivo =',i5,'  au lieu de 1 ou 2')
c
      if(nivo.eq.2) go to 50
      if(nivo.ne.1) go to 100
c
c
c     la descente  ( l ca )  (z) = (r)
c     ---------------------------------
c
            do 1 i=1,ntdl
            k1=mat4(i)+1
            k2=mat4(i+1)
            s= 0.d0
                do 2 k=k1,k2
                icol=mat5(k)
                if(icol-i) 20,22,22
   20           s=s +ca(k) * z(icol)
    2           continue
   22       z(i)=  r(i) - s
    1       continue
      return
c
c     la remontee   ( u ca )  (z) = (r)
c     ---------------------------------
c
c
   50 i = ntdl
           do 15 i0 = 1,ntdl
           k1 = mat4(i)+1
           k2 = mat4(i+1)
           s  = 0.d0
           do 16 k=k1,k2
           icol = mat5(k)
           if(icol-i) 16,16,17
   17      s = s +ca(k)*z(icol)
   16      continue
           z(i)= ( r(i) - s ) / ca(k2)
           i = i-1
   15      continue
c
      return
  100 write (imprim,1000) nivo
      stop
      end
