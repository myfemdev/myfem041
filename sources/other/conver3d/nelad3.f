      subroutine nelad3(mf,k,nf,ns,lnf)
*************************************************************************
*                         subprograma  nelad3
*                         -------------------
*************************************************************************
*   Objetivo
*   --------     Calculo de los tetraedros adyacentes a cada tetraedro
*                en una malla de dimension 3
*************************************************************************
*   Parametros
*   ----------
*      Common     nel       numero de elementos de la malla
*                 nver      numero de vertices de la malla
*                 mm        numeros de los vertices de cada tetraedro
*      Salida     nead=nua  tetraedros adyacentes a cada tetraedro
*      Internos   mf        tablero de numeracion de las caras
*                 k         tablero de trabajo dimensionado a 2*nbf
*                 nbf       numero de caras
*                 nf        tablero de los vertices de cada cara
*                 lnf       numero maximo de elementos de nf
*************************************************************************
      include 'fcommon'
      dimension mf(4,*),k(2,*),nf(3,*),ns(*)
***
* Calculamos una numeracion de las caras mf
***
      call ct3fac(nf,nbf,ns,mf,lnf)
      print*,'nbf ',nbf
***
* Calculamos los tetraedros adyacentes nead
***
      call ct3nua(mf,nead,k,nbf)
      return
      end
