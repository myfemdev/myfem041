      subroutine blomamorp(xmat,nrefb,irefb,tablo,sm)
******************************************************************
* Analoga a blomamor pero en vez de la funcion de bloqueo se le pasa
* un tablero para bloquear en puntos
* GOAL:   fichero que bloquea los grados de libertad de la matriz
*         morse con punteros ifil de la ecuacion de la aerodinamica
*         en unas referencias dadas por nrefb e irefb, pero con la
*         particularidad de que los segundos miembros se bloquean
*         a cero y por tanto no hay que restar nada al segundo
*         miembro por poner ceros en las columnas de la matriz.
****
* IN:    xmat -----> matriz en forma morse no bloqueada
*        nrefb ----> numero de referencias en las que se bloquea
*        irefb ----> valores de las referencias en las que se bloquea
*
* OUT:   xmat -----> matriz en forma morse bloqueada
******************************************************************
      use bloqueo
      use malla_3DP1
      use electros3D
     
      implicit double precision (a-h,o-z)
      dimension xmat(*),irefb(*),sm(*),tablo(*)
*-----------------------------------------------------------------

      
      

* Modifico la fila
      do 1 i=1,nrefb
         iv=irefb(i)
         do 3 l=ib(iv)+1,ib(iv+1)-1
           xmat(l)=0d0
 3       continue
         xmat(ib(iv+1))=1d0
 1    continue

* Hago ceros en las columnas
      do 2 i=1,nver
       do 6 i1=ib(i)+1,ib(i+1)-1
        icol=jb(i1)
        do 4 j=1,nrefb
          if (icol.eq.irefb(j)) then
            sm(i)=sm(i)-xmat(i1)*tablo(j)
            xmat(i1)=0d0
            goto 6
          endif
 4      continue
 6     continue
 2    continue
*-----------------------------------------------------------------
      return
      end
