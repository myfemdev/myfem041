      subroutine calit
******************************************************************
*  Parametros.
*    in:     datos de la malla que se incluyen en common
*    out:    it -->tablero que le asigna a cada vertice i un
*                  elemento it(i) al que pertenece.
*            bx -->tablero de coordenadas baricentricas de cada
*                  vertice respecto al elemento it asignado.
******************************************************************
      include 'fcommon'
*
      ki=1
      do 1 i=1,nver
        it(i)=0
        do 4 i1=1,4
 4        bx(i1,i)=0d0
 3      do 2 k=ki,nel
        do 2 j=1,4
          if (mm(j,k).eq.i) then
            it(i)=k
            bx(j,i)=1d0
            ki=k
            goto 1
          endif
 2      continue
        ki=1
        goto 3
 1    continue
*
      return
      end
