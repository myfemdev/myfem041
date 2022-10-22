      subroutine calivref(ivref,nref,iref)
***********************************************************************
* GOAL:    subrutina que calcula el tablero ivref que contiene para
*          cada referencia de la malla el numero de uno de sus
*          vertices.
******
* IN:     nref ----> numero de referencias en la malla
*         iref ----> indices de las referencias en la malla
*         nel -----> numero de elementos
*         nrv -----> numero de referencias de los vertices
*         mm ------> numeracion global de los vertices
*
* OUT:    ivref ---> vertice en cada referencia
***********************************************************************
      include 'fcommon'
      dimension ivref(*),iref(*)
*----------------------------------------------------------------------
* Inicializamos
      do 4 i=1,nref
        ivref(iref(i))=0
 4    continue

* buscamos un vertice en cada referencia
      do 3 n=1,nref
        do 1 k=1,nel
        do 1 i=1,4
          if (nrv(i,k).eq.iref(n)) then
            ivref(iref(n))=mm(i,k)
            goto 3
          endif
 1      continue
 3    continue

**** escribimos resultados por pantalla
!      print*,'ivref -----> '
!      do 2 i=1,nref
!        if (ivref(iref(i)).ne.0) then
!          print*,iref(i),ivref(iref(i))
!        endif
! 2    continue
*----------------------------------------------------------------------
      return
      end
