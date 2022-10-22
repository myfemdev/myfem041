      double precision function evalta(nn,xt,yt,x)
***** X****************************************************************X
*     funcion que evalua la tabla (xt,yt) para el material im y el tipo
*	de dato id en 'x'
*
*     materiales(10): 1: grafito, 2: pasta, 3: nipple, 4: virola, 
*     5: placas, 6: pasta liquida, 7: briquetas
*     datos(15): 1: cond. term., 2:res. elec., 3: cal. esp., 4: dens, 
*     5: er, 6: ez, 7:nur, 8: nuz, 9: gz, 10: alfr, 11: alfz
*     12: entalpia                                                     *
***** X****************************************************************X
      
	implicit double precision (a-h,o-z)
	dimension xt(*),yt(*)
c     comprobacion
      if (nn .lt. 2) stop 'La tabla no es valida'
c     extrapolacion a la izquierda
      if (x .le. xt(1)) then
        call punto1(xt(1),yt(1),xt(2),yt(2),x,y)
        evalta = y
        return
      endif
c     interpolacion
      do 1 ic=2,nn
        if (x .le. xt(ic)) then
          call punto1(xt(ic-1),yt(ic-1),xt(ic),
     &    yt(ic),x,y)
          evalta = y
          return
        endif
    1 continue
c     extrapolacion a la derecha
      call punto1(xt(nn-1),yt(nn-1),xt(nn),
     &yt(nn),x,y)
      evalta = y
      return

      end