      subroutine blomat(c,mua)
*
*     OBJETIVO: Bloquea una matriz no simetrica morse:
*
**********************************************************************
* 
*    IN:
*       commons
*    OUT:
*       c ... matriz bloqueada
**********************************************************************
      use dirichlet
	
      implicit double precision (a-h,o-z)
      dimension c(*),mua(*)

      if(nvdi.gt.0) then
        do 1 i=1,nvdi
           c(mua(nvd(i)+1))=1.d50
1       continue  
      endif

      return
      end
