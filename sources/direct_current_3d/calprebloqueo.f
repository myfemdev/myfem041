!!!!!!! estas dos subrutinas son iguales excepto en los nombres de las variables nvreb[c,f] y ivreb[c,f], además de que solamente calprebloqueof usa xvrebf


      subroutine calprebloqueof (nref,iref)
*****************************************************************
* GOAL:     Calculos previos al bloqueo.
*           Calculamos los vertices que corresponden a las
*          diferentes partes de la frontera de la caldera para
*          facilitar el bloqueo posterior de los gdl.
* nref: numero de referencias Dirichlet a bloquear
* iref(i): numero de la i-esima referencia a bloquear
*****************************************************************
         use malla_3DP1
         use bloqueo
         
         implicit double precision (a-h,o-z)
         dimension iref(*)
         
*----------------------------------------------------------------
* inicializamos
      do 99 j=1,nref
        nvrebf(iref(j))=0
!        xvrebf(iref(j))=0 ! novo. innecesario
 99   continue
*
      do 1 k=1,nel
       do 1 i=1,4
* vemos que referencia tiene cada vertice
         nv=mm(i,k)
         do 2 n=1,nref
           if (nrvg(nv).eq.iref(n)) then
             ire=iref(n)
* una vez que sabemos la referencia que tiene comprobamos que
* no fue contabilizado
             do 3 m=1,nvrebf(ire)
               if (nv.eq.ivrebf(ire,m)) goto 1
 3           continue
!             xvrebf(ire) = n ! novo. innecesario
             nvrebf(ire)=nvrebf(ire)+1
             ivrebf(ire,nvrebf(ire))=nv
             goto 1
           endif
 2       continue
 1    continue
*----------------------------------------------------------------
      return
      end


      subroutine calprebloqueoc (nref,iref)
*****************************************************************
* GOAL:     Calculos previos al bloqueo.
*           Calculamos los vertices que corresponden a las
*          diferentes partes de la frontera de la caldera para
*          facilitar el bloqueo posterior de los gdl.
* nref: numero de referencias Dirichlet a bloquear
* iref(i): numero de la i-esima referencia a bloquear
*****************************************************************
         use malla_3DP1
         use bloqueo
         
         implicit double precision (a-h,o-z)
         dimension iref(*)
         
*----------------------------------------------------------------
* inicializamos
      do 99 j=1,nref
        nvrebc(iref(j))=0
 99   continue
*
      do 1 k=1,nel
       do 1 i=1,4
* vemos que referencia tiene cada vertice
         nv=mm(i,k)
         do 2 n=1,nref
           if (nrvg(nv).eq.iref(n)) then
             ire=iref(n)
* una vez que sabemos la referencia que tiene comprobamos que
* no fue contabilizado
             do 3 m=1,nvrebc(ire)
               if (nv.eq.ivrebc(ire,m)) goto 1
 3           continue
             nvrebc(ire)=nvrebc(ire)+1
             ivrebc(ire,nvrebc(ire))=nv
             goto 1
           endif
 2       continue
 1    continue
*----------------------------------------------------------------
      return
      end
