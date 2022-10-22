      subroutine lupt(md,ndim,ip,iq,a)
************************************************************
*   factorizacion LU (pivote total)
************************************************************
* entrada:
*     md    primera dimension de la matriz
*     ndim  orden del la matriz 
*     a     matriz
* salida:
*     ip,iq punteros  
*     a     factorizacion                  
************************************************************
      implicit double precision (a-h,o-z)
      complex*16 a,s
      dimension a(md,*),ip(*),iq(*)

* INICIALIZACION DE PUNTEROS
      do 10 i=1,ndim
        ip(i)=i
        iq(i)=i
10    continue
* CONTADOR DE CAMBIOS
      index=0

      do 1 k=1,ndim-1
* CALCULO DEL PIVOTE
        piv=0.d0
        ipiv=k
        jpiv=k
        do 11 i=k,ndim
          do 11 j=k,ndim
            if(dabs(piv).lt.cdabs(a(ip(i),iq(j)))) then
              piv=a(ip(i),iq(j))
              ipiv = i
              jpiv = j
            end if
 11     continue
        if(ipiv.ne.k) then 
          it=ip(k)
          ip(k)=ip(ipiv)
          ip(ipiv)=it
          index=index+1
        end if
        if(jpiv.ne.k) then 
          it=iq(k)
          iq(k)=iq(jpiv)
          iq(jpiv)=it
          index=index+1
        end if
        if(cdabs(a(ip(k),iq(k))).lt.1.d-16) then
           write(6,*) 'pivote nulo',a(ip(k),iq(k)), ' en etapa ',k
           write(6,*) 'pivote nulo',ip(k),iq(k), ' en etapa ',k
           stop
        end if

* ELIMINACION
        do 2 i=k+1,ndim
           s=a(ip(i),iq(k))/a(ip(k),iq(k))
           do 3 j=k+1,ndim
              a(ip(i),iq(j))=a(ip(i),iq(j))-s*a(ip(k),iq(j))
 3         continue
           a(ip(i),iq(k))=s
 2      continue
 1    continue

      if(cdabs(a(ip(ndim),iq(ndim))).lt.1.d-15) then
         write(6,*) 'pivote nulo',a(ip(ndim),iq(ndim)), 
     &              ' en etapa ',ndim 
         stop
      end if

      return
      end
