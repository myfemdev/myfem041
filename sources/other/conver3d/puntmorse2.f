      subroutine puntmorse2(nel,nver,mm,mua1,mua2,nnove,ivec,mnver)
* 
*    OBJETIVO: esta subrutina crea los tablemos mua1 y mua2
*              necesarios para el almacenamiento tipo MORSE
*              de una matriz no simetrica. Los nodos son los
*              vertices de la malla
**********************************************************************
* 
*    IN:
*       nel   ... numero de elementos de la malla
*       nver  ... numero de vertices de la malla
*       mm    ... numeracion global de los vertices
*    OUT:
*       nnove(i)  ... numero de nodos vecinos del nodo i-esimo
*       ivec(i,j) ... numero del j-esimo nodo vecino del nodo i-esimo
*       mua1(i)   ... lugar que ocupa el elemento de la diagonal
*                     principal
*       mua2(j)   ... columna que corresponde a la posicion j
*
**********************************************************************
      implicit double precision (a-h,o-z)
      dimension mm(4,*),mua1(*),mua2(*),nnove(*),ivec(mnver,*),mmn(4)
      
      mua1(1)=0
      do 1 i=1,nver 
        nnove(i)=0
        mua1(i+1)=0
1     continue

*
* creacion de los tableros:
*       nnove(i) --> numero de nodos vecinos del nodo i
*       ivec(i,j), j=1,...,nnove(i)  --> numero del j-esimo
*         vertice vecino del nodo i-esimo
*   hay que tener en cuenta que:
*       si el nodo esta en el interior del dominio, sus
*         nodos vecinos son los vertices "adyacentes" 
*        
      do 2 k=1,nel
        do 9 ii=1,4
          mmn(ii)=mm(ii,k)
  9     continue
        do 3 i=1,4
          do 3 j=1,4
            if(j.eq.i) go to 3
            do 4 jj=1,nnove(mmn(i))
              if(mmn(j).eq.ivec(mmn(i),jj)) go to 3
 4          continue
            nnove(mmn(i))=nnove(mmn(i))+1
            ivec(mmn(i),nnove(mmn(i)))=mmn(j)
 3      continue
 2    continue
*
*      se ordena ivec
*    
      do 5 i=1,nver
        do 5 j=1,nnove(i)-1
          do 5 jj=j+1,nnove(i)
            if(ivec(i,j).gt.ivec(i,jj)) then
              ik=ivec(i,jj)
              ivec(i,jj)=ivec(i,j)
              ivec(i,j)=ik
            endif
 5    continue  
*
*
*     creacion de mua1 y mua2
*       
      do 7 i=1,nver
        mua1(i+1)=mua1(i)+nnove(i)+1

        do 8 j=1,nnove(i)
          mua2(mua1(i)+j)=ivec(i,j)
8       continue

        mua2(mua1(i+1))=i
 7    continue

      return
      end
