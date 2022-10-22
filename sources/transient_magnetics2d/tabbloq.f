      subroutine tabbloq(nel,mm,nra,nrd,irefd,nvdi,nvd,nrvd,
     &	ndvdi,indblo)

      implicit double precision (a-h,o-z)
      dimension mm(3,*),nra(3,*),irefd(*),nvd(*),nrvd(*),indblo(*)


*********************************************************************
*            creacion del tablero de vertices dirichlet            * 
*********************************************************************
      nvdi=0
      if(nrd.gt.0) then
      do 14 i=1,nrd
        do 14 k=1,nel
          do 14 l=1,3
            if (nra(l,k).eq.irefd(i)) then
              ik1=l;ik2=mod(l,3)+1
              if(nvdi>1)then
                do 29 ll=1,nvdi
                  if(mm(ik1,k).eq.nvd(ll)) then 
                    go to 20 
                  endif
29              continue
              endif
              nvdi=nvdi+1
              nvd(nvdi)=mm(ik1,k)  
              nrvd(nvdi)=nra(l,k)
	        indblo(nvdi) = i
20            continue
              if(nvdi>1)then
                do 30 ll=1,nvdi
                  if(mm(ik2,k).eq.nvd(ll)) then 
                    go to 21 
                  endif
30              continue
              endif
              nvdi=nvdi+1
              nvd(nvdi)=mm(ik2,k)
              nrvd(nvdi)=nra(l,k)
	        indblo(nvdi) = i

21            continue
            endif  
14    continue 

      else
      
        nvdi=0
      
      endif
      
       

      return
      end
