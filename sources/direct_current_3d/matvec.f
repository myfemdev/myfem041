	subroutine matvec(alpha,x,beta,y,xmat,n,mua1,mua2)
**********************************************************************
* Calculo del producto:
*                          y=beta*y+alpha*xmat*x
**********************************************************************
        implicit double precision (a-h,o-z)

	dimension x(*),y(*),xmat(*)
        dimension mua1(*),mua2(*)

        if (beta.ne.1.d0) then
 	  if (beta.ne.0.d0) then
	    do 1 i=1,n
	      y(i)=beta*y(i)
1	    continue
	  else
	    do 2 i=1,n
	      y(i)=0.d0
2	    continue
	  endif
        endif
	
	if (alpha.ne.0.d0) then
	  if (alpha.eq.1.d0) then
	    call procb1(n,xmat,mua1,mua2,x,y)
	  else	   
	    call procb2(n,xmat,mua1,mua2,alpha,x,y)
	  endif
	endif

	return
	end
