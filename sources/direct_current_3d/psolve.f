	subroutine psolve(x,b,xmat,n,mua1,mua2)
        implicit double precision (a-h,o-z)

	dimension x(*),b(*),xmat(*)
        dimension mua1(*),mua2(*)

        call reslud(1,n,mua1,mua2,xmat,b,x)
        call reslud(2,n,mua1,mua2,xmat,x,x)

	return
	end
