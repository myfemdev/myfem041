subroutine segvar(nel,nver,z,mm,p,bvar,ndnolin,idnolin,nsd)

implicit none  !alfredo
integer nel,nver,mm(3,*),k, nsd(*),ndnolin,idnolin(*),i,mm1,mm2,mm3 !alfredo
double precision p(2,*),bvar(*),z(2,*),ab,bc,cd,de   !alfredo

do k=1,nel
    do i=1, ndnolin
        if (nsd(k).eq.idnolin(i)) then
	
	            mm1=mm(1,k)
	            mm2=mm(2,k)
	            mm3=mm(3,k)
	
	            ab = z(1,mm2) - z(1,mm1)
                bc = z(2,mm2) - z(2,mm1)
                cd = z(1,mm3) - z(1,mm1)
                de = z(2,mm3) - z(2,mm1)

!	            bvar(mm1)=bvar(mm1)-(1.d0/2.d0)*(p(1,k)*(bc-de)+p(2,k)*(cd-ab))
!	            bvar(mm2)=bvar(mm2)-(1.d0/2.d0)*(p(1,k)*de-p(2,k)*cd)
!	            bvar(mm3)=bvar(mm3)-(1.d0/2.d0)*(-bc*p(1,k)+ab*p(2,k))
	            
	            bvar(mm1)=bvar(mm1)-(1.d0/2.d0)*(-p(2,k)*(bc-de)+p(1,k)*(cd-ab))
	            bvar(mm2)=bvar(mm2)-(1.d0/2.d0)*(-p(2,k)*de-p(1,k)*cd)
	            bvar(mm3)=bvar(mm3)-(1.d0/2.d0)*(bc*p(2,k)+ab*p(1,k))
	            exit
	     end if
	enddo
enddo

return
end
