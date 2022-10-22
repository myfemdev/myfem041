!-----------------------------------------------------------------------------
! This subroutine calculates:
!      number of edges associated to the curvilinear charge references
!             and it's nodes
!      number of faces associated to the superficial charge references
!             and it's nodes
! output variable : naristas,nod1,nod2
!                   ncaras,nodc1,nodc2,nodc3
!-----------------------------------------------------------------------------

subroutine arint3D ()

  use  malla_3DP1,            only :nel, mm, nra, nrc,nelem, ensd, nsd
  use  cargacur             
  use  cargasup
  use  parametros_electros3D, only : ndar
  use  electros3D,            only : inda, indc
  use  auxiliar_cargas
  use  derivados3D
        
  implicit none
  integer :: nref
  integer :: nov1,nov2,nov3
  integer :: i,j,k,ii

  if(allocated(nelem))deallocate(nelem)
  allocate(nelem(carvol%numero),stat=ierror)
  if (ierror.ne.0) then
    print*,'Error while allocating array nelem'
    stop 1
  endif
  if(allocated(ensd))deallocate(ensd)
  allocate(ensd(carvol%numero,nel),stat=ierror)
  if (ierror.ne.0) then
    print*,'Error while allocating array ensd'
    stop 1
  endif
        
  nelem = 0; ensd = 0
  if (carvol%numero > 0) then
     do k=1,nel
        do i=1,carvol%numero
           if (carvol%referencias(i) == nsd(k)) then
              ensd(i,nelem(i)+1) = k
              nelem(i) = nelem(i) + 1
              exit
           endif
        enddo
     enddo
  endif  

  if (carcur%numero.gt.0) then
     nod1(1,1)=0
     nod2(1,1)=0
     donumcar: do i= 1,carcur%numero
        naristas(i) = 0
        do k=1,nel
           doaristas: do  j=1,6
              nref=nra(j,k)
              nov1=mm(inda(1,j),k)
              nov2=mm(inda(2,j),k)
              if (nref.eq.carcur%referencias(i)) then
                 do ii=1, naristas(i)
                    if ((nod1(i,ii).eq.nov1.and.nod2(i,ii).eq.nov2)&
                    .or.(nod1(i,ii).eq.nov2.and.nod2(i,ii).eq.nov1)) then
! IN THIS CASE THE EDGE IS ALREDY COUNTED
                       cycle doaristas
                    endif
                 enddo
                 naristas(i) = naristas(i) + 1
                 nod1(i,naristas(i)) = nov1
                 nod2(i,naristas(i)) = nov2
              endif
           enddo doaristas
        enddo 
        if (naristas(i).eq.0) then
           print*,'ATENTION: there is no edge associated with the reference', carcur%referencias
        endif
     enddo donumcar
  endif
      
  if (carsup%numero.gt.0) then
     nodc1(1,1)=0
     nodc2(1,1)=0
     nodc3(1,1)=0
     donumcarsup: do i= 1,carsup%numero
        ncaras(i)   = 0
        do k=1,nel
           docaras: do  j=1,4
              nref=nrc(j,k)
              nov1=mm(indc(1,j),k)
              nov2=mm(indc(2,j),k)
              nov3=mm(indc(3,j),k)
              if (nref.eq.carsup%referencias(i)) then
                 do ii=1, ncaras(i)
                    if ((nodc1(i,ii).eq.nov1.and.&
                         nodc2(i,ii).eq.nov2.and.&
                         nodc3(i,ii).eq.nov3).or.&
                        (nodc1(i,ii).eq.nov2.and.&
                         nodc2(i,ii).eq.nov3.and.&
                         nodc3(i,ii).eq.nov1).or.&
                        (nodc1(i,ii).eq.nov3.and.&
                         nodc2(i,ii).eq.nov1.and.&
                         nodc3(i,ii).eq.nov2).or.&
                        (nodc1(i,ii).eq.nov3.and.&
                         nodc2(i,ii).eq.nov2.and.&
                         nodc3(i,ii).eq.nov1).or.&
                        (nodc1(i,ii).eq.nov2.and.&
                         nodc2(i,ii).eq.nov1.and.&
                         nodc3(i,ii).eq.nov3).or.&
                        (nodc1(i,ii).eq.nov1.and.&
                         nodc2(i,ii).eq.nov3.and.&
                         nodc3(i,ii).eq.nov2) ) then
! IN THIS CASE THE EDGE IS ALREDY COUNTED
                        cycle docaras
                     endif
                  enddo
                  ncaras(i) = ncaras(i) + 1
                  nodc1(i,ncaras(i)) = nov1
                  nodc2(i,ncaras(i)) = nov2
                  nodc3(i,ncaras(i)) = nov3
               endif
            enddo docaras
         enddo 
         if (ncaras(i).eq.0) then
            print*,'ATENTION: there is no face associated with the refence', carsup%referencias(i)
         endif
      enddo donumcarsup
   endif

end
