!------------------------------------------------------------------------
!                            DATA INPUT                              
!------------------------------------------------------------------------

subroutine endat( )

  use fich_electros
  use electros_2D
  use cargavol
  use cargacur
  use cargapun
  use permitividad
  use bloqueo
  use derivados

  implicit none
  integer :: i,j

  print*,'Reading data...'

  read*,fichma
  read*,fichsol
  read*,fichElectricField

  read*,iopblo
  opcion_bloqueo: if (iopblo.eq.1) then

     read*,iopblo1
     read*,iopblo2
     read*,iopblo3
        
     entrada_por_funcion: if (iopblo1.eq.1) then
        read*,nrd
        if(nrd.gt.0)then
           read*,(irefd(i),i=1,nrd)
           read*,DirBC_Func
        endif
     endif entrada_por_funcion

     entrada_por_constantes: if (iopblo2.eq.1) then
        read*,blofron%numero
        if(blofron%numero.gt.0) then
           do i=1,blofron%numero
              read*,blofron%referencias(i)
              read*,blofron%valor(i)
           enddo
        endif
     endif entrada_por_constantes

     entrada_por_puntos: if (iopblo3.eq.1) then
        read*,blopun%numero
        if(blopun%numero.gt.0) then
           do i=1,blopun%numero
              read*,blopun%referencias(i)
              read*,blopun%valor(i)
           enddo
        endif
     endif  entrada_por_puntos

  endif  opcion_bloqueo

  read*,iopneu
  opcion_neuman:  if (iopneu.eq.1) then

     read*,iopinneu1
     read*,iopinneu2

     entrada_por_funcion_neuman: if (iopinneu1.eq.1) then
        read*,nrn
        if(nrn.gt.0)then
           read*,(irefn(i),i=1,nrn)
           read*,NeuBC_Func
        endif
     endif entrada_por_funcion_neuman

     entrada_por_constantes_neuman: if (iopinneu2.eq.1) then
        read*,neuman%numero
        if(neuman%numero.gt.0) then
           do i=1,neuman%numero
              read*,neuman%referencias(i)
              read*,neuman%valor(i)
           enddo
        endif
     endif entrada_por_constantes_neuman

  endif opcion_neuman

  read*,iop
  read*,iopf

  read*,iopvol
  if (iopvol.eq.1) then
     read*,iopinvol
     if (iopinvol.eq.2) then
        read*,carvol%numero
        if(carvol%numero.gt.0) then 
           do i=1,carvol%numero
              read*,carvol%referencias(i)
              read*,carvol%valor(i)
           enddo
        endif
     endif
  end if

  read*,iopcur
  if (iopcur.eq.1) then
     read*,iopincur
     if (iopincur.eq.2) then
        read*,carcur%numero
        do i=1,carcur%numero
           read*,carcur%referencias(i)
           read*,carcur%valor(i)
        enddo
     endif
  endif

  read*,ioppun
  if (ioppun.eq.1) then
     read*,ncarpun
     do i=1,ncarpun
        read*,xcarpun(i)
        read*,ycarpun(i)
        read*,carpun(i)
     enddo
  end if

  iopteta=0
  
  read*,permirel%numero
  do i=1,permirel%numero
     read*,permirel%referencias(i)
     read*,permirel%iopermir(i)
     if(permirel%iopermir(i).eq.2) then
        read*,permirel%valorx(i), permirel%valory(i)
     elseif(permirel%iopermir(i).eq.3)then 
        iopteta=1 
        read*,permirel%ntab(i)
        do j=1,permirel%ntab(i)
           read*,permirel%teta(i,j),permirel%valtabx(i,j),&
                 permirel%valtaby(i,j)
        enddo
     elseif(permirel%iopermir(i).eq.1)then 
        read*,permirel%etiqueta(i)
     endif
  enddo
  
  if (iopteta.eq.1) then
     read*,fichteta
  endif
      
  iopej=0
  
  ! CONTOUR CONDITIONS TYPE CHECKING
  if(iopblo.eq.1.and.(nrd.gt.0.or.blofron%numero.gt.0)) then
     ichneu=0
  else
     ichneu=1
  endif      
      
  return
end
