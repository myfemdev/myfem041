!-----------------------------------------------------------------------  
!     DATA READING DIRECT CURRENT 2D                 
!-----------------------------------------------------------------------  

subroutine endat( )

  use fich_electros
  use dcurrent_2D
  use conductividad
  use bloqueo
  use derivados
  
  implicit none
  integer :: i,j

  print*,'Reading data'

  read*,fichma
  read*,fichsol
  read*,fichElectricField
  read*,fichCurrentDensity

  read*,iopblo

  opcion_bloqueo: if (iopblo.eq.1) then

     read*,iopblo1
     read*,iopblo2
     read*,iopblo3

     entrada_por_funcion: if (iopblo1.eq.1) then

        read*,nrd
        if (nrd.gt.0)then
           read*,(irefd(i),i=1,nrd)
           read*,DirBC_Func
        endif

     endif entrada_por_funcion

     entrada_por_constantes:	if (iopblo2.eq.1) then

       read*,blofron%numero
       if (blofron%numero.gt.0) then
	      do i=1,blofron%numero
	         read*,blofron%referencias(i)
	         read*,blofron%valor(i)
	      enddo
       endif

     endif entrada_por_constantes

     entrada_por_puntos: if (iopblo3.eq.1) then

        read*,blopun%numero
        if (blopun%numero.gt.0) then
           do i=1,blopun%numero
    	      read*,blopun%referencias(i)
              read*,blopun%valor(i)
           enddo
        endif
     endif entrada_por_puntos
 
  endif opcion_bloqueo

  read*,iopneu
  opcion_neuman:  if (iopneu.eq.1) then
    
     read*,iopinneu1
     read*,iopinneu2

     entrada_por_funcion_neuman: if (iopinneu1.eq.1) then

        read*,nrn
    
        if (nrn.gt.0)then
           read*,(irefn(i),i=1,nrn)
           read*,NeuBC_Func
        endif

     endif entrada_por_funcion_neuman

     entrada_por_constantes_neuman: if (iopinneu2.eq.1) then

        read*,neuman%numero
        if (neuman%numero.gt.0) then
	       do i=1,neuman%numero
	          read*,neuman%referencias(i)
	          read*,neuman%valor(i)
	       enddo
        endif

     endif entrada_por_constantes_neuman

  endif opcion_neuman

  read*,iopint
  opcion_intensidad:  if (iopint.eq.1) then
     
     read*,iopint2

     entrada_por_constantes_intensidad:	if (iopint2.eq.1) then

        read*,inten%numero
        if (inten%numero.gt.0) then
	       do i=1,inten%numero
              read*,inten%referencias(i)
              read*,inten%valor(i)
              read*, inten%thickness(i)
	       enddo
        endif

     endif entrada_por_constantes_intensidad

  endif opcion_intensidad    

  read*,iop
  read*,iopf

  iopteta=0

  read*,conduc%numero
  do i=1,conduc%numero
     read*,conduc%referencias(i)
	 read*,conduc%iopcond(i)
	 if (conduc%iopcond(i).eq.2) then
	    read*,conduc%valorx(i), conduc%valory(i)
	 elseif(conduc%iopcond(i).eq.3)then 
	    iopteta=1 
	    read*,conduc%ntab(i)
	    do j=1,conduc%ntab(i)
           read*,conduc%teta(i,j),conduc%valtabx(i,j),conduc%valtaby(i,j)
	    enddo
	 elseif(conduc%iopcond(i).eq.1)then 
	    read*,conduc%etiqueta(i)
	 endif
  enddo
	
  if (iopteta.eq.1) then	
     read*,fichteta
  endif

  iopej=1

! CONTOUR CONDITIONS TYPE CHECKING

  if (iopblo.eq.1.and.(nrd.gt.0.or.blofron%numero.gt.0)) then
     ichneu=0
  else
     ichneu=1
  endif      

  return

end
