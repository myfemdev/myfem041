!------------------------------------------------------------------------
!                            DATA INPUT                              
!------------------------------------------------------------------------

  subroutine endat3D( )

  use fich_electros3D
  use parametros_electros3D
  use electros3D
  use cargavol
  use cargacur
  use cargapun
  use permitividad
  use bloqueo
  use derivados3D
  use auxiliar_cargas

  implicit none

  integer :: temp       ! Dirichlte/Neumann : number of references per function
                        ! Volumic charges   : number of charged domains per function
  integer :: fnum       ! function numbers: Dirichlet (1 to 7) function(1) == User defined
                        !                   Neumann   (1 or 3) function(1) == User defined
  integer :: temp1      ! input via function for the volumic charge (1 Y 0 N)
  integer :: temp2      ! input via array for the volumic charge (1 Y 0 N)
  integer :: temp3      ! input via file for the volumic charge (1 Y 0 N)
  integer :: i,j

  dir%fun = 1
  neu%fun = 1
  vol%fun = 1
  sup%fun = 1
  cur%fun = 1

  nrn             = 0
  neuman%numero   = 0 
  nrd             = 0
  blofron%numero  = 0 
  blopun%numero   = 0 
  carvol%numero   = 0
  carsup%numero   = 0
  carcur%numero   = 0
  ncarpun         = 0 
  permirel%numero = 0 

  read*,fichma
  read*,fichsol
  read*,fichgradsol

  print*,'Strong imposition of Dirichlet conditions option (1 Yes 0 No)'
  read*,iopblo
  print*,iopblo
  
  opcion_bloqueo: if (iopblo.eq.1) then
  
     print*, 'Strong imposition of Dirichlet conditions'
     print*, 'Input via function (1 Yes 0 No)'
     read*,iopblo1
     print*,iopblo1
     print*, 'Strong imposition of Dirichlet conditions'
     print*, 'Input via constants in the boundaries (1 Yes 0 No)'
     read*,iopblo2
     print*,iopblo2
     print*, 'Strong imposition of Dirichlet conditions'
     print*, 'Input via punctual blocking (1 Yes 0 No)'
     read*,iopblo3
     print*,iopblo3

     entrada_por_funcion: if (iopblo1.eq.1) then
        print*, 'Strong imposition of Dirichlet conditions'
        print*, 'Input via function'
        do while (.TRUE.)
           print *, 'Number of references'
           read*,temp      
           print*,temp
           if (temp <= 0) exit
           print *, 'References'
           read*,(irefd(i),i=nrd+1,nrd+temp)
           print*,(irefd(i),i=nrd+1,nrd+temp)
           print *, 'Function associated to the references'
           read*, fnum
           print*,fnum
           dir%fun(nrd+1:nrd+temp) = fnum
           if (fnum<1 .or. fnum>7) stop 'incorrect function number'
           nrd = nrd + temp
        enddo
     endif entrada_por_funcion

     entrada_por_constantes: if (iopblo2.eq.1) then
        print*, 'Strong imposition of Dirichlet conditions'
        print*, 'Input via constants in the boundaries'
        print*, 'Number of references'
        read*,blofron%numero
        print*,blofron%numero
        if(blofron%numero.gt.0) then
           print*,'References and values'
           do i=1,blofron%numero
              read*,blofron%referencias(i)
              read*,blofron%valor(i)
           enddo
           print*,(blofron%referencias(i),i=1,blofron%numero)
           print*,(blofron%valor(i),i=1,blofron%numero)
        endif
     endif entrada_por_constantes

     entrada_por_puntos: if (iopblo3.eq.1) then
        print*, 'Strong imposition of Dirichlet conditions'
        print*, 'Input via punctual blocking'
        print*, 'Number of points'
        read*,blopun%numero
        print*,blopun%numero
        if (blopun%numero.gt.0) then
           print*,'References and values'
           do i=1,blopun%numero
              read*,blopun%referencias(i)
              read*,blopun%valor(i)
           enddo
           print*,(blopun%referencias(i),i=1,blopun%numero)
           print*,(blopun%valor(i),i=1,blopun%numero)
        endif
     endif  entrada_por_puntos

  endif  opcion_bloqueo

  print*, 'Neumann references option (1 Yes 0 No)'
  read*,iopneu
  print*,iopneu
  
  opcion_neuman:  if (iopneu.eq.1) then
  
     print*, 'Neumann references option'
     print*, 'Input via function (1 Yes 0 No)'
     read*,iopneu1
     print*,iopneu1
     print*, 'Neumann references option'
     print*, 'Input via constants in the boundaries (1 Yes 0 No)'
     read*,iopneu2
     print*,iopneu2

     entrada_por_funcion_neuman: if (iopneu1.eq.1) then
        print*, 'Neumann references option'
        print*, 'Input via function'
        do while (.TRUE.)
           print *, 'Number of references'
           read*,temp
           print *, temp
           if (temp <= 0) exit
           print *, 'References'
           read*,(irefn(i),i=nrn+1,nrn+temp)
           print*,(irefn(i),i=nrn+1,nrn+temp)
           print *, 'Function associated to the references'
           read*, fnum
           neu%fun(nrn+1:nrn+temp) = fnum
           if (fnum/=1 .and. fnum/=3) stop 'incorrect function number'
           print*,fnum,functions(fnum)
           nrn = nrn + temp
        enddo
     endif entrada_por_funcion_neuman

     entrada_por_constantes_neuman: if (iopneu2.eq.1) then
        print*, 'Neumann references option'
        print*, 'Input via constants in the boundaries'
        print*, 'Number of references'
        read*,neuman%numero
        print*,neuman%numero
        if (neuman%numero.gt.0) then
           print *, 'References and values'
           do i=1,neuman%numero
              read*,neuman%referencias(i)
              read*,neuman%valor(i)
           enddo
        endif
        print*,(neuman%referencias(i),i=1,neuman%numero)
        print*,(neuman%valor(i),i=1,neuman%numero)
     endif entrada_por_constantes_neuman

  endif  opcion_neuman

  print *, 'Quadrature formula for matrix and second member option'
  read*,iop
  print*,iop
  print *, 'Quadrature formula for the boundary terms'
  read*,iopf
  print*,iopf

  print *, 'Volumic charge option (1 Yes 0 No)'
  read*,iopvol
  print*,iopvol
  
  if (iopvol.eq.1) then
     print*, 'Volumic charge option'
     print*, 'Input via function (1 Yes 0 No)'
     read*, temp1
     print *, temp1
     print*, 'Volumic charge option'
     print*, 'Input via constants in the domains (1 Yes 0 No)'
     read*, temp2
     print *, temp2

     if (temp1.eq.1) then
        do while (.TRUE.)
           print *, 'Number of references'
           read*,temp
           print *, temp
           if (temp <= 0) exit   
           print*,'Domain references'
           read*,(carvol%referencias(i), i=carvol%numero+1,carvol%numero+temp)
           print*,(carvol%referencias(i), i=carvol%numero+1,carvol%numero+temp)
           carvol%valor(carvol%numero+1:carvol%numero+temp)=0.d0 
           carvol%constante(carvol%numero+1:carvol%numero+temp)=.FALSE.
           print*,'Type one of the function numbers below:'
           print*,1,': ',functions(1)
           print*,2,': ',functions(2)
           read*, fnum
           vol%fun(carvol%numero+1:carvol%numero+temp) = fnum
           if (fnum/=1 .and. fnum/=2) stop 'incorrect function number'
           print*,fnum,functions(fnum)
           carvol%numero = carvol%numero + temp
        enddo
     endif

     if (temp2.eq.1) then
        print*,'Number of charges by constant'
        read*,temp
        print*,temp
        if (temp.gt.0) then 
           do i=1,temp
              print*,'Domain references'
              read*,carvol%referencias(carvol%numero+i)
              print*,carvol%referencias(carvol%numero+i)
              print*,'Charge associated to the domain'
              read*,carvol%valor(carvol%numero+i)
              print*,carvol%valor(carvol%numero+i)
              carvol%constante(carvol%numero+i) = .TRUE.
           enddo
        carvol%numero = carvol%numero + temp
        endif
     end if
     
  end if

  print*,'Superficial charge option (1 Yes 0 No)'
  read*,iopsup
  print*,iopsup
  
  if (iopsup.eq.1) then
  
     print*,'Superficial charge option'
     print*, 'Input via function (1 Yes 0 No)'
     read*,temp1
     print*,temp1
     print*,'Superficial charge option'
     print*, 'Input via constants in the surfaces (1 Yes 0 No)'
     read*,temp2
     print*,temp2

     if (temp1.eq.1) then
        do while (.TRUE.)
           print*,'Number of surfaces'
           read*,temp
           print *, temp
           if (temp <= 0) exit   
           print*,'Surface references'
           read*,(carsup%referencias(i),i=carsup%numero+1,carsup%numero+temp)
           print*,(carsup%referencias(i),i=carsup%numero+1,carsup%numero+temp)
           carsup%valor(carsup%numero+1:carsup%numero+temp)=0.d0 
           carsup%constante(carsup%numero+1:carsup%numero+temp)=.FALSE.
           print*,'Type one of the function numbers below:'
           print*,1,': ',functions(1)
           print*,3,': ',functions(3)
           print*,4,': ',functions(4)
           read*, fnum
           sup%fun(carsup%numero+1:carsup%numero+temp) = fnum
           if (fnum/=1 .and. fnum/=3 .and. fnum/=4) stop 'incorrect function number'
           print*,fnum,functions(fnum)
           carsup%numero = carsup%numero + temp
        enddo
     endif

     if (temp2.eq.1) then
        print*,'Number of surfaces'
        read*,temp
        print *, temp
        if (temp.gt.0) then
           print *, 'Surface references and associated values'
           do i=1,temp
              read*,carsup%referencias(carsup%numero+i)
              read*,carsup%valor(carsup%numero+i)
              carsup%constante(carsup%numero+i) = .TRUE.
           enddo
           print *, (carsup%referencias(carsup%numero+i),i=1,temp)
           print *, (carsup%valor(carsup%numero+i),i=1,temp)
           carsup%numero = carsup%numero + temp
        endif               
     end if
     
  end if

  print*,'Curvilinear charge option (1 Yes 0 No)'
  read*,iopcur
  print*,iopcur
  
  if (iopcur.eq.1) then
  
     print*,'Curvilinear charge option'
     print*, 'Input via function (1 Yes 0 No)'
     read*,temp1
     print *, temp1
     print*,'Curvilinear charge option'
     print*, 'Input via constants in the curves (1 Yes 0 No)'
     read*,temp2
     print *, temp2

     if (temp1.eq.1) then
        do while (.TRUE.)
           print*,'Number of curves'
           read*,temp
           print*,temp
           if (temp <= 0) exit  
           print*,'Curve references'
           read*,(carcur%referencias(i),i=carcur%numero+1,carcur%numero+temp)
           print*,(carcur%referencias(i),i=carcur%numero+1,carcur%numero+temp)
           carcur%valor(carcur%numero+1:carcur%numero+temp)=0.d0 
           carcur%constante(carcur%numero+1:carcur%numero+temp)=.FALSE.
           print*,'Type one of the function numbers below:'
           print*,1,': ',functions(1)
           print*,5,': ',functions(5)
           read*, fnum
           cur%fun(carcur%numero+1:carcur%numero+temp) = fnum
           if (fnum/=1 .and. fnum/=5) stop 'incorrect function number'  
           print*,fnum,functions(fnum)
           carcur%numero = carcur%numero + temp
        enddo
     endif

     if (temp2.eq.1) then
           print*,'Number of curves'
           read*,temp
           print *, temp
           if(temp.gt.0) then
              print*, 'Curve references and associated charges' 
              do i=1,temp
                 read*,carcur%referencias(carcur%numero+i)
                 read*,carcur%valor(carcur%numero+i)
                 carcur%constante(carcur%numero+i) = .TRUE.
              enddo
              print *, (carcur%referencias(carcur%numero+i),i=1,temp)
              print *, (carcur%valor(carcur%numero+i),i=1,temp)
              carcur%numero = carcur%numero + temp
           endif               
        end if
        
     end if

  print*,'Punctual charge option (1 Yes 0 No)'
  read*,ioppun
  print*,ioppun
  
  if (ioppun.eq.1) then
     print*,'Number of charges'
     read*,ncarpun
     do i=1,ncarpun
        print*,'Point coordinates and associated charge'
        read*,xcarpun(i)
        read*,ycarpun(i)
        read*,zcarpun(i)
        read*,carpun(i)
        print*, xcarpun(i),ycarpun(i),zcarpun(i)
        print*, carpun(i)
     enddo
  end if

  iopteta=0

  print*,'Number of subdomains'
  read*,permirel%numero
  do i=1,permirel%numero
     print*,'Subdomain number'
     read*,permirel%referencias(i)
     print*,permirel%referencias(i)
     print*,'Relative electric permitivity option'
     print*,'1 --> Via function'
     print*,'2 --> Via constants per domain'
     print*,'3 --> Via array'
     read*,permirel%iopermir(i)
     print*,permirel%iopermir(i)
     if(permirel%iopermir(i).eq.1) then 
        print*,'Function name'
        !do j=1,size(functions_perm,1)
        !   print *, j,': ', functions_perm(j)
        !enddo
        read*,permirel%etiqueta(i)
        !print*,permirel%fun(i)
        if (permirel%fun(i)<1 .or. permirel%fun(i)> &
            size(functions_perm,1)) stop 'incorrect function number' 
     elseif(permirel%iopermir(i).eq.2) then
        print*,'Electric permitivity (x,y,z)' 
        read*,permirel%valorx(i), permirel%valory(i), permirel%valorz(i)
        print*,permirel%valorx(i), permirel%valory(i), permirel%valorz(i)
     elseif(permirel%iopermir(i).eq.3)then
        iopteta=1
        read*,permirel%ntab(i) 
        do j=1,permirel%ntab(i)
           read*,permirel%teta(i,j),permirel%valtabx(i,j),  &
                 permirel%valtaby(i,j),permirel%valtabz(i,j)
        enddo
     else
        stop 'Incorrect relative electric permitivity option: only 1 , 2 , 3'
     endif              
  enddo

  if (iopteta.eq.1) then
     print*,'Name of temperature file'
     read*,fichteta
     print*,fichteta
  endif
 
  print*,'Option for the linear system resolution'
  print*,'1: Direct method, 0 : Conjugate gradient'
  read*,iopsl
  print*,iopsl          
  if(iopsl.eq.0) then
     print*,'Corvengence error in CG'
     read* ,epscg
     print*,epscg
     print*,'Maximum number of iterations in CG'
     read*,nitcg
     print*,nitcg
  endif

  if(allocated(ncaras))deallocate(ncaras)
  allocate(ncaras(carsup%numero),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating ncaras'
     stop 1
  endif      

  if(allocated(nodc1))deallocate(nodc1)
  if(allocated(nodc2))deallocate(nodc2)
  if(allocated(nodc3))deallocate(nodc3)
  allocate(nodc1(carsup%numero,ndcaras), &
           nodc2(carsup%numero,ndcaras), &
           nodc3(carsup%numero,ndcaras),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating nodc1,2 o 3'
     stop 1
  endif

  if(allocated(naristas))deallocate(naristas)
  allocate(naristas(carcur%numero),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating naristas'
     stop 1
  endif
  
  if(allocated(nod1))deallocate(nod1)
  if(allocated(nod2))deallocate(nod2)
  allocate(nod1(carcur%numero,ndar), &
           nod2(carcur%numero,ndar),stat=ierror)
  if (ierror.ne.0) then
     print*,'Error while allocating nod1 o nod2'
     stop 1
  endif

  return
  end
