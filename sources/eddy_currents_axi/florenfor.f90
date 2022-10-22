      subroutine florenfor(nsdl,ndom)

!******************************************************************
!* objetivo : calcula la fuerza de Lorentz                        *
!  promedio en un ciclo, y se calcula utilizando gradA
!******************************************************************
!
!*              floel: fuerza de Lorentz                            *
!*              grar: gradiente de la parte dreal del potencial    *
!*              grai: ggradiente de la parte dimaginaria del pot.  *
!*              ai: parte dimaginaria del potencial                *
!*              ar. parte dreal del potencial                      * 
!******************************************************************

      use electros_2D
      use fich_electros
      use malla_2DP1
      use derivados
      use module_writeVTU
      use module_fem_extract
      use sourcevolumic
      use conductividad
 
      use postpro
      implicit none
      
      integer :: k, nsdl, ndom,ii
      integer :: no1,no2,no3

      double complex, allocatable :: cf(:),floel(:,:)
      double complex:: jbar
      
      double precision, allocatable::   grar(:,:),grai(:,:)
      double precision:: sigmaloc, ai(3),ar(3),g1,g2, aux1,aux2
      
      character(len=255) :: cad
  
	  sigmaloc = conducel(nsdl,ndom)



      if (allocated(grar)) deallocate(grar) 
      allocate(grar(2,subnel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array grar'

	  if (allocated(grai)) deallocate(grai) 
      allocate(grai(2,subnel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array grai'

	  if (allocated(floel)) deallocate(floel) 
      allocate(floel(2,subnel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array floel'

	  floel = dcmplx(0.d0,0.d0)
	  grar = 0.d0
	  grai = 0.d0


      do 1 k=1,subnel

        no1=submm(1,k)
        no2=submm(2,k)
        no3=submm(3,k)
      
        ar(1)=dreal(sol(globv(no1)))
        ai(1)=dimag(sol(globv(no1)))
        
        ar(2)=dreal(sol(globv(no2)))
        ai(2)=dimag(sol(globv(no2)))
        
        ar(3)=dreal(sol(globv(no3)))
        ai(3)=dimag(sol(globv(no3)))

!*
!*     calculo del gradiente de la parte dreal
!*
        call  matelfl(subz(1,no1),subz(1,no2),subz(1,no3),ar,g1,g2)
        
        grar(1,k) = g1
        grar(2,k) = g2
        

!*
!*     calculo del gradiente de la parte dimaginaria
!*
        call  matelfl(subz(1,no1),subz(1,no2),subz(1,no3),ai,g1,g2)
        
        grai(1,k) = g1
        grai(2,k) = g2

		jbar = (subcncm(no1) + subcncm(no2) + subcncm(no3))/3.d0
		

		floel(1,k) =  (dreal(jbar)*grar(1,k) + dimag(jbar)*grai(1,k))/2.d0

	    floel(2,k) = (dreal(jbar)*grar(2,k) + dimag(jbar)*grai(2,k))/2.d0
	    
   
1     continue


      if (allocated(cf)) deallocate(cf) 
      allocate(cf(2*subnel), STAT = ierror)
      if (ierror .ne. 0) stop 'Error when allocating array cf'
      cf(1:subnel*2:2) = floel(1,1:subnel)
      cf(2:subnel*2:2) = floel(2,1:subnel)

         
          !      - paso del campo a nodos (subcn)
         call cell2node(subnver, submm, cf, subcn)
     
              
!      - escritura por nodos y subdominio
       write(cad,*) nsdl
       call writeVTU(subnel,subnver,submm,subz,'triangle',dreal(subcn),'Lorentz force', &
       'vector','node',trim(fichflo)//trim(adjustl(cad))//'.vtu')
       
   


         return          
      deallocate(floel, grar,grai,cf)

      return
      end
