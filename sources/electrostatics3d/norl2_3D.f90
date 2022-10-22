!-----------------------------------------------------------------
!  Computation of the Euclidean norm using a quadrature
!  formula of degree 2
!  u      : function of which we are going to calculate the norm
!  xnorl2 : euclidean norm calculated
!-----------------------------------------------------------------

subroutine norl2_3D(u,xnorl2)

  use malla_3DP1 , only : nel, mm, z
  use mod_mcambio
  use cp

  implicit none
  type(cambio)::cambiot
  integer::ie,ic,j,ncua,i

  real(DOUBLE)::ncnorm(6,4),pes(6),P(1,4,6),BT(3,3),detBT,suma,suma1,aux1,&
                Pj(1,4),fexa,temp(3,1),x(3,1),CT(3,1),suma2,suma3,xnorl2, u(*)
 
  ncua=6
  pes(1:6)=1d0/6d0
  ncnorm(1:6,1:4)=0d0
  ncnorm(1,1)=0.5d0
  ncnorm(1,2)=0.5d0
  ncnorm(2,1)=0.5d0
  ncnorm(2,3)=0.5d0
  ncnorm(3,2)=0.5d0
  ncnorm(3,3)=0.5d0
  ncnorm(4,1)=0.5d0
  ncnorm(4,4)=0.5d0
  ncnorm(5,2)=0.5d0
  ncnorm(5,4)=0.5d0
  ncnorm(6,3)=0.5d0
  ncnorm(6,4)=0.5d0

! LOOP IN QUADRATURE NODES    
  do i=1,ncua 
    P(1,1,i)=ncnorm(i,1)
    P(1,2,i)=ncnorm(i,2)
    P(1,3,i)=ncnorm(i,3)
    P(1,4,i)=ncnorm(i,4)
  enddo

  suma1=0d0
  do ie=1,nel
    cambiot=afin(ie) ! CHANGE OF TETRAHEDRON          
    detBT=cambiot%det   
    suma=0d0
    suma3=0d0
    CT(1:3,1)=cambiot%CT(1:3)
    do ic=1,ncua
      aux1=0d0
      Pj=P(:,:,ic)
      temp(1,1)=ncnorm(ic,1)
      temp(2,1)=ncnorm(ic,2)
      temp(3,1)=ncnorm(ic,3)
      x=matmul(cambiot%BT,temp)+CT ! CALCULATES THE QUADRATURE POINT IN TETRAHEDRON ie FOR EVALUATING
                                   ! THE EXACT, OTHERWISE HAVING THE DETERMINANT IS ENOUGH 
      do j=1,4
        aux1=aux1+Pj(1,j)*u(mm(j,ie))
      enddo
      suma=suma+aux1**2*pes(ic)
    enddo
    suma1=suma1+suma*abs(detBT)/6.
  enddo
  xnorl2=sqrt(suma1)

  return
end
