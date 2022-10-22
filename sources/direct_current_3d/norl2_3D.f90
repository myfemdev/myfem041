!function norl2_3d(u,mallap2,t)	result(nl2)

subroutine norl2_3D(u,xnorl2)

! u: funcion a la que se desea calcular su norma
! xnorl2 : normal2 calculada

use malla_3DP1 , only : nel, mm, z

use mod_mcambio
use cp

implicit none

type(cambio)::cambiot

integer::ie,ic,j,ncua,i

real(DOUBLE)::ncnorm(6,4),pes(6),P(1,4,6),BT(3,3),detBT,suma,suma1,aux1,&
&		Pj(1,4),fexa,temp(3,1),x(3,1),CT(3,1),suma2,suma3,xnorl2, u(*)

! u es un argumento al que en llamada le entra vexac 

!Calculo de la norma en l2 usando una formula de cuadratura de orden 2
!Usamos siempre la formula de cuadratura de orden 2!
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

!
do i=1,ncua !bucle en nodos de cuadratura 
    P(1,1,i)=ncnorm(i,1)
    P(1,2,i)=ncnorm(i,2)
    P(1,3,i)=ncnorm(i,3)
    P(1,4,i)=ncnorm(i,4)
enddo
!
suma1=0d0
do 1 ie=1,nel
    cambiot=afin(ie) !Aplicacion cambio de tetraedro 
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
        x=matmul(cambiot%BT,temp)+CT !Calcula el punto de cuadratura en el tetraedro ie para
                                     ! poder evaluar la exacta, si no evalueas la exacta basta con tener el determinante
        do j=1,4
            aux1=aux1+Pj(1,j)*u(mm(j,ie))
        enddo
        suma=suma+aux1**2*pes(ic)
    enddo
    suma1=suma1+suma*abs(detBT)/6.
1   continue
xnorl2=sqrt(suma1)

return
end
