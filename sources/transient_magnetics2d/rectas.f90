    subroutine rectas (bf,hf,t,q,m)

    implicit none
    double precision :: bf(*),hf(*),t(*),q(*)
    integer, intent(in) :: m
    integer i

! Lectura de las medidas B-H del fabricante
! j: indica numero de subdominio
! i: indica cada uno de los tramos entre dos medidas consecutivas
! para cada i, se calculan t y q por subdominio. 
! t es la pendiente de la recta uniendo dos medidas consecutivas y
! q es la ecuacion de la recta
    do i=2,m
	    t(i)=(bf(i)-bf(i-1))/(hf(i)-hf(i-1))
	    q(i)=bf(i-1)-t(i)*hf(i-1)
    end do
    q(1)=q(2)
    q(m+1)=q(m)
    t(1)=t(2) 
    t(m+1)=t(m) 

    return
    end