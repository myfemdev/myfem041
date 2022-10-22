!-----------------------------------------------------------------
!  Computation of the Euclidean norm of the error for a 
!  two-dimensional mesh with P1 finite elements in cartesian
!  coordinates 
!  b: array with the absolute error in the vertices
!-----------------------------------------------------------------

subroutine norl22d(nel,mm,z,b,xnorl2)

  implicit none
  integer,          intent(in)  :: nel
  integer,          intent(in)  :: mm(3,*)
  double precision, intent(in)  :: z(2,*),b(*)
  double precision, intent(out) :: xnorl2
  integer                       :: mm1,mm2,mm3
  double precision              :: ab,bc,cd,de,det,suma
  integer                       :: k

  xnorl2=0.d0
  do k=1,nel
     mm1=mm(1,k)
     mm2=mm(2,k)
     mm3=mm(3,k)
     ab=z(1,mm2)-z(1,mm1)
     bc=z(2,mm2)-z(2,mm1)
     cd=z(1,mm3)-z(1,mm1)
     de=z(2,mm3)-z(2,mm1)
     det=ab*de-bc*cd
     suma=((b(mm1)+b(mm2))*0.5d0)**2+&
         ((b(mm2)+b(mm3))*0.5d0)**2+&
         ((b(mm1)+b(mm3))*0.5d0)**2
     xnorl2=xnorl2+suma*det/6
  enddo
  xnorl2=dsqrt(xnorl2)

  return
end

