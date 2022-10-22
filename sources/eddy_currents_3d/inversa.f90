! http://en.wikipedia.org/wiki/Invertible_matrix

module inversa_mod

use defines

implicit none

contains

logical function inv2x2(A, B)
real(real64), dimension(2,2), intent(in) :: A
real(real64), dimension(2,2), intent(out) :: B
real(real64) :: d

 inv2x2 = .true.

 d = A(1,1)*A(2,2)-A(1,2)*A(2,1)

 if (d==0) then
    inv2x2 = .false.
    return
 endif

 B(1,1) = A(2,2)
 B(2,1) = -A(2,1)
 B(1,2) = -A(1,2)
 B(2,2) = A(1,1)

 B = B / d

end function

logical function inv4x4a(M1, M2)
real(real64), dimension(4,4), intent(in) :: M1
real(real64), dimension(4,4), intent(out) :: M2
real(real64), dimension(2,2) :: A, B, C, D, Ai, V, Vi

 inv4x4a = .false.

 A = M1(1:2,1:2)
 B = M1(1:2,3:4)
 C = M1(3:4,1:2)
 D = M1(3:4,3:4)

 if (.not. inv2x2(A, Ai)) then
!    print *, 'ret no a1'
    return
 endif

 V = D - matmul(matmul(C,Ai),B)

 if (.not. inv2x2(V, Vi)) then
!    print *, 'ret no a2'
    return
 endif

 M2(1:2,1:2) = Ai + matmul(matmul(matmul(matmul(Ai,B),Vi),C),Ai)
 M2(1:2,3:4) = matmul(matmul(-Ai,B),Vi)
 M2(3:4,1:2) = matmul(matmul(-Vi,C),Ai)
 M2(3:4,3:4) = Vi

 inv4x4a = .true.

end function


logical function inv4x4b(M1, M2)
real(real64), dimension(4,4), intent(in) :: M1
real(real64), dimension(4,4), intent(out) :: M2
real(real64), dimension(2,2) :: A, B, C, D, Di, V, Vi

 inv4x4b = .false.

 A = M1(1:2,1:2)
 B = M1(1:2,3:4)
 C = M1(3:4,1:2)
 D = M1(3:4,3:4)

 if (.not. inv2x2(D, Di)) then
!    print *, 'ret no b1'
    return
 endif

 V = A - matmul(matmul(B,Di),C)

 if (.not. inv2x2(V, Vi)) then
!    print *, 'ret no b2'
    return
 endif

 M2(1:2,1:2) = Vi
 M2(1:2,3:4) = matmul(matmul(-Vi,B),Di)
 M2(3:4,1:2) = matmul(matmul(-Di,C),Vi)
 M2(3:4,3:4) = Di + matmul(matmul(matmul(matmul(Di,C),Vi),B),Di)

 inv4x4b = .true.

end function


! preferible

real(real64) function det4x4z(m)
real(real64), dimension(0:3,0:3), intent(in) :: m

 det4x4z = &
   m(0,3) * m(1,2) * m(2,1) * m(3,0) - m(0,2) * m(1,3) * m(2,1) * m(3,0) &
        - m(0,3) * m(1,1) * m(2,2) * m(3,0) + m(0,1) * m(1,3) * m(2,2) * m(3,0)+ &
   m(0,2) * m(1,1) * m(2,3) * m(3,0) - m(0,1) * m(1,2) * m(2,3) * m(3,0) &
        - m(0,3) * m(1,2) * m(2,0) * m(3,1) + m(0,2) * m(1,3) * m(2,0) * m(3,1)+ &
   m(0,3) * m(1,0) * m(2,2) * m(3,1) - m(0,0) * m(1,3) * m(2,2) * m(3,1) &
        - m(0,2) * m(1,0) * m(2,3) * m(3,1) + m(0,0) * m(1,2) * m(2,3) * m(3,1)+ &
   m(0,3) * m(1,1) * m(2,0) * m(3,2) - m(0,1) * m(1,3) * m(2,0) * m(3,2) &
        - m(0,3) * m(1,0) * m(2,1) * m(3,2) + m(0,0) * m(1,3) * m(2,1) * m(3,2)+ &
   m(0,1) * m(1,0) * m(2,3) * m(3,2) - m(0,0) * m(1,1) * m(2,3) * m(3,2) &
        - m(0,2) * m(1,1) * m(2,0) * m(3,3) + m(0,1) * m(1,2) * m(2,0) * m(3,3)+ &
   m(0,2) * m(1,0) * m(2,1) * m(3,3) - m(0,0) * m(1,2) * m(2,1) * m(3,3) &
        - m(0,1) * m(1,0) * m(2,2) * m(3,3) + m(0,0) * m(1,1) * m(2,2) * m(3,3)

end function


! preferible a inv4x4a e inv4x4b (hai casos que inv4x4z invirte e non inv4x4a ou inv4x4b)

logical function inv4x4z(m, a)
real(real64), dimension(0:3,0:3), intent(in) :: m
real(real64), dimension(0:3,0:3), intent(out) :: a
real(real64) :: d
 inv4x4z = .false.
 d = det4x4z(m)
 if (d == 0.0d0) return
 inv4x4z = .true.

   a(0,0) = m(1,2)*m(2,3)*m(3,1) - m(1,3)*m(2,2)*m(3,1) + m(1,3)*m(2,1)*m(3,2) &
        - m(1,1)*m(2,3)*m(3,2) - m(1,2)*m(2,1)*m(3,3) + m(1,1)*m(2,2)*m(3,3)
   a(0,1) = m(0,3)*m(2,2)*m(3,1) - m(0,2)*m(2,3)*m(3,1) - m(0,3)*m(2,1)*m(3,2) &
        + m(0,1)*m(2,3)*m(3,2) + m(0,2)*m(2,1)*m(3,3) - m(0,1)*m(2,2)*m(3,3)
   a(0,2) = m(0,2)*m(1,3)*m(3,1) - m(0,3)*m(1,2)*m(3,1) + m(0,3)*m(1,1)*m(3,2) &
        - m(0,1)*m(1,3)*m(3,2) - m(0,2)*m(1,1)*m(3,3) + m(0,1)*m(1,2)*m(3,3)
   a(0,3) = m(0,3)*m(1,2)*m(2,1) - m(0,2)*m(1,3)*m(2,1) - m(0,3)*m(1,1)*m(2,2) &
        + m(0,1)*m(1,3)*m(2,2) + m(0,2)*m(1,1)*m(2,3) - m(0,1)*m(1,2)*m(2,3)
   a(1,0) = m(1,3)*m(2,2)*m(3,0) - m(1,2)*m(2,3)*m(3,0) - m(1,3)*m(2,0)*m(3,2) &
        + m(1,0)*m(2,3)*m(3,2) + m(1,2)*m(2,0)*m(3,3) - m(1,0)*m(2,2)*m(3,3)
   a(1,1) = m(0,2)*m(2,3)*m(3,0) - m(0,3)*m(2,2)*m(3,0) + m(0,3)*m(2,0)*m(3,2) &
        - m(0,0)*m(2,3)*m(3,2) - m(0,2)*m(2,0)*m(3,3) + m(0,0)*m(2,2)*m(3,3)
   a(1,2) = m(0,3)*m(1,2)*m(3,0) - m(0,2)*m(1,3)*m(3,0) - m(0,3)*m(1,0)*m(3,2) &
        + m(0,0)*m(1,3)*m(3,2) + m(0,2)*m(1,0)*m(3,3) - m(0,0)*m(1,2)*m(3,3)
   a(1,3) = m(0,2)*m(1,3)*m(2,0) - m(0,3)*m(1,2)*m(2,0) + m(0,3)*m(1,0)*m(2,2) &
        - m(0,0)*m(1,3)*m(2,2) - m(0,2)*m(1,0)*m(2,3) + m(0,0)*m(1,2)*m(2,3)
   a(2,0) = m(1,1)*m(2,3)*m(3,0) - m(1,3)*m(2,1)*m(3,0) + m(1,3)*m(2,0)*m(3,1) &
        - m(1,0)*m(2,3)*m(3,1) - m(1,1)*m(2,0)*m(3,3) + m(1,0)*m(2,1)*m(3,3)
   a(2,1) = m(0,3)*m(2,1)*m(3,0) - m(0,1)*m(2,3)*m(3,0) - m(0,3)*m(2,0)*m(3,1) &
        + m(0,0)*m(2,3)*m(3,1) + m(0,1)*m(2,0)*m(3,3) - m(0,0)*m(2,1)*m(3,3)
   a(2,2) = m(0,1)*m(1,3)*m(3,0) - m(0,3)*m(1,1)*m(3,0) + m(0,3)*m(1,0)*m(3,1) &
        - m(0,0)*m(1,3)*m(3,1) - m(0,1)*m(1,0)*m(3,3) + m(0,0)*m(1,1)*m(3,3)
   a(2,3) = m(0,3)*m(1,1)*m(2,0) - m(0,1)*m(1,3)*m(2,0) - m(0,3)*m(1,0)*m(2,1) &
        + m(0,0)*m(1,3)*m(2,1) + m(0,1)*m(1,0)*m(2,3) - m(0,0)*m(1,1)*m(2,3)
   a(3,0) = m(1,2)*m(2,1)*m(3,0) - m(1,1)*m(2,2)*m(3,0) - m(1,2)*m(2,0)*m(3,1) &
        + m(1,0)*m(2,2)*m(3,1) + m(1,1)*m(2,0)*m(3,2) - m(1,0)*m(2,1)*m(3,2)
   a(3,1) = m(0,1)*m(2,2)*m(3,0) - m(0,2)*m(2,1)*m(3,0) + m(0,2)*m(2,0)*m(3,1) &
        - m(0,0)*m(2,2)*m(3,1) - m(0,1)*m(2,0)*m(3,2) + m(0,0)*m(2,1)*m(3,2)
   a(3,2) = m(0,2)*m(1,1)*m(3,0) - m(0,1)*m(1,2)*m(3,0) - m(0,2)*m(1,0)*m(3,1) &
        + m(0,0)*m(1,2)*m(3,1) + m(0,1)*m(1,0)*m(3,2) - m(0,0)*m(1,1)*m(3,2)
   a(3,3) = m(0,1)*m(1,2)*m(2,0) - m(0,2)*m(1,1)*m(2,0) + m(0,2)*m(1,0)*m(2,1) &
        - m(0,0)*m(1,2)*m(2,1) - m(0,1)*m(1,0)*m(2,2) + m(0,0)*m(1,1)*m(2,2)

 a = a / d

end function



!inverts M -> yy
!% M=[1 1 1 1;a11 a12 a13 a14;a21 a22 a23 a24;a31 a32 a33 a34]
!%
!% M =
!%
!% [   1,   1,   1,   1]
!% [ a11, a12, a13, a14]
!% [ a21, a22, a23, a24]
!% [ a31, a32, a33, a34]
logical function inv3x4_1(aa, yy)
real(real64), dimension(3,4), intent(in) :: aa
real(real64), dimension(4,4), intent(out) :: yy
real(real64), dimension(36) :: pp
real(real64), dimension(24) :: summ
real(real64) :: denm

 inv3x4_1 = .false.

pp(1)=aa(2,3)*aa(3,4)
pp(2)=aa(2,4)*aa(3,3)
pp(3)=aa(2,2)*aa(3,4)
pp(4)=aa(2,2)*aa(3,3)
pp(5)=aa(3,2)*aa(2,4)
pp(6)=aa(3,2)*aa(2,3)
pp(7)=aa(1,3)*aa(3,4)
pp(8)=aa(1,4)*aa(3,3)
pp(9)=aa(1,2)*aa(3,4)
pp(10)=aa(1,2)*aa(3,3)
pp(11)=aa(3,2)*aa(1,4)
pp(12)=aa(3,2)*aa(1,3)
pp(13)=aa(1,3)*aa(2,4)
pp(14)=aa(1,4)*aa(2,3)
pp(15)=aa(1,2)*aa(2,4)
pp(16)=aa(1,2)*aa(2,3)
pp(17)=aa(2,2)*aa(1,4)
pp(18)=aa(2,2)*aa(1,3)
pp(19)=aa(2,1)*aa(3,4)
pp(20)=aa(2,1)*aa(3,3)
pp(21)=aa(3,1)*aa(2,4)
pp(22)=aa(3,1)*aa(2,3)
pp(23)=aa(1,1)*aa(3,4)
pp(24)=aa(1,1)*aa(3,3)
pp(25)=aa(3,1)*aa(1,4)
pp(26)=aa(3,1)*aa(1,3)
pp(27)=aa(1,1)*aa(2,4)
pp(28)=aa(1,1)*aa(2,3)
pp(29)=aa(2,1)*aa(1,4)
pp(30)=aa(2,1)*aa(1,3)
pp(31)=aa(2,1)*aa(3,2)
pp(32)=aa(3,1)*aa(2,2)
pp(33)=aa(1,1)*aa(3,2)
pp(34)=aa(3,1)*aa(1,2)
pp(35)=aa(1,1)*aa(2,2)
pp(36)=aa(2,1)*aa(1,2)

summ(1)=aa(1,2)*pp(1)
summ(2)=aa(1,2)*pp(2)
summ(3)=aa(2,2)*pp(7)
summ(4)=aa(2,2)*pp(8)
summ(5)=pp(12)*aa(2,4)
summ(6)=pp(11)*aa(2,3)
summ(7)=aa(1,1)*pp(1)
summ(8)=aa(1,1)*pp(2)
summ(9)=aa(1,1)*pp(3)
summ(10)=aa(1,1)*pp(4)
summ(11)=aa(1,1)*pp(5)
summ(12)=aa(1,1)*pp(6)
summ(13)=aa(2,1)*pp(7)
summ(14)=aa(2,1)*pp(8)
summ(15)=aa(2,1)*pp(9)
summ(16)=aa(2,1)*pp(10)
summ(17)=aa(2,1)*pp(11)
summ(18)=aa(2,1)*pp(12)
summ(19)=aa(3,1)*pp(13)
summ(20)=aa(3,1)*pp(14)
summ(21)=aa(3,1)*pp(15)
summ(22)=aa(3,1)*pp(16)
summ(23)=aa(3,1)*pp(17)
summ(24)=aa(3,1)*pp(18)

denm=-summ(1)+summ(2)+summ(3)-summ(4)-summ(5)+summ(6)+summ(7)-summ(8)-summ(9)+summ(10)+summ(11)-summ(12) &
    -summ(13)+summ(14)+summ(15)-summ(16)-summ(17)+summ(18)+summ(19)-summ(20)-summ(21)+summ(22)+summ(23)-summ(24)

 if (denm == 0.0d0) return
 inv3x4_1 = .true.

yy(1,1)=-(summ(1)-summ(2)-summ(3)+summ(4)+summ(5)-summ(6))
yy(1,2)=pp(1)-pp(2)-pp(3)+pp(4)+pp(5)-pp(6)
yy(1,3)=-(pp(7)-pp(8)-pp(9)+pp(10)+pp(11)-pp(12))
yy(1,4)=pp(13)-pp(14)-pp(15)+pp(16)+pp(17)-pp(18)

yy(2,1)=summ(7)-summ(8)-summ(13)+summ(14)+summ(19)-summ(20)
yy(2,2)=-(pp(1)-pp(2)-pp(19)+pp(20)+pp(21)-pp(22))
yy(2,3)=pp(7)-pp(8)-pp(23)+pp(24)+pp(25)-pp(26)
yy(2,4)=-(pp(13)-pp(14)-pp(27)+pp(28)+pp(29)-pp(30))

yy(3,1)=-(summ(9)-summ(11)-summ(15)+summ(17)+summ(21)-summ(23))
yy(3,2)=pp(3)-pp(5)-pp(19)+pp(31)+pp(21)-pp(32)
yy(3,3)=-(pp(9)-pp(11)-pp(23)+pp(33)+pp(25)-pp(34))
yy(3,4)=pp(15)-pp(17)-pp(27)+pp(35)+pp(29)-pp(36)

yy(4,1)=summ(10)-summ(12)-summ(16)+summ(18)+summ(22)-summ(24)
yy(4,2)=-(pp(4)-pp(6)-pp(20)+pp(31)+pp(22)-pp(32))
yy(4,3)=pp(10)-pp(12)-pp(24)+pp(33)+pp(26)-pp(34)
yy(4,4)=-(pp(16)-pp(18)-pp(28)+pp(35)+pp(30)-pp(36))

 yy = yy / denm

end function


end module

