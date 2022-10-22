      SUBROUTINE CMUA(MUA,MM,NEL,NVER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION MUA(*),MM(3,*)
      DO 1 I=2,NVER+1
1     MUA(I)=I-1
*
      DO 2  K=1,NEL

      MI=MIN0(MM(1,K),MM(2,K),MM(3,K))
      DO 2 J=1,3
2     MUA(MM(J,K)+1)=MIN0(MUA(MM(J,K)+1),MI)
      MUA(1)=0
      DO 3 I=3,NVER+1
3     MUA(I)=MUA(I-1)+I-MUA(I)
      RETURN
      END
