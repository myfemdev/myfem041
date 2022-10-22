************************************************************************
*     SUBRUTINA wrtcmp                                                 *
*     Subrutina para la escritura del campo                            *
************************************************************************
      SUBROUTINE wrtcmp(num,sol,iusal,fich)

!      use malla_3DP1

      implicit double precision(a-h,o-z)
      integer :: num
      DIMENSION sol(*)
      character*(*) fich
      integer :: iusal
  
         open(iusal, file = fich, form = 'formatted')
          rewind(iusal)
         write(iusal,*)num,(sngl(sol(i)),i=1,num) 
         close(iusal)
         return

      END
