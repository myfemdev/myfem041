************************************************************************
*     SUBRUTINA wrtcmp                                                 *
*     Subrutina para la escritura del campo                            *
************************************************************************
      SUBROUTINE wrtcmp(sol,iusal,fich)
      use malla_3DP1

      implicit double precision(a-h,o-z)
      DIMENSION sol(*)
      character*(*) fich
      integer :: iusal
  
         open(iusal, file = fich, form = 'unformatted')
          rewind(iusal)
         write(iusal)nver,(sngl(sol(i)),i=1,nver) 
         close(iusal)
         return

      END