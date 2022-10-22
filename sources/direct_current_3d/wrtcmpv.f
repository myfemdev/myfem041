************************************************************************
*     SUBRUTINA wrtcmpv                                                 *
*     Subrutina para la escritura del campo vectorial                            *
************************************************************************
      SUBROUTINE wrtcmpv(sol,iusal,fich)

      use malla_3DP1

      implicit double precision(a-h,o-z)
      DIMENSION sol(3,*)
      character*(*) fich
      integer :: iusal
  
      open(iusal, file = fich, form = 'unformatted')
       rewind(iusal)
       write(iusal)nel,(sngl(sol(1,i)),sngl(sol(2,i)),sngl(sol(3,i))
     &       ,i=1,nel) 
       close(iusal)
      return

      END