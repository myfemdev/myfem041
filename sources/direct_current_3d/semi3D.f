*****************************************************************
*        CONSTRUCION DEL SEGUNDO MIEMBRO DEL PROBL. TERMICO     *
*****************************************************************

      SUBROUTINE semi3D()
      
       use malla_3DP1
       use electros3D
       use external_electros3D
       use cargavol
       use cargacur
       use cargapun 
       use cargasup
       use neumann
       use auxiliar_cargas
       use intensidades

       implicit double precision (a-h,o-z)
       
       dimension vnu(3)
       integer :: i,j,k
              
!       print*,'en semi3D'

C        ELECCION DEL METODO PARA LA INTEGRAL EN LA FRONTERA 
        
         IF (iopf .EQ. 1) THEN
            xnod11 = -0.577350269189626
            xnod22 =  0.577350269189626
         ELSE IF (iopf .EQ. 2) THEN
            xnod11 = -1.
            xnod22 =  1.
         END IF
         xnod1 = (1. + xnod11) * 0.5D0
         xnod2 = (1. + xnod22) * 0.5D0
        
        print*,'carvol%numero,iopvol,iop='
        print*,carvol%numero,iopvol,iop
        

        ! forma de calcular usando os arrays do principio de arint3D.f
        if (carvol%numero > 0 .and. iopvol == 1 .and. iop == 2) then
            do i = 1, carvol%numero
                do j = 1, nelem(i)
                    k = ensd(i,j)    
                    mm1 = mm(1,k)
                    mm2 = mm(2,k)
                    mm3 = mm(3,k)
                    mm4 = mm(4,k) 

                    if (carvol%constante(i)) then
                    b(mm1)=b(mm1)+fc(nsd(k),i)*det(k)/24d0
                    b(mm2)=b(mm2)+fc(nsd(k),i)*det(k)/24d0
                    b(mm3)=b(mm3)+fc(nsd(k),i)*det(k)/24d0
                    b(mm4)=b(mm4)+fc(nsd(k),i)*det(k)/24d0
                    else
                    b(mm1)=b(mm1)+f(z(1,mm1),z(2,mm1),z(3,mm1),nsd(k),i)
     &                 *det(k)/24d0
                    b(mm2)=b(mm2)+f(z(1,mm2),z(2,mm2),z(3,mm2),nsd(k),i)
     &                 *det(k)/24d0
                    b(mm3)=b(mm3)+f(z(1,mm3),z(2,mm3),z(3,mm3),nsd(k),i)
     &                 *det(k)/24d0
                    b(mm4)=b(mm4)+f(z(1,mm4),z(2,mm4),z(3,mm4),nsd(k),i)
     &                 *det(k)/24d0
                    endif 
                enddo
            enddo
        endif
        
        
C        BUCLE EN ELEMENTOS
         

         bucle_elementos: DO  k=1,nel
    

C           INTEGRAL EN LA FRONTERA NEUMANN 

      opcion_cc_neumann: if(iopneu.eq.1) then
      
      !print*,'opcion neuman en semi3d'


       opcion_cc_neuman_fun:   if(iopneu1.eq.1) then

            IF (nrn .GT. 0) THEN
               DO 3 j=1,4
                  DO 4 i=1,nrn
                     IF (nrc(j,k) .EQ. irefn(i)) THEN
                        iref = irefn(i)
       call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),
     &     z(1,mm(indc(3,j),k)),vnu,dnor)
      
c print*,'en semi3D, vnormal=',vnu,' en la cara iref=',iref
                        iv1=mm(indc(1,j),k)
                        iv2=mm(indc(2,j),k)
                        iv3=mm(indc(3,j),k)
                        area=dnor/2.d0

      b(iv1)=b(iv1)+g(z(1,iv1),z(2,iv1),z(3,iv1),iref,i)*area/3.d0
      b(iv2)=b(iv2)+g(z(1,iv2),z(2,iv2),z(3,iv2),iref,i)*area/3.d0
      b(iv3)=b(iv3)+g(z(1,iv3),z(2,iv3),z(3,iv3),iref,i)*area/3.d0

                        GO TO 3
                     END IF
    4             CONTINUE
    3          CONTINUE  
            END IF
      endif      opcion_cc_neuman_fun

       opcion_cc_neuman_cons:   if(iopneu2.eq.1) then
!       print*,'integrando neuman2'


            IF (neuman%numero .GT. 0) THEN
               DO 7 j=1,4
                  DO 8 i=1,neuman%numero
                     IF (nrc(j,k) .EQ. neuman%referencias(i)) THEN
                        iref = neuman%referencias(i)
         call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),
     &     z(1,mm(indc(3,j),k)),vnu,dnor)
                        iv1=mm(indc(1,j),k)
                        iv2=mm(indc(2,j),k)
                        iv3=mm(indc(3,j),k)
                        area=dnor/2.d0
                        gcons=neuman%valor(i)

            b(iv1)=b(iv1)+gcons*area/3.d0
            b(iv2)=b(iv2)+gcons*area/3.d0
            b(iv3)=b(iv3)+gcons*area/3.d0

                        GO TO 7
                     END IF
    8             CONTINUE
    7          CONTINUE  
            END IF

      endif     opcion_cc_neuman_cons

      endif     opcion_cc_neumann
      
      
!******************************************************************************
! IMPLEMENTACIÓN DE LA CONDICIÓN NEUMANN DANDO COMO DATO LAS INTENSIDADES
!******************************************************************************


C           INTEGRAL EN LA FRONTERA INTENSIDAD

      opcion_cc_intensidad: if(iopint.eq.1) then


       opcion_cc_intensidad_fun:  if(iopint1.eq.1) then

            IF (nri.GT. 0) THEN
               DO 33 j=1,4
                  DO 44 i=1,nri
                     IF (nrc(j,k) .EQ. irefi(i)) THEN
                        iref = irefi(i)
       call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),
     &     z(1,mm(indc(3,j),k)),vnu,dnor)
      

                        iv1=mm(indc(1,j),k)
                        iv2=mm(indc(2,j),k)
                        iv3=mm(indc(3,j),k)
                        area=dnor/2.d0

      b(iv1)=b(iv1)+gint(z(1,iv1),z(2,iv1),z(3,iv1),iref,i)*
     &              area/3.d0/areafi(i)
      b(iv2)=b(iv2)+gint(z(1,iv2),z(2,iv2),z(3,iv2),iref,i)*
     &              area/3.d0/areafi(i)
      b(iv3)=b(iv3)+gint(z(1,iv3),z(2,iv3),z(3,iv3),iref,i)*
     &              area/3.d0/areafi(i)


                        GO TO 33
                     END IF
   44             CONTINUE
   33          CONTINUE  
            END IF
      endif      opcion_cc_intensidad_fun

       opcion_cc_intensidad_cons:   if(iopint2.eq.1) then

            IF (inten%numero .GT. 0) THEN
               DO 77 j=1,4
                  DO 88 i=1,inten%numero
                     IF (nrc(j,k) .EQ. inten%referencias(i)) THEN
                        iref = inten%referencias(i)
         call calvnoru(z(1,mm(indc(1,j),k)),z(1,mm(indc(2,j),k)),
     &     z(1,mm(indc(3,j),k)),vnu,dnor)
                        iv1=mm(indc(1,j),k)
                        iv2=mm(indc(2,j),k)
                        iv3=mm(indc(3,j),k)
                        area=dnor/2.d0
                        gcons=inten%valor(i)

            b(iv1)=b(iv1)+gcons*area/3.d0/inten%area(i)
            b(iv2)=b(iv2)+gcons*area/3.d0/inten%area(i)
            b(iv3)=b(iv3)+gcons*area/3.d0/inten%area(i)
            

                        GO TO 77
                     END IF
   88             CONTINUE
   77          CONTINUE  
            END IF

      endif     opcion_cc_intensidad_cons

      endif     opcion_cc_intensidad
      
      enddo     bucle_elementos 



      END
