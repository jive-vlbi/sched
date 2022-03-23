      SUBROUTINE PLAXDF( EL )
C
C     Routine for sched that Reset the default value for
C     the axis selected.
C     If X axis equal UT/GST/LST set to the schedule time
C     If X/Y axis equal Km set to the schedule max baseline
C
      INCLUDE 'plot.inc'
C
      INTEGER          EL, K, I, J, KI, KE
      CHARACTER        AX
C ----------------------------------------------------------------------
C
C     Set the pointers to the axis type.
C
      IF( EL .LE. 2 ) THEN
         K  = PXYBCK(1)
         J  = 0
         AX = 'X'
      ELSE
         K  = PXYBCK(2)
         J  = 2
         AX = 'Y'
      END IF
C
C     Reset values
C
      IF( PXYTYP(K) .EQ. 'UT' ) THEN
         DO 10 I=1,3
            PXSVAL(K,EL,I) = PUTVAL(EL,I)
 10      CONTINUE
C
      ELSE IF( PXYTYP(K) .EQ. 'LST' ) THEN
         DO 20 I=1,3
            PXSVAL(K,EL,I) = PLSVAL(EL,I)
 20      CONTINUE
C
      ELSE IF( PXYTYP(K) .EQ. 'GST' ) THEN
         DO 30 I=1,3
            PXSVAL(K,EL,I) = PSGVAL(EL,I)
 30      CONTINUE
C
      ELSE IF( PXYTYP(K) .EQ. 'Km' .OR. 
     1         PXYTYP(K) .EQ. 'Wv' ) THEN
         IF( POPTYP(POPBCK) .EQ. 'UV' .AND. PLOVAL ) THEN
            KI = 1
            KE = 4
         ELSE
            KI = EL
            KE = EL
         ENDIF
C
         DO 35 I=KI,KE
            IF( PXYTYP(K) .EQ. 'Km' ) THEN
               PXSVAL(K,I,1) = ABS( PKMVAL(I) )
               IF( PKMVAL(I) .GE. 0 ) THEN
                  PXSSGN(K,I) = 1
               ELSE
                  PXSSGN(K,I) = -1
               END IF
            ELSE
               PXSVAL(K,I,1) = ABS( PWLVAL(I) )
               PXYWLE = 3
               IF( PWLVAL(I) .GE. 0 ) THEN
                  PXSSGN(K,I) = 1
               ELSE
                  PXSSGN(K,I) = -1
               END IF
               CALL PLWLSC( ' ', 0, 1 )
            END IF   
 35      CONTINUE
C
      ELSE IF( PXYTYP(K) .EQ. 'Sec' ) THEN
            PXYSEC = .TRUE.
C
      ELSE
         PXSVAL(K,EL,1) = ABS( PXSLIM(K,( EL - J )) )
         DO 40 I = 2, 3
            PXSVAL(K,EL,I) = 0
 40      CONTINUE
         IF( PXSLIM(K,( EL - J )) .GE. 0 ) THEN
            PXSSGN(K,EL) = 1
         ELSE
            PXSSGN(K,EL) = -1
         END IF
C
      END IF
C
C     Reset Days if type UT/GST/LST
C
      IF( K .LE. 3 ) THEN
         IF( EL .EQ. 1 ) THEN
            PADAYS(K,1) = 0.D0
         ELSE
            PADAYS(K,2) = PADAYS(K,3)
         END IF
      END IF
C
C
C     Replot Value
C
      CALL PLAXST( AX )
C      
      IF( POPTYP(POPBCK) .EQ. 'UV' .AND. PLOVAL ) THEN
         IF( AX .EQ. 'X' ) THEN
            AX = 'Y'
         ELSE
            AX = 'X'
         ENDIF
         CALL PLAXST( AX )
      ENDIF
C
      RETURN
      END
