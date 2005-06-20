      SUBROUTINE VXWRIF
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the IF = $IF section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Update to 1.4 240796
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER IIF, KS, ICH, J
      INTEGER LEN1
      LOGICAL NEWFND, DOWARN
C ----------------------------------------------------------------------
C
C     Write the IF section
C
      DOWARN = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$IF', SEP     
C
      DO IIF = 1, NIFVEX
         KS = IFISSET(IIF)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        IFLINK(IIF)(1:LEN1(IFLINK(IIF))), SEP
         CALL VXSTLI( IIF, NSTAIF, ISTAIF )
         DO ICH = 1, NCHAN(KS)
            NEWFND = .FALSE.
            IF( ICH .EQ. 1 ) THEN
               NEWFND = .TRUE.
            ELSE
               NEWFND = .TRUE.
               DO J = 1, ICH-1
                  IF( IFCHAN( ICH, KS ) .EQ. IFCHAN( J,KS)  ) THEN
                     NEWFND = .FALSE.
                  END IF
               END DO
            END IF
C
C           could save NIF and IFNAMSs
C
            IF( NEWFND ) THEN
C
               IF( TONEINT(IIF) .NE. 1.0 ) DOWARN = .TRUE.
               IF( TONEINT(IIF) .GT. 1E-15 ) THEN               
                  WRITE( IVEX, '( 5X, A, A1, A, A, 1X, A1, 
     1                1X, A, 1X, A1, 1X, A1, 1X, A1, F8.1, 1X, A, 1X, 
     2                A1, 1X, A1, 1X, A1, 1X, I1, 1X, A, 1X, A1 )' ) 
     3                'if_def = ', LNK, 'IF_', 
     4                IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL, 
     5                IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL,
     6                POL(ICH,KS)(1:1), COL,
     7                FIRSTLO( ICH, KS), 'MHz',
     8                COL, SIDE1( ICH, KS ), COL, NINT(TONEINT(IIF)),
     9                'MHz', SEP
               ELSE
C
C              No Tone; 0 MHz here, or nothing....
C
                  WRITE( IVEX, '( 5X, A, A1, A, A, 1X, A1, 
     1                1X, A, 1X, A1, 1X, A1, 1X, A1, F8.1, 1X, A,
     2                1X, A1, 1X, A1, 1X, A1, 1X, A1, 1X, A )' ) 
     3                'if_def = ', LNK, 'IF_', 
     4                IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL, 
     5                IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL, 
     6                POL(ICH,KS), COL,
     7                FIRSTLO( ICH, KS), 'MHz',
     8                COL, SIDE1( ICH, KS ), SEP, COM, 'PCall off!'
               END IF
            END IF
         END DO
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C     
      ENDDO
      WRITE( IVEX, '( A )' ) COMLIN
C
C     warn in case of none standard PCAL
C
      IF( DOWARN ) THEN
         CALL WLOG( 1,'VXWRIF: WARNING, Phase cal not under '//
     2       'computer control for some stations.' ) 
         CALL WLOG( 1,'        Contact stations by e-mail to '//
     1       'make sure phase cal is switched off ')
      END IF

      RETURN
      END
   
