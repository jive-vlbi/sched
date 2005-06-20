      SUBROUTINE SETREC
C
C     Routine for SCHED that sets some of the recorder parameters.
C     Called by SETDEFS.  
C
C     The hardest part is setting the format.  See SETFORM for
C     details.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           KS, ISTA
      INTEGER           NEEDDR
      LOGICAL           BWARN
      SAVE              BWARN
      DATA              BWARN / .TRUE. /
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETREC: Starting' )
C
C     Set the formats.  This is the most painful part.  SETFORM
C     and the routines it calls do the work.
C
      CALL SETFORM
C
C     Set some more parameters related to VLBA/MKIV/MARKIII systems.
C
      DO KS = 1, NSET
         IF( VLBAMKIV(KS) .OR. FORMAT(KS) .EQ. 'MARKIII' ) THEN
C         
C           Set tape speed.
C         
            CALL SETSPD( KS )
C
         END IF
C         
C        Set the barrel roll default.  This is correlator and
C        format dependent.  Do not use it with disk systems.
C         
         ISTA = ISCHSTA(ISETSTA(KS))
         IF( BARREL(KS) .EQ. 'not_set' ) THEN
            IF( USEDISK(ISTA) ) THEN
               BARREL(KS) = 'roll_off'
            ELSE IF( CORREL(1:7) .EQ. 'SOCORRO' .OR. 
     1          CORREL(1:4) .EQ. 'VLBA' ) THEN
               IF( FORMAT(KS)(1:4) .NE. 'VLBA' .OR. 
     1             CONTROL(ISETSTA(KS)) .EQ. 'VEX' ) THEN
                  BARREL(KS) = 'roll_off'
               ELSE
                  IF( CORCHAN .GE. 1000 ) THEN
                     BARREL(KS) = 'roll_8'
                  ELSE
                     BARREL(KS) = 'roll_auto'
                  END IF
               END IF
            ELSE IF( CORREL(1:4) .EQ. 'JIVE' ) THEN
               BARREL(KS) = 'roll_off'
            ELSE
               BARREL(KS) = 'roll_off'
            END IF
         ELSE IF( BARREL(KS) .NE. 'roll_off' .AND. 
     1       ( USEDISK(ISTA) .AND. .NOT. USETAPE(ISTA) ) ) THEN
            IF( BWARN ) THEN
               CALL WLOG( 1, 'SETREC:  Barrel roll request will be'//
     1              'ignored for stations using disk' )
               BWARN = .FALSE.
            END IF
            BARREL(KS) = 'roll_off'
         END IF
      END DO
C
C     Prevent changes in TAPEMODE.
C
      CALL TPMFIX
C
C     Deal with S2
C
      DO KS = 1, NSET
         IF( FORMAT(KS) .EQ. 'S2' ) THEN
            IF( SPEEDL(KS) .EQ. 0.0 ) SPEEDL(KS) = 6.3
            IF( SPEEDH(KS) .EQ. 0.0 ) SPEEDH(KS) = 4.2
            FANOUT(KS) = MIN( 1.0, SAMPRATE(KS) / 16.0 )
            IF( TAPEMODE(KS) .EQ. 0  ) THEN
               NEEDDR = STNDRIV(ISETSTA(KS)) * 16.0 / 
     1                 ( NCHAN(KS) * BITS(1,KS) * SAMPRATE(KS) )
               TAPEMODE(KS) = MIN( NEEDDR, STNDRIV(ISETSTA(KS)) )
            END IF
            SPEEDUP(KS) = 1.0
         END IF
      END DO
C
      RETURN
      END



