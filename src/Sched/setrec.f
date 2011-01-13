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
C
C        Remove the setting of tape speeds - obsolete.  Jan. 7, 2011
C
C        Set the barrel roll default.  This is correlator and
C        format dependent.  Do not use it with disk systems.
C        Probably drive a stake through BARREL some day as I don't
C        think it is used anywhere.
C         
         ISTA = ISCHSTA(ISETSTA(KS))
         IF( BARREL(KS) .EQ. 'not_set' ) THEN
            IF( USEDISK(ISTA) ) THEN
               BARREL(KS) = 'roll_off'
            ELSE IF( CORREL(1:7) .EQ. 'FXCORR' ) THEN
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
     1            USEDISK(ISTA) ) THEN
            IF( BWARN ) THEN
               CALL WLOG( 1, 'SETREC:  Barrel roll is obsolete - '//
     1              'not used for disk' )
               BWARN = .FALSE.
            END IF
            BARREL(KS) = 'roll_off'
         END IF
C
      END DO
C
C     Make changes to TAPEMODE if needed.
C
C     For disk stations, set TAPEMODE to 1 which better reflects
C     what they do.  This should only affect "track" assignments
C     downstream from here.  Earlier it might affect mode 
C     assignments.
C
C     Obsolete, and removed from routine.  But it used to, for 
C     tape stations, prevent changes in TAPEMODE which can
C     make tape management a nightmare.
C
      CALL TPMFIX
C
C     Tape speed stuff for S2 removed Jan. 7, 2011.  RCW.
C
      RETURN
      END



