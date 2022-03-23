      CHARACTER*32 FUNCTION VXNMPO2( IXX )
C
C     Routine specific for the VEX extension of SCHED
C     function generates a name for PO block IXX
C     By H.J. van Langevelde, JIVE, 010996
C
C     Removed tape section July 22, 2010 RCW.
C     Allow no disk if OBSTYP is NONE or PTVLBA.  Nov. 14, 2011
C     Note that this should also be ok if there are FORMAT=NONE
C     scans in a recording observation (USEDISK true).
C     The PO block is the pass order block, which is basically
C     obsolete.
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
      INCLUDE 'vxlink2.inc'
      INTEGER IXX, KS, ASTAT, ISTA
C ----------------------------------------------------------------------
C
C     Pass order depends on TAPEMODE
C
      KS = POISSET(IXX)
      IF( FORMAT(KS) .EQ. 'S2' ) THEN
         IF( TAPEMODE(KS) .LT. 10 ) THEN
            WRITE( VXNMPO2, '( A, I1, A )' ) 'S2with',
     1          TAPEMODE(KS), 'Groups'
         ELSE
            WRITE( VXNMPO2, '( A, I2, A )' ) 'S2with',
     1          TAPEMODE(KS), 'Groups'
         END IF
      ELSE IF( FORMAT(KS) .EQ. 'LBA' ) THEN
         WRITE( VXNMPO2, '( A )' ) 'LBADummy'
      ELSE
C
C     Check if two heads applies
C     Long tape section removed.
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS) ) 
     4          ASTAT = ISTA
         END DO
C
         IF( USEDISK(ASTAT) .OR. OBSTYP .EQ. 'NONE' .OR.
     1      OBSTYP .EQ. 'PTVLBA' .OR. FORMAT(KS)(1:4) .EQ. 'NONE' ) THEN
            VXNMPO2 = 'DiskVoid'
         ELSE 
            CALL ERRLOG('VXNMPO2: Not using disks at '//
     1           STATION(STANUM(ASTAT))// '!' )
         END IF
      END IF
C
      RETURN
      END
