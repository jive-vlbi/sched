      CHARACTER*32 FUNCTION VXNMHP( IXX )
C
C     Routine specific for the VEX extension of SCHED
C     function generates a name for HP block IXX
C     for now it is all 14 track, except for S2
C     By H.J. van Langevelde, JIVE, 010996
C
C     Removed tape section July 22, 2010 RCW.
C     Allow OBSTYPE NONE or PTVLBA.  Nov. 14, 2011.
C     Note that the HP block is the head position block, 
C     which is basically obsolete.
C     Dec. 8, 2011.  Note that all observations that are recording
C     data will pass ok, even if there are FORMAT=NONE scans.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C      
      INTEGER IXX, ASTAT
      INTEGER KS, ISTA
C ----------------------------------------------------------------------
C     
C     Different for S2 (meaningless)
C
      KS = HPISSET(IXX)
      IF( FORMAT(KS) .EQ. 'S2' ) THEN
         VXNMHP = 'S2Void'
      ELSE IF( FORMAT(KS) .EQ. 'LBA' ) THEN
         VXNMHP = 'LBAVoid'
      ELSE
C
C     Check if two heads applies
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS))
     4            ASTAT = ISTA
         END DO
C
         IF( USEDISK(ASTAT) .OR. OBSTYP .EQ. 'PTVLBA' .OR.
     1       OBSTYP .EQ. 'NONE' .OR. FORMAT(KS)(1:4) .EQ. 'NONE' ) THEN
            VXNMHP = 'DiskVoid'
         ELSE
            CALL ERRLOG('VXNMHP: Not using disks at '//
     1           STATION(STANUM(ASTAT))// '!' )
         END IF
      END IF
C
      RETURN
      END
