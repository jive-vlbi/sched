      SUBROUTINE RECCTL
C
C     Set the system dependent defaults for PRESTART and MINPAUSE.
C     This routine for SCHED is called by DEFAULTs.
C
C     There is some discussion of what to do here.  For most of the 
C     modern systems, setting both to zero is appropriate.  But some
C     systems need some time to get started and synced.  We're still
C     debating the best course.  It will likely depend on the control
C     system and recorder.
C
C     Note that the field system has a built-in 10s minimum pause that
C     is apparently controllable in the VEX file (but I haven't found
C     the parameter).  This might serve to apply MINPAUSE there without
C     subjecting systems that don't need it to the offset start time.
C
C     So SCHED could just not worry, but then the disk space
C     calculations could be off.  Perhaps I could account for this at
C     the time of the space calculation.  It could be done by station
C     there, but not here (MINPAUSE is global in the scan).
C
C     Apparently MARKIV systems need about 4 seconds to get going.
C     See email from Ed Himwich Dec 30, 2013.  DBBC systems and VLBA5
C     did not take a significant time.
C
C     It seems that the direction is toward not needing these parameters
C     so make that the default, although acknowledge the field system
C     10 sec internal MINPAUSE.
C

C  The following is meant to be temporary until we decide what to do.
C     For now
C     Set both to zero for the RDBE/VLBA, WIDAR and RDBE with control 
C     systems (eg GBT).  For the DBBC, use PRESTART=0, but acknowledge
C     the defacto MINPAUSE=10.  So here is the desired outcome from
C     an email from RCW on Dec. 19, 2013:
C     PRESTART = 0 if only the following combinations are present:
C           DAR=RDBE
C           DAR=WIDAR
C           DAR=DBBC
C           DAR=LBA
C     PRESTART = 5 if anything else is present
C 
C     MINPAUSE = 0 if only the following combinations are present:
C           DAR=RDBE  CONTROL=VLBA
C           DAR=WIDAR
C           DAR=LBA
C     MINPAUSE = 10 if anything else, including DBBC, is present.
C
C     Note that MINPAUSE and PRESTART influence the global scan 
C     start time so they are common to all antennas.  They are 
C     allowed to vary from scan to scan.  Note that with the VLBA,
C     the data good time from VEX is used as the start time of 
C     recordings so these parameters don't have much effect.  The
C     Field System uses the nominal start time.
C
C     Routine started Dec. 2013  RCW.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, KSTA
      LOGICAL    USEPS5, USEMP10
C     LOGICAL    USE0
C --------------------------------------------------------------------------
C     Note that PRESTART and MINPAUSE are experiment wide so use the
C     highest wanted by any of the systems.
C
      USEPS5 = .FALSE.
      USEMP10 = .FALSE.
      DO ISTA = 1, NSTA
         KSTA = STANUM(ISTA)
C
C        Only use 5 second prestart for VLBA legacy system and
C        Mark IV.
C
         IF( DAR(KSTA) .EQ. 'VLBA' .OR. DAR(KSTA) .EQ. 'MKIV' )
     1       USEPS5 = .TRUE.
C
C        Only use 10 second minpause for the field system and
C        the VLBA legacy system.
C
         IF( ( CONTROL(KSTA) .EQ. 'VEX' .AND. DAR(KSTA) .NE. 'WIDAR' )
     1       .OR. DAR(KSTA) .EQ. 'VLBA' )  USEMP10 = .TRUE.
C
      END DO

C
C     Actually set the default values.
C
      DO ISCN = SCAN1, SCANL
         IF( USEPS5 ) THEN
            IF( PRESTART(ISCN) .EQ. UNSET * ONESEC ) 
     1            PRESTART(ISCN) = 5.D0 * ONESEC
         ELSE
            IF( PRESTART(ISCN) .EQ. UNSET * ONESEC ) 
     1          PRESTART(ISCN) = 0.D0
         END IF
         IF( USEMP10 ) THEN
            IF( MINPAUSE(ISCN) .EQ. UNSET * ONESEC ) 
     1            MINPAUSE(ISCN) = 10.D0 * ONESEC
         ELSE
            IF( MINPAUSE(ISCN) .EQ. UNSET * ONESEC ) 
     1          MINPAUSE(ISCN) = 0.D0
         END IF
      END DO
C
      RETURN
      END
