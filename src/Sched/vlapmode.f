      SUBROUTINE VLAPMODE
C
C     Set the VLA phasing mode flags in the setup files.  These are
C     needed to help pick the right IF channels from frequency catalog
C     info.  This is called after by SETFEFS after SETEXPND so there 
C     should only be one station per setup group.  
C
C     Be sure that the VLA is in the scans being tested.  Otherwise 
C     the VLAMODE might not have been set to an appropriate value by the 
C     user - like pointing scans, and the test will be relative to the
C     VLAMODE of the previous scan.
C
C     But do worry about whether this setup applies to the scan in case
C     there are more than one setup for the VLA.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER    KS, ISCN, ISTA, JSTA, NPMODE
      LOGICAL    DOTEST
C ---------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'SETDEFS: Starting' )
C
C     Loop through the setup groups setting the VLA phasing mode flags.
C
      JSTA = 0
      DO ISTA = 1, NSTA
         IF( STANAME(ISTA)(1:3) .EQ. 'VLA' ) JSTA = ISTA
      END DO
C
      DO KS = 1, NSET
C
C        Initialize the flags for mode used.  Do for all setups.
C
         VLAVA(KS) = .FALSE.
         VLAVB(KS) = .FALSE.
         VLAVR(KS) = .FALSE.
         VLAVL(KS) = .FALSE.
C
C        Now for VLA setups, set the flags.
C
         IF( JSTA .NE. 0 ) THEN         
            IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' ) THEN
               DO ISCN = 1, NSCANS
                  IF( SETNUM(ISCN) .EQ. ISETNUM(KS) .AND. 
     1                STASCN(ISCN,JSTA) ) THEN
                     VLAVA(KS) = VLAVA(KS) .OR. VLAMODE(ISCN) .EQ. 'VA'
                     VLAVB(KS) = VLAVB(KS) .OR. VLAMODE(ISCN) .EQ. 'VB'
                     VLAVR(KS) = VLAVR(KS) .OR. VLAMODE(ISCN) .EQ. 'VR'
                     VLAVL(KS) = VLAVL(KS) .OR. VLAMODE(ISCN) .EQ. 'VL'
                  END IF
               END DO
           END IF
         END IF
C
C        Discourage use of more than one phasing mode per setup.
C
         NPMODE = 0
         IF( VLAVA(KS) ) NPMODE = NPMODE + 1
         IF( VLAVB(KS) ) NPMODE = NPMODE + 1
         IF( VLAVR(KS) ) NPMODE = NPMODE + 1
         IF( VLAVL(KS) ) NPMODE = NPMODE + 1
         IF( NPMODE .GE. 2 ) THEN
            CALL WRTMSG( 1, 'VLAMPODE', 'warn2phasemode' )
         END IF
      END DO
C
      RETURN
      END
