      SUBROUTINE VLAPMODE
C
C     Set the VLA phasing mode flags in the setup files.  These are
C     needed to help pick the right IF channels from frequency catalog
C     info.  This is called after by SETFEFS after SETEXPND so there 
C     should only be one station per setup group.  
C
C     Don't worry about whether the VLA is in the scans.  VLAMODE will 
C     most likely only be set on scans when the VLA it is present.
C
C     But do worry about whether this setup applies to the scan in case
C     there are more than one setup for the VLA.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER    KS, ISCN
C ---------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'SETDEFS: Starting' )
C
C     Loop through the setup groups setting the VLA phasing mode flags.
C
      DO KS = 1, NSET
         VLAVA(KS) = .FALSE.
         VLAVB(KS) = .FALSE.
         VLAVR(KS) = .FALSE.
         VLAVL(KS) = .FALSE.
         IF( SETSTA(1,KS) .EQ. 'VLA27' ) THEN
            DO ISCN = 1, NSCANS
               IF( SETNUM(ISCN) .EQ. ISETNUM(KS) ) THEN
                  VLAVA(KS) = VLAVA(KS) .OR. VLAMODE(ISCN) .EQ. 'VA'
                  VLAVB(KS) = VLAVB(KS) .OR. VLAMODE(ISCN) .EQ. 'VB'
                  VLAVR(KS) = VLAVR(KS) .OR. VLAMODE(ISCN) .EQ. 'VR'
                  VLAVL(KS) = VLAVL(KS) .OR. VLAMODE(ISCN) .EQ. 'VL'
               END IF
            END DO
         END IF
      END DO
C
      RETURN
      END
