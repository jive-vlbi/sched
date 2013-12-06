      SUBROUTINE SETFIRM
C
C     Routine called by SETDEF that sets DBE if needed.
C
C     This was separated from CHKFIRM on Oct 31, 2013 for logical
C     cleanliness in SCHED where I like to separate the settings
C     from the checking.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER   KS, KSTA
C ----------------------------------------------------------------
C
C     Loop over the setup files which are now one per station (after
C     SETEXPND).
C
      DO KS = 1, NSET
         KSTA = ISETSTA(KS)
C
C        Note that all VLBA stations (all RDBE stations?) will be
C        RDBE2.  Somewhere else, protect against using the PFB with
C        more than one RDBE.
C
         IF( DBE(KS) .EQ. ' ' ) THEN
C
C           For the RDBE, we need to choose PFB or DDC.  If the DDC
C           can do it, use that.  If not, use PFB.  That will be based
C           on the number of channels.  The PFB also depends on lots
C           of other restrictions too, but let the check routines 
C           deal with that.
C  
C           CR: DBBC defaults to DDC personality 
C
C           If none of the specific cases are met, assume we don't 
C           need DBE and leave it blank.
C
            IF( DAR(KSTA)(1:4) .EQ. 'RDBE' .AND. NCHAN(KS) .GT. 8 ) THEN
               DBE(KS) = 'RDBE_PFB'
            ELSE IF( DAR(KSTA)(1:4) .EQ. 'RDBE' ) THEN
               DBE(KS) = 'RDBE_DDC'
            ELSE IF( DAR(KSTA) .EQ. 'DBBC' ) THEN
               DBE(KS) = 'DBBC_DDC'
            ELSE IF( DAR(KSTA) .EQ. 'WIDAR' ) THEN
               DBE(KS) = 'WIDAR'
            END IF
         END IF
      END DO
      RETURN
      END
