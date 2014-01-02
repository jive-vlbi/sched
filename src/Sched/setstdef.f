      SUBROUTINE SETSTDEF( KS )
C
C     Routine for SCHED called by SETEXPND that sets the stations in
C     a setup file prior to the expansion into single station groups.
C     If station = ' ', this routine looks for all stations that
C     use this setup and adds them to the list.  If stations are
C     specified, it eliminates the ones that are not in the schedule
C     (this helps avoid having excessive groups for, for example,
C     VLBA only projects that use lots of setups).
C
C     At the time this routine is run, the automatic pointing scan
C     insertion has not happened.  Therefore we will need to assume
C     that any setups that might be invoked by that process actually
C     are used.
C
C     Expand 'VLBA' to be the full list of VLBA stations. (Dec2004)
C
      INCLUDE      'sched.inc'
      INCLUDE      'schset.inc'
      INCLUDE      'schpeak.inc'
C
      INTEGER     KS, JS, ISCN, ISTA, KSTA, NOUT
      INTEGER     ANTKS(MANT)
      LOGICAL     ANTFILE(MANT)
      CHARACTER   NEWSTA(2*MANT)*8
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETSTDEF: Starting.' )
C
      DO NOUT = 1, MANT
         NEWSTA(NOUT) = ' '
      END DO
C
C     First find the setup group within this setup file where any
C     specifically mentioned stations are located.
C     It doesn't matter if the other groups have had their defaults
C     expanded yet, I don't think.  First look for a generic VLBA
C     specification, then for individual stations.  That puts any
C     VLBA stations with specific requests in the right place.
C     Include this group in the search.
C
      DO ISTA = 1, NSTA
         ANTKS(ISTA) = 0
         DO JS = 1, NSET
            IF( ISETNUM(JS) .EQ. ISETNUM(KS) ) THEN
               DO KSTA = 1, MANT
                  IF( STANAME(ISTA)(1:4) .EQ. 'VLBA' .AND. 
     1                  SETSTA(KSTA,JS) .EQ. 'VLBA' ) 
     2                ANTKS(ISTA) = JS
                  IF( STANAME(ISTA) .EQ. SETSTA(KSTA,JS) ) 
     1                ANTKS(ISTA) = JS
               END DO
            END IF
         END DO
      END DO
C
C     If no station was specified, that means use this for any 
C     stations not explicitly covered elsewhere.  Go through and 
C     set ANTKS to this group for all as-yet unspecified stations.
C
      IF( SETSTA(1,KS) .EQ. ' ' ) THEN
         DO ISTA = 1, NSTA
            IF( ANTKS(ISTA) .EQ. 0 ) ANTKS(ISTA) = KS
         END DO
      END IF
C
C     Now go through and eliminate stations that obviously did not
C     use this setup group.  Do by determining if each station might
C     have used this setup file, and then correlating with the ANTKS.
C
C     Loop over stations in the schedule, then scans (for simple 
C     schedules, this will be quick as we won't get past the 
C     first scan or first few scans.
C
      DO ISTA = 1, NSTA
         ANTFILE(ISTA) = .FALSE.
         DO ISCN = 1, NSCANS
            IF( STASCN(ISCN,ISTA) .AND. 
     1          SETNUM(ISCN) .EQ. ISETNUM(KS) ) THEN
               ANTFILE(ISTA) = .TRUE.
               GO TO 100
            END IF
         END DO
  100    CONTINUE
      END DO
C
C     Now check any stations that might get added by the automatic
C     pointing scan insertions.  Utilize PKGROUP that was set up
C     in RDPEAK and PKFINISH to simplify this sort of thing.  Test
C     for both the continuum and line setup files.
C
      IF( DOPOINT .AND. NPKGRP .GT. 0 ) THEN
         DO ISTA = 1, NSTA
            IF( PKGROUP(ISTA) .NE. 0 ) THEN
               IF( PKLSET(PKGROUP(ISTA)) .EQ. ISETNUM(KS) .OR. 
     1             PKLSETL(PKGROUP(ISTA)) .EQ. ISETNUM(KS) ) THEN
                  ANTFILE(ISTA) = .TRUE.
               END IF
            END IF
         END DO
      END IF
C
C     We now have a complete list of stations using this setup file.
C     Create the NEWSTA list.
C
      NOUT = 0
      DO ISTA = 1, NSTA
         IF( ANTFILE(ISTA) .AND. ANTKS(ISTA) .EQ. KS ) THEN
            NOUT = NOUT + 1
            NEWSTA(NOUT) = STANAME(ISTA)
         END IF
      END DO
C
C     Now replace the station list with the new one.  Note that
C     all will be blank if no used stations were found.  NEWSTA
C     may have had stations eliminated above so not all elements
C     should be used.  First reinitialize the SETSTA.
C     Strictly speaking, I didn't need to go through NEWSTA.  That
C     was left from an older and much more complicated version of
C     this routine - before giving each VLBA station a group.
C     It may be useful again some day.
C
      DO ISTA = 1, MANT
         SETSTA(ISTA,KS) = ' '
      END DO
C
C     Then transfer NEWSTA
C
      DO ISTA = 1, NOUT
         SETSTA(ISTA,KS) = NEWSTA(ISTA)
      END DO
C
      RETURN
      END


