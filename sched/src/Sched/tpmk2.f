      SUBROUTINE TPMK2( ISCN, ISTA, LASTISCN )
C
C     Obsolete - only called if tapes in use (Mark II no less)
C
C
C     Routine for SCHED called by TPTAPE that triggers Mark II tape
C     changes.
C
C     Tape changes are based on elapsed wall clock time.
C
C     The earliest start time of the whole experiment was determined in
C     SCHTIM and is called TFIRST
C
C     AUTOTAPE (the default) causes tape changes every TPLEN (def 4hr).
C     The reference time is, in order of priority, the time of the last
C     TAPE command for the station, the TPTIME for the station from the 
C     TAPEINI input (if > 0), TPREF (if > 0), or the time of the 
C     earliest scan in the schedule (TFIRST). 
C
C     If a tape change is needed in the middle of a scan, it will be 
C     requested at the beginning and the next TPLEN interval will
C     be measured from that scan start time.  Note that this can cause
C     non-simultaneous tape changes if it occurs on some stations and
C     not others. 
C
      INCLUDE 'sched.inc'
C
      INTEGER            ISCN, ISTA, LASTISCN(MAXSTA)
      DOUBLE PRECISION   TLAST(MAXSTA), TP1(MAXSTA), ONEMIN
      LOGICAL            FIRSTS, DOTAPE
      PARAMETER          (ONEMIN = 1.D0/1440.D0)
      SAVE               TLAST, TP1
C --------------------------------------------------------------------
      FIRSTS = LASTISCN(ISTA) .EQ. 0
      DOTAPE = .FALSE.
      IF( AUTOTAPE .AND. FIRSTS ) THEN
C
C        Get reference time for first tape change.
C        Tape change reference times are only specified in hours.
C        They are assumed to apply to a time in the 24 hours 
C        preceeding the run. 
C
         IF( TPTIME(ISTA) .GE. 0.D0 ) THEN
C
C           Base tape changes on station specific reference time.
C
            TP1(ISTA) = DINT( TFIRST - 1.D0 ) + 
     1                     (TPTIME(ISTA) / 86400.)
C
         ELSE IF( TPREF .GE. 0.D0 ) THEN
C
C           Base tape changes on experiment wide reference time that
C           is before TFIRST.
C
            TP1(ISTA) = DINT( TFIRST - 1.D0 ) + ( TPREF / 86400. )
C
         ELSE
C
C           Base tape changes on start of first scan.
C
            TP1(ISTA) = TFIRST
C
         END IF
C
C        Now get the psuedo start time of the first tape. 
C
         TLAST(ISTA) = TP1(ISTA) + 
     1               TPLEN * DINT( (STARTJ(ISCN) - TP1(ISTA) ) / TPLEN )
C
C        Also specify a tape change on first scan.
C
         DOTAPE = .TRUE.
      END IF
C
C     Arbitrary scans - check if tape change needed.
C
      IF( TAPE(ISCN,ISTA) ) THEN
         DOTAPE = .TRUE.
         TLAST(ISTA) = STARTJ(ISCN) 
      END IF
      IF( AUTOTAPE .AND. STOPJ(ISCN) .GT. 
     1    TLAST(ISTA) + TPLEN + ONEMIN ) THEN
         DOTAPE = .TRUE.
C
C        Deal with case where "every 4 hour" tape change came during
C        a data gap.
C
         IF( STARTJ(ISCN) .LE. TLAST(ISTA) + TPLEN + ONEMIN ) THEN
            TLAST(ISTA) = STARTJ(ISCN)
         ELSE
            TLAST(ISTA) = TLAST(ISTA) + TPLEN * 
     1           DINT( (STARTJ(ISCN) - TLAST(ISTA)) / TPLEN )
         END IF
      END IF
C
C     Put the tape change request into TPDAT.
C
      CALL TPPACK( 'PACK', TPDAT(1,ISCN,ISTA), DOTAPE, .FALSE.,
     1             .FALSE., 1, 1, 1, 1, 1 )
C
      RETURN
      END

