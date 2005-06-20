      SUBROUTINE VLBATI( ISCN, ISTA, FIRSTS, DOTAPE, POSTPASS, REWSP, 
     1                   DOSET, TPCDRIV, TPCDIR, TPCHEAD, TPCINDX, 
     2                   PASSOK, LASTDY, LSTOP )
C
C     Routine for SCHED, called by VLBA, that gets tape information,
C     flags a tape change, and keeps track of whether a pass constitutes
C     a viable postpass.
C
C     This routine is only called if tape is being used.
C
      INCLUDE 'sched.inc'
C
      INTEGER           ISCN, ISTA, LASTDY, LTPCDIR
      INTEGER           DOY1
      LOGICAL           PPOK, POSTPASS, FIRSTS
      DOUBLE PRECISION  LSTOP, FRACD1
      LOGICAL           PASSOK, DOSET
      CHARACTER         REWSP*7, TSTART*9, TFORM*15
      CHARACTER         VLBAD1*9, HUMAND1*16
C
C     Tape information from TPDAT.
C
      INTEGER           TPPASS, TPCDIR, TPCINDX, TPCHEAD, TPCDRIV
      LOGICAL           DOTAPE, DOFAST, DOREW, RUNNING
C
      SAVE              LTPCDIR, RUNNING
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'VLBATI: Starting.' )
C
C     Extract the tape commands from PTDAT. 
C     a tape change, add comment to schedule.
C
      CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, DOFAST,
     1             DOREW, TPPASS, TPCDRIV, TPCDIR, TPCINDX, TPCHEAD )
C
C     If we aren't actually using tape (eg Mark5A), return after 
C     getting the above information.
C
      IF( .NOT. USETAPE(ISTA) ) RETURN
C
C     Get head position and what to do with the tape if there is a 
C     setup or NOREC scan.  Don't rewind on tape changes.
C
      IF( DOTAPE ) THEN
         REWSP = 'STOP'
      ELSE IF( DOREW ) THEN
         REWSP = '-REWIND'
      ELSE IF( DOFAST ) THEN
         REWSP = '+REWIND'
      ELSE
         REWSP = 'STOP'                  
      END IF
C     
C     Keep running track of whether the it would be safe to UNLOAD
C     a tape rather than POSTPASS it if there is a tape change
C     at the moment.  It is safe to unload if the tape has run
C     from end to beginning without stopping.  PPOK is the 
C     indicator that it is currently safe to unload.  PASSOK is
C     the indicator that this pass is not yet disqualified.
C     
      IF( FIRSTS ) THEN
         LTPCDIR = 0
         PASSOK = .FALSE.
         RUNNING = .FALSE.
      END IF
C     
C     First determine if it is safe to unload - the tape is at
C     the beginning and has not stopped since the other end.
C     
      PPOK = TPFOOT1(ISCN,ISTA) .LT. 300.0 .AND. PASSOK
C     
C     Now see what this scan should do to PASSOK - which won't be
C     used again until the next scan.
C     
C     A setup scan or norecord scan will disqualify the pass.
C     However later it might be reset to ok if it is the start
C     of the pass.  Also recording in the forward direction is
C     an automatic disqualifier.
C     
      IF( DOSET .OR. NOREC(ISCN) .OR. TPCDIR .EQ. 1 ) THEN
         PASSOK = .FALSE.
      END IF
C     
C     Now, see if this is the start of a reverse pass and the tape
C     is at the end.  If so, set PASSOK to true.
C     
      IF( TPCDIR .EQ. -1 .AND. LTPCDIR .EQ. 1 .AND. 
     1    TPFOOT1(ISCN,ISTA) .GT. TPLENG(ISTA) * 0.95 ) THEN
         PASSOK = .TRUE.
      END IF
C     
C     Save what last pass did.
C     
      LTPCDIR = TPCDIR
C     
C     Deal with a tape change.
C     Deal with the unused tape drive if necessary.  VLBACHG Will
C     issue STOP, UNLOAD, POSTPASS or whatever commands.
C     A short extra scan will be created to stop the tape before
C     an UNLOAD or POSTPASS.
C     
      IF( DOTAPE .AND. .NOT. AUTOALOC(ISTA) ) THEN
         WRITE( IUVBA, '( 1X, /, A )' )
     1       '!* ============= Change Tape ============ *!'
         IF( PPOK ) THEN
            WRITE( IUVBA, '( A )' )
     1         '!*   There were no scheduled tape stops in the ' //
     2         'last pass.  *!'
            WRITE( IUVBA, '( A )' )
     1         '!*   If it was not interrupted, a POSTPASS is ' //
     2         'not needed.  *!'
         END IF
         CALL VLBACHG( ISCN, ISTA, FIRSTS, DOTAPE, POSTPASS, 
     1              LASTDY, LSTOP, TPCDRIV, PPOK )
         RUNNING = .FALSE.
      END IF
C     
C     Add a comment about when the next tape after a change
C     starts.
C     
      IF( .NOT. ( AUTOALOC(ISTA) .OR. RUNNING .OR. 
     1    NOREC(ISCN) ) ) THEN
         CALL SCHDAY( STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     1                VLBAD1, HUMAND1, DOY1, FRACD1 )
         TSTART = TFORM( FRACD1*TWOPI, 'T', 0, 2, 2, 'hms' )
         WRITE( IUVBA, '( 3A )' )
     1       '!* ========= New tape starts at: ', TSTART, 
     2       ' ========= *!'
         RUNNING = .TRUE.
      END IF
C
      RETURN
      END
