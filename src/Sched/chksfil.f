      SUBROUTINE CHKSFIL
C
C     Routine for SCHED called by DEFSET that checks parameters that
C     shouldn't vary between setup groups within a file.
C
C     For now, only the speed up factor and samplerate are checked.
C     In fact, even the speedup factor is no longer needed by any 
C     known correlators.  The speedup factor is really not a relevant
C     concept for the disk based recordings.  They can play back
C     at arbitrary speeds.  So take out that check.  Jan. 11 2011.  RCW.
C
C     Allow the mixed bandwidth mode where several channels from one
C     station play against one from another station.  Don't allow
C     FREQ and DOPPLER when this happens as it becomes ambiguous.
C
C     Older comment, reworded:  The speedup factor only mattered for some
C     correlators.  The sample rate won't matter for DiFX eventually,
C     but leave that for now.  Dec. 3, 2009 RCW
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER  ISETF, KS, RSAMP(MAXSET)
C         LOGICAL  DOCHKSU
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETSFIL: Starting.' )
C
C     Initialize the variables that are to be set.
C     These are used elsewhere so set them even if not using them.
C
      DO ISETF = 1, NSETF
         FSPEED(ISETF) = 1.0
         RSAMP(ISETF) = 0.0
      END DO
C
C     Loop through the setup groups that are used.
C
      DO KS = 1, NSET
C
C        Get the input setup file that this group was in.
C
         ISETF = ISETNUM(KS)
C
C        Get the speedup factor for the input setup file and check 
C        against any previous groups in this setup file to be 
C        sure it doesn't change.  If there was no previous group 
C        the speedup factor will be zero.
C
C        Don't do the test for S2 at the moment.  The implications
C        are unclear.  Work on this.
C
C        Dec 2009 - start restricting these tests because the new
C        correlator, especially DiFX aren't subject to them.
C
C         DOCHKSU = FORMAT(KS) .NE. 'S2' .AND. FORMAT(KS) .NE. 'NONE' 
C     1       .AND. FORMAT(KS) .NE. 'MARKII' 
C
C        CORREL FXCORR has been turned off, so this won't get done.
C
C         DOCHKSU = DOCHKSU .AND. CORREL .EQ. 'FXCORR' 
C         IF( DOCHKSU ) THEN
C            IF( SPEEDUP(KS) .NE. FSPEED(ISETF) .AND. 
C     1          FSPEED(ISETF) .NE. 0.0 ) THEN
C               CALL WLOG( 1, 'CHKSFIL: Two setup groups in the same ' //
C     1          'setup file have different speedup factors.' )
C               CALL WLOG( 1, '         This cannot be correlated on ' //
C     1          'the '// CORREL // ' correlator.' )
C               CALL ERRSET( KS )
C            END IF
C            FSPEED(ISETF) = SPEEDUP(KS)
C         ELSE
C            FSPEED(ISETF) = 1.0
C         END IF
C
C        Now do the same thing for the sample rate.
C        We now need to allow for multiple bands at one station playing
C        against 1 band at another.  That means that this needs to be
C        turned into a warning, not an error.  Also, I'd like to indicate
C        the amount of overlap.  Finally, note that later code (OKXC) will
C        prevent DOPPLER and FREQ from being used in the main schedule
C        because the meaning of channels becomes ambiguous.
C          
         IF( FORMAT(KS) .NE. 'NONE' .AND. 
     1       FORMAT(KS) .NE. 'MARKII' ) THEN
            IF( SAMPRATE(KS) .NE. RSAMP(ISETF) .AND. 
     1          RSAMP(ISETF) .NE. 0.0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, A, I3, 2A )' )
     1             'CHKSFIL: Setup group, ', KS, ' in setup file ',
     2             ISETF, 
     3             ' does not have the same sample rate as ',
     4             ' other groups in the file.'
               CALL WLOG( 1, MSGTXT )
               CALL WRTMSG( 1, 'CHKSFIL', 'MismatchedSamprate' )
            END IF
            RSAMP(ISETF) = SAMPRATE(KS)
         END IF
C
      END DO
C
      RETURN
      END
