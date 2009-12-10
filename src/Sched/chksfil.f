      SUBROUTINE CHKSFIL
C
C     Routine for SCHED called by DEFSET that checks parameters that
C     shouldn't vary between setup groups within a file.
C
C     For now, only the speed up factor and samplerate are checked.
C
C     The speedup factor does not need to be the same for some correlators
C     so make that check correlator specific.  The same will be true
C     of the sample rate for DiFX in a few months, although there will
C     need to be factor of 2 relationships.  Dec. 3, 2009 RCW
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER  ISETF, KS, RSAMP(MAXSET)
      LOGICAL  DOCHKSU
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETSFIL: Starting.' )
C
C     Initialize the variables that are to be set.
C
      DO ISETF = 1, NSETF
         FSPEED(ISETF) = 0.0
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
         DOCHKSU = FORMAT(KS) .NE. 'S2' .AND. FORMAT(KS) .NE. 'NONE' 
     1       .AND. FORMAT(KS) .NE. 'MARKII' 
         DOCHKSU = DOCHKSU .AND. ( 
     1       CORREL .EQ. 'SOCORRO' .OR.
     2       CORREL .EQ. 'VLBA' )
         IF( DOCHKSU ) THEN
            IF( SPEEDUP(KS) .NE. FSPEED(ISETF) .AND. 
     1          FSPEED(ISETF) .NE. 0.0 ) THEN
               CALL WLOG( 1, 'CHKSFIL: Two setup groups in the same ' //
     1          'setup file have different speedup factors.' )
               CALL WLOG( 1, '         This cannot be correlated on ' //
     1          'the '// CORREL // ' correlator.' )
               CALL ERRSET( KS )
            END IF
            FSPEED(ISETF) = SPEEDUP(KS)
         ELSE
            FSPEED(ISETF) = 1.0
         END IF
C
C        Now do the same thing for the sample rate.
C          
         IF( FORMAT(KS) .NE. 'NONE' .AND. 
     1       FORMAT(KS) .NE. 'MARKII' ) THEN
            IF( SAMPRATE(KS) .NE. RSAMP(ISETF) .AND. 
     1          RSAMP(ISETF) .NE. 0.0 ) THEN
               CALL WLOG( 1, 'CHKSFIL: Two setup groups in the ' //
     1          'same setup file have different sample rates.' )
               CALL ERRSET( KS )
            END IF
            RSAMP(ISETF) = SAMPRATE(KS)
         END IF
C
      END DO
C
      RETURN
      END
