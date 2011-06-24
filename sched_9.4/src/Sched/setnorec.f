      SUBROUTINE SETNOREC( NEEDFMT )
C
C     Set the formats for non-recording scans.
C     Try to match following scans fan out.  That might help
C     avoid a reconfigure.
C     Don't worry about keeping the bit rates constant across
C     stations - that doesn't matter for non-recording scans.
C
C     Called by SETFORM, which has already checked that all 
C     recording scans have formats already set.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER  KS, ISTA, ISCN
      REAL     FANBUFF, TRKBPS
      LOGICAL  NEEDFMT(*), OK
C ----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETNOREC: starting' )
C
C     Loop through stations to find unset formats.
C
      DO ISTA = 1, NSTA
C
C        Go backwards through time to make it easier to match
C        the following scans.
C
         FANBUFF = 0.0
         DO ISCN = NSCANS, 1, -1
C
            IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
C
C              Record the last fanout on a previously set scan.
C
               IF( VLBAMKIV(KS) .AND. .NOT. NEEDFMT(KS) ) THEN
                  FANBUFF = SAMPRATE(KS) / TBPS(KS)
               END IF
C
C              Set the format if needed.  Recall that SETFORM
C              already called SETFMT with TRKBPS set to 0.0 so 
C              any forced formats have been dealt with, including
C              all wide band formats.  Use the same fanout previously
C              determined on a recording scan, but constrain the
C              track bit rate to lie in the allowed range.
C
               IF( VLBAMKIV(KS) .AND. NEEDFMT(KS) .AND. .NOT. 
     1             RECUSED(KS) ) THEN
C
                  IF( FANBUFF .NE. 0.0 ) THEN
                     TRKBPS = SAMPRATE(KS) / FANBUFF
                     TRKBPS = MIN( 8.0, TRKBPS )
                     TRKBPS = MAX( 2.0, TRKBPS )
                     CALL SETFMT( KS, TRKBPS, OK )
                     IF( OK ) NEEDFMT(KS) = .FALSE.
                  END IF
C
               END IF
            END IF
         END DO
C
      END DO
C
C     Set the formats of anything left, if possible.  Again recall
C     that all forcable formats were dealt with on the first 
C     call to SETFMT from SETFORM.  So don't worry about the wide
C     bandwidth cases.
C
      DO KS = 1, NSET
         IF( VLBAMKIV(KS) .AND. NEEDFMT(KS) ) THEN
            IF( SAMPRATE(KS) .GT. 8.0 ) THEN
               TRKBPS = 8.0
            ELSE
               TRKBPS = SAMPRATE(KS)
            END IF
C
            CALL SETFMT( KS, TRKBPS, OK )
            IF( OK ) NEEDFMT(KS) = .FALSE.
C
         END IF
      END DO
C
      RETURN
      END



