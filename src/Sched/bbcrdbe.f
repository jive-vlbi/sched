      SUBROUTINE BBCRDBE( KS )
C
C     Routine called by SETBBC to set the BBC assignments for
C     the RDBE digital backend.
C
c     The RDBE doesn't have an obvious BBC numbering scheme
C     like the legacy system.  But I think it
C     is ok to simply assign a "BBC" number equal to the
C     channel number.  This should work for DAR=RDBE and
C     DAR=RDBE2
C
C     Based on BBCVLBA.
C
C     Updatedd a bit for the DDC  Nov. 7, 2012.  RCW
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
      INCLUDE    'schfreq.inc'
C
      INTEGER    ICH, JCH, KS ! , MMBBC
C --------------------------------------------------------------
C
C     Use the hard wired freqeuncies.
C
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .EQ. 0 ) BBC(ICH,KS) = ICH
      END DO
C
C     Check for duplicates in case the user assigned some.
C
      IF( NCHAN(KS) .GE. 2 ) THEN
         DO ICH = 2, NCHAN(KS)
            DO JCH = 1, ICH -1
               IF( BBC(ICH,KS) .EQ. BBC(JCH,KS) ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' )
     1               'BBCRDBE:  For RDBE, all channels must be set ',
     2               'to different BBCs.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, 2I5, A, I5 )' )
     1               '          Channels ', ICH, JCH, ' both use BBC ',
     2               BBC(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  CALL ERRLOG( 'Fix BBC assignments' )
               END IF
            END DO
         END DO
      END IF
      RETURN
      END
