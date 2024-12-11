      SUBROUTINE BBCWIDAR( KS )
C
C     Routine called by SETBBC to set the BBC assignments for
C     the WIDAR digital backend at the VLA
C
C     This is basically a copy of BBCRDBE, which was based on
C     BBCVLBA.   Started May 9, 2012.
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
     1               'BBCWIDAR:  For WIDAR, all channels must be set ',
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
C
      RETURN
      END
