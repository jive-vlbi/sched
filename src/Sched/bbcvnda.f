      SUBROUTINE BBCVNDA( KS )
C
C     Routine called by SETBBC to set the BBC assignments for
C     the new VNDA system.
C
c     Similarily to the RDBE, VNDA doesn't have an obvious BBC 
C     numbering scheme like the legacy system. Since starting 
C     with the RDBE the BBC assingments are virtual, it is ok  
C     to start at zero and count up. This should work for 
C     DAR=VNDA.
C
C     Based on BBCRDBE.
C
C     Jul. 2, 2024.  Adriana Escobar (AED)
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
     1               'BBCVNDA:  For VNDA, all channels must be set ',
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
