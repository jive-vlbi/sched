      SUBROUTINE BBCCDAS( KS )
C
C     Make the BBC assignments for the CDAS.
C
C     For now, require the user set them.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER      KS, ICH
C-------------------------------------------------------------
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .LE. 0 ) THEN
            CALL WLOG( 1, 
     1         'BBCCDAS:  The user must set the BBC assignments ' //
     2         'for any CDAS stations.' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I4 )' )
     1         '           They are missing in setup ', KS
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
C
C     See the attachments on my email to Cormac on Dec. 16, 2013 for
C     some more information.  It seems that BBCs 1-4 are attached to
C     IF1, BBCs 5-8 see IF2, BBCs 9-12 see IF3 and BBCs 13-16 see
C     IF4.  I don't yet know the IF names.
C
      RETURN
      END
