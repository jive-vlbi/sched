      SUBROUTINE OMSFREQ( KS, IUNIT )
C
C     Subroutine for SCHED called by OMSSET that writes frequency
C     group information to the OMS file.  This was requested by
C     Lorant Sjouwerman to aid in setting which VLA IF various
C     channels come from to assign calibration information to channels.
C
C     The code is stolen shamelessly from PRTFREQ.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER            I, IUNIT, KS, KF, NNCHAN
      REAL               BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION   LOSUM(MCHAN)
C----------------------------------------------------------------------
      NNCHAN = NCHAN(KS)
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, 3I7 )' ) 'OMSFREQ starting. ', 
     1       NNCHAN, NFSET, MAXPC
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     List the frequency sets.  Don't worry about all the error
C     checking that is in PRTFREQ.
C
      IF( NFSET .GT. 0 ) THEN
         DO KF = 1, NFSET
            IF( FSETKS(KF) .EQ. KS ) THEN
C
C              Get the frequencies to be used for this frequency set.
C
               CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C        
C              Write the frequencies etc for this frequency set.
C        
               IF( NNCHAN .GT. 0 ) THEN
                  WRITE( IUNIT, '( 1X )' )
                  WRITE( IUNIT, '( A )' )
     1               '    BEGIN = FREQUENCY_SET'
                  WRITE( IUNIT, '( 1X )' )
                  WRITE( IUNIT, '( A, 64( F10.2, : ) )' )
     1               '        LO_SUM         = ', 
     2               ( LOSUM(I), I  =1, NNCHAN )
                  WRITE( IUNIT, '( A, 64( F10.2, : ) )' )
     1               '        BBC_FREQUENCY  = ', 
     2               ( BBCFREQ(I), I = 1, NNCHAN )
                  WRITE( IUNIT, '( A, 64( F10.3, : ) )' )
     1               '        BANDWIDTH      = ', 
     2               ( BBCBW(I), I = 1, NNCHAN )
                  WRITE( IUNIT, '( 1X )' )
                  WRITE( IUNIT, '( A )' )
     1               '    END = FREQUENCY_SET'
               END IF
            END IF
         END DO
      END IF
C
      RETURN
      END
