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
      INTEGER            IUNIT, KS, KF, NNCHAN
      INTEGER            ICH1, ILINE, CRSETC(MAXCHN)
      DOUBLE PRECISION   BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION   LOSUM(MCHAN)
      INTEGER            CRDN
      DOUBLE PRECISION   CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      CHARACTER          CRDS(MCHAN)*1
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
C              We will be concerned only about the main recording 
C              frequencies, not any different frequencies used for
C              reference pointing etc (CRD.. parameters below).
C
               CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
C        
C              Write the frequencies etc for this frequency set.
C        
               IF( NNCHAN .GT. 0 ) THEN
                  WRITE( IUNIT, '( 1X )' )
                  WRITE( IUNIT, '( A )' )
     1               '    BEGIN = FREQUENCY_SET'
                  WRITE( IUNIT, '( 1X )' )
                  ICH1 = 25
                  ILINE = 1
                  CALL LSTFREQ( IUNIT, LOSUM, BBCFREQ, BBCBW, ICH1, 
     1                NNCHAN, ILINE, 1024, 200, .TRUE., .TRUE., 
     2                '        LO_SUM         = ',
     3                '        BBC_FREQUENCY  = ',
     4                '        BANDWIDTH      = ' )
C
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
