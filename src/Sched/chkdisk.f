      SUBROUTINE CHKDISK( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED that checks some generic disk specific
C     setup parameters.  There isn't much here yet.
C
C     This routine is only called if USEDISK(ISTA) is set.
C       KS is setup group
C       ERRS indicates an error was found and SCHED should die after
C            other checks are done.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER  KS, ISTA, NTRACKS
      LOGICAL  ERRS
C -----------------------------------------------------------------
C     Get the station number.
C
      ISTA = ISCHSTA(ISETSTA(KS))
C
C     Don't allow less than 8 tracks for Mark5A.
C     But don't complain if not recording with this setup.
C     Don't worry about non-recording setups - like pointing.
C
      IF( DISK(ISETSTA(KS)) .EQ. 'MARK5A' .AND. RECUSED(KS) ) THEN
         NTRACKS = NCHAN(KS) * FANOUT(KS) * BITS(1,KS)
         IF( NTRACKS .LT. 8 ) THEN
            CALL WLOG( 1, 'CHKDISK:  Less than 8 tracks for '//
     1         'Mark5A at '// SETSTA(1,KS) )
C
C           We used to just issue warnings.  But the padding with
C           extra channels seems to have gone away.  These cases
C           will be rare and the old system is slated for removal,
C           so just make it an error and request a fix.
C
C            CALL WLOG( 1, 
C     1         '          On VLBA systems, dummy data will '//
C     2         'be added to make 8.  This will use extra disk. ')
            CALL WLOG( 1, 
     1            '          Note ntracks = nchan * fanout * bits)' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, I3, A, F4.1, A, I2 )' )
     1         '                Tracks=', NTRACKS, 
     2         '  NCHAN=', NCHAN(KS), 
     3         '  FANOUT=', FANOUT(KS),
     4         '  BITS=', BITS(1,KS)
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, 
     1          '          Please adjust one or more of these '//
     2          'parameters to get at least 8 tracks and try again.' )
C
C           Walter doesn't want this to be an error, just a warning.
C           Changed his mind later.
C
            ERRS = .TRUE.
         END IF
      END IF   
C
      RETURN
      END
