      SUBROUTINE CHKDISK( KS, ERRS )
C
C     Routine for SCHED that checks some generic disk specific
C     setup parameters.  There isn't much here yet.
C
C     This routine is only called if USDISK(ISTA) is set.
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
C
      IF( DISK(ISETSTA(KS)) .EQ. 'MARK5A' ) THEN
         NTRACKS = NCHAN(KS) * FANOUT(KS) * BITS(1,KS)
         IF( NTRACKS .LT. 8 ) THEN
            CALL WLOG( 1, 'CHKDISK: ****** Too few tracks for '//
     1         'Mark5A at '// SETSTA(1,KS) )
            CALL WLOG( 1, 
     1         '                Minimum is 8 (chan*fanout*bits)' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, I3, A, F4.1, A, I2 )' )
     1         '                Tracks=', NTRACKS, 
     1         '  NCHAN=', NCHAN(KS), 
     2         '  FANOUT=', FANOUT(KS),
     3         '  BITS=', BITS(1,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END IF   
C
      RETURN
      END
