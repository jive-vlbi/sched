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
      LOGICAL  ERRS, WARNED
      DATA     WARNED / .FALSE. /
      SAVE     WARNED
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
            IF( .NOT. WARNED ) THEN
               CALL WLOG( 1, 
     1            '          On VLBA systems, dummy data will '//
     2            'be added to make 8.  This will use extra disk. ')
               CALL WLOG( 1, 
     1            '          Note ntracks = nchan * fanout * bits)' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, A, I3, A, F4.1, A, I2 )' )
     1            '                Tracks=', NTRACKS, 
     2            '  NCHAN=', NCHAN(KS), 
     3            '  FANOUT=', FANOUT(KS),
     4            '  BITS=', BITS(1,KS)
               CALL WLOG( 1, MSGTXT )
            END IF
C
C            Walter doesn't want this to be an error, just a warning.
C
C            ERRS = .TRUE.
            WARNED = .TRUE.
         END IF
      END IF   
C
      RETURN
      END
