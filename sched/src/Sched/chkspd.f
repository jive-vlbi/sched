      SUBROUTINE CHKSPD( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks the 
C     bit rate per track and the tape speeds for VLBA, Mark III, and
C     Mark IV recorder systems.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     KS, BITRATE, LEN1, ISTA
      LOGICAL     OKSPEED, ERRS, CVLBA, GOTLOW, OK
C -------------------------------------------------------------------
C     Only make these checks for Mark III/IV or VLBA formats.
C
      IF( FORMAT(KS) .NE. 'MARKIII' .AND. 
     1    FORMAT(KS)(1:4) .NE. 'VLBA' .AND. 
     2    FORMAT(KS)(1:4) .NE. 'MKIV' ) 
     3      RETURN
C
C     Check the requested bit rate per track.  All of these systems
C     can handle 2, 4, and 8 Mbps per track.  The MarkIV formatters
C     can also do 16 Mbps per track.  A VLBA4 DAR is mostly a VLBA
C     DAR but includes a MarkIV formatter, so it can also do 16 Mbps
C     per track.
C
      BITRATE = TBPS(KS) + 0.001
      OK = BITRATE .EQ. 2 .OR. BITRATE .EQ. 4 .OR. BITRATE .EQ. 8
      OK = OK .OR. 
     1    ( BITRATE .EQ. 16 .AND. DAR(ISETSTA(KS)) .EQ. 'MKIV' ) .OR.
     4    ( BITRATE .EQ. 16 .AND. DAR(ISETSTA(KS)) .EQ. 'VLBA4' )
      IF( .NOT. OK ) THEN
         CALL WLOG( 1, 'CHKSPD: SAMPRATE and FORMAT(KS) do not ' //
     1      'combine to give valid bit rate on tape.' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, F10.3, 2A, A, I5, 4A )' ) 
     1         'CHKSPD: SAMPRATE=', SAMPRATE(KS), '   FORMAT=',
     2         FORMAT(KS), '   BITRATE=', BITRATE, '  DAR at ',
     3         STATION(ISETSTA(KS)), ' is ', DAR(ISETSTA(KS))
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C     Check the high density speed.  Valid speeds are 40, 80, and
C     160 ips for Mark IV and VLBA, plus 320 ips for Mark IV.
C     The bit rate was checked above, so I only need to check that
C     the speed scales correctly.
C
      OKSPEED = SPEEDH(KS) / BITRATE .EQ. 20.0
      IF( .NOT. OKSPEED ) THEN
         WRITE( MSGTXT, '( 2A, F8.2, A )' )
     1        'CHKSPD: This combination of high density ',
     2        'tape speed (', SPEEDH(KS), ') and ' 
         CALL WLOG( 1, MSGTXT )
         WRITE( MSGTXT, '( A, I3, A )' )
     3        '        track bit rate (', BITRATE, ' Mbps). '
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '        is not correct.')
         CALL WLOG( 1, 
     1       '        Speed (ips) / Bitrate(Mbps) should equal 20.' )
         ERRS = .TRUE.
      END IF
C
C     Check Low density speed, but only  if the station will be using 
C     low density.  VLBA systems will use odd speeds
C     for VLBA format at low density.  This was the result of a 
C     desire long ago to not have to do an on-line software update
C     when the system required an integer kilobits per inch!  The
C     price we pay for a bit of short term convenience.  It has caused
C     no end of confusion since.
C
C     In fact, this hardly matters any more because of the demise of
C     thick tapes (but they're not quite gone).
C
      ISTA = ISCHSTA(ISETSTA(KS))
      GOTLOW = DENSITY(ISTA) .EQ. 'L'
      IF( GOTLOW ) THEN
C
C        Check if this is a VLBA DAR.
C
         CVLBA = DAR(ISETSTA(KS))(1:4) .EQ. 'VLBA'
C
         IF( CVLBA .AND. FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
            OKSPEED = .FALSE.
            IF( BITRATE .EQ. 2 .AND. SPEEDL(KS) .EQ. 66.665 ) 
     1          OKSPEED = .TRUE.
            IF( BITRATE .EQ. 4 .AND. SPEEDL(KS) .EQ. 133.33 ) 
     1          OKSPEED = .TRUE.
            IF( BITRATE .EQ. 8 .AND. SPEEDL(KS) .EQ. 266.66 ) 
     1          OKSPEED = .TRUE.
            IF( .NOT. OKSPEED ) THEN
               WRITE( MSGTXT, '( 2A, F8.2, A )' )
     1              'CHKSPD: This combination of low density ',
     2              'tape speed (', SPEEDL(KS), ') and ' 
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, I3, A )' )
     1              '        track bit rate (', BITRATE, 
     2              ' Mbps) for VLBA format data'
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, 
     1           '        is not correct for VLBA controlled systems' )
               ERRS = .TRUE.
            END IF
C        
         ELSE
C        
C           Most systems use the round number speeds.
C        
            OKSPEED = .FALSE.
            IF( BITRATE .EQ. 2 .AND. SPEEDL(KS) .EQ. 67.5 ) 
     1          OKSPEED = .TRUE.
            IF( BITRATE .EQ. 4 .AND. SPEEDL(KS) .EQ. 135.0 ) 
     1          OKSPEED = .TRUE.
            IF( BITRATE .EQ. 8 .AND. SPEEDL(KS) .EQ. 270.0 ) 
     1          OKSPEED = .TRUE.
            IF( .NOT. OKSPEED ) THEN
               WRITE( MSGTXT, '( 2A, F8.2, A )' )
     1              'CHKSPD: This combination of low density ',
     2              'tape speed (', SPEEDL(KS), ') and ' 
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, I3, A )' )
     1              '        track bit rate (', BITRATE, 
     2              ' Mbps) for Mark III/IV format data'
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '        is not correct for ' //
     1              ' non-VLBA controlled systems.' )
               CALL WLOG( 1, '        Setup: '//
     1              SETNAME(KS)(1:LEN1(SETNAME(KS))) )
               CALL WLOG( 1, '        Station: '//SETSTA(1,KS) )
               ERRS = .TRUE.
            END IF
         END IF
      END IF
C
      RETURN
      END


