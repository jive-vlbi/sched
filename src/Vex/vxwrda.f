      SUBROUTINE VXWRDA
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the DA = $DAS section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Fix 1.5 version for DAS = none 070397
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IXX, ISTA, I, ISCAT, KS, IDRIV, JHEAD
      INTEGER   LEN1
      CHARACTER VCRSPD*3, HEAD*10
      INTEGER   DURATIO
      LOGICAL   VIOLFS
C ----------------------------------------------------------------------
C     write $DAS section 
C     
      VIOLFS = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$DAS', SEP     
C
C     first, for recording system
C
      DO IXX = 1, NDAVEX
C
C        find a station to which this refers
C
         ISTA = -1
         DO I = 1, NSTA
            IF( ISTADA(I) .EQ. IXX ) ISTA = I
         END DO
         ISCAT = STANUM(ISTA)
C
         IF( ISTA .LT. 0 ) 
     1       CALL ERRLOG(' VXWRDA: no station for $DAS def ')
C
C        Prevent use of MkIII recorder and DAS with VEX in current version
C        proper implementation reuires work on chk3dar and a lot on
C        chk3dar, if PCFS will ever support this...
C
         ISCAT = STANUM(ISTA)
         IF( RECORDER(ISCAT) .EQ. 'MKIII' .OR.
     1       DAR(ISCAT) .EQ. 'MKIII' )
     2       CALL ERRLOG('VXWRDA: MkIII stations not supported '//
     3       'through VEX yet...')
C
C        start with regular stuff
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' )  'def ', 
     1       DALINK(IXX)(1:LEN1(DALINK(IXX))),SEP
C
C        Start by finding a set in which this station occurs,
C        because some things need to know eg. FORMAT
C
         DO I = 1, NSET
            IF ( USED(I) .AND. 
     1          ( STATION(ISCAT) .EQ. SETSTA(1,I) .OR.
     2          (STATION(ISCAT)(1:4) .EQ. 'VLBA' .AND.
     3          SETSTA(1,I)(1:4) .EQ. 'VLBA' ))) KS = I
         END DO
C
C        Now write recorder - tape takes precedence over disk
C
         IF( USETAPE(ISTA) ) THEN
             IF( RECORDER(ISCAT) .EQ. 'VLBA' .OR. 
     1           RECORDER(ISCAT) .EQ. 'S2' .OR. 
     1           RECORDER(ISCAT) .EQ. 'VLBA4' .OR. 
     2           RECORDER(ISCAT) .EQ. 'K4' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1              'record_transport_type = ',
     2              RECORDER(ISCAT)(1:LEN1(RECORDER(ISCAT))), SEP
             ELSE IF( RECORDER(ISCAT) .EQ. 'MKIII' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1              'record_transport_type = ',
     2              'Mark3A', SEP
             ELSE IF( RECORDER(ISCAT) .EQ. 'MKIV' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1              'record_transport_type = ',
     2              'Mark4', SEP
             ELSE IF( RECORDER(ISCAT) .EQ. 'NONE' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1              'record_transport_type = ',
     2              'none', SEP
             ELSE
                CALL ERRLOG(' VXWRDA: Unknown recorder of type: '//
     1              RECORDER(ISCAT) )
             END IF
         ELSE IF( USEDISK(ISTA) ) THEN
             IF( DISK(STANUM(ISTA)) .EQ. 'MARK5A' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' )
     1              'record_transport_type = ',
     2              'Mark5A', SEP
             END IF
         END IF
C
C        Next electronic rack
C
         IF( DAR(ISCAT) .EQ. 'VLBA' .OR. 
     1       DAR(ISCAT) .EQ. 'S2' .OR. 
     1       DAR(ISCAT) .EQ. 'VLBAG' .OR.
     1       DAR(ISCAT) .EQ. 'VLBA4' .OR. 
     2       DAR(ISCAT) .EQ. 'K4' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          DAR(ISCAT)(1:LEN1(DAR(ISCAT))), SEP
         ELSE IF( DAR(ISCAT) .EQ. 'MKIII' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'Mark3A', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'MKIV' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'Mark4', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'none', SEP
         ELSE
            CALL ERRLOG(' VXWRDA: Unknown DAR of type: '//
     1          DAR(ISCAT) )
         END IF
C
C        Some things obsolete for S2, take out of loop
C
         IF( RECORDER(ISCAT) .NE. 'S2' ) THEN
C
C           Write number of drives.
C
            WRITE( IVEX, '( 5X, A, I1, A1 )' ) 'number_drives = ',
     1          STNDRIV(STANUM(ISTA)), SEP
C
C           Check if this fits the PCFS
C
            IF (NHEADS(STANUM(ISTA)) .GT. 2 ) THEN
               VIOLFS = .TRUE.
               WRITE( MSGTXT, '( A, A, A, I3, A )' ) 
     1             'VXWRDA: WARNING: More than 2 heads on ',
     2             STATION(STANUM(ISTA)), ' recorder (', 
     3             NHEADS(STANUM(ISTA)), ')'
               CALL WLOG( 1, MSGTXT )
            END IF            
C     
C           And the headstacks for each
C
            IF( USETAPE(ISTA) ) THEN
C              WRITE (HEAD, '( A )' ) 'read/write'
               HEAD = 'read/write'
            ELSE IF( USEDISK(ISTA) ) THEN
C              WRITE (HEAD, '( A )' ) ''
               HEAD = ''
            END IF
            IF( STNDRIV(STANUM(ISTA)) .LT. 1 .OR. 
     1          STNDRIV(STANUM(ISTA)) .GT. 9 .OR.
     2          NHEADS(STANUM(ISTA)) .LT. 1 .OR.
     3          NHEADS(STANUM(ISTA)) .GT. 9 ) THEN
               CALL ERRLOG('VXWRDA inconsitent number of'//
     1             ' drives/headstacks')
            ELSE
               DO IDRIV = 1, STNDRIV(STANUM(ISTA)) 
                  DO JHEAD = 1, NHEADS(STANUM(ISTA))
                     WRITE( IVEX, '( 5X, A, I1, 1X, A1, 1X, A, 
     1                   1X, A1, 1X, I1, 1X, A1 )' ) 
     2                   'headstack = ', 
     3                   JHEAD + NHEADS(STANUM(ISTA))*(IDRIV-1), COL,
     4                   HEAD, COL, IDRIV-1, SEP
                  END DO
               END DO
            END IF
C
C           next write density, formally a fuction of FORMAT
C           not an issue for S2 either (or disks)
C
            IF( USETAPE(ISTA) ) THEN
               IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
                  IF( DENSITY(ISTA) .EQ. 'H' ) THEN
                     WRITE( IVEX, '( 5X, A, I5, 1X, A, A1 )' ) 
     1                   'record_density = ',
     2                   56700, 'bpi', SEP
                  ELSE IF( DENSITY(ISTA) .EQ. 'L' ) THEN
                     WRITE( IVEX, '( 5X, A, I5, 1X, A, A1 )' ) 
     1                   'record_density = ',
     2                   34020, 'bpi', SEP
                  ELSE
                     CALL ERRLOG(' VXWRDA: Unknown density ')
                  END IF
C
               ELSE IF( FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
                  IF( DENSITY(ISTA) .EQ. 'H' ) THEN
                     WRITE( IVEX, '( 5X, A, I5, 1X, A, A1 )' ) 
     1                   'record_density = ',
     2                   56250, 'bpi', SEP
                  ELSE IF( DENSITY(ISTA) .EQ. 'L' ) THEN
                     WRITE( IVEX, '( 5X, A, I5, 1X, A, A1 )' ) 
     1                   'record_density = ',
     2                   33333, 'bpi', SEP
                  ELSE
                     CALL ERRLOG(' VXWRDA: Unknown density ')
                  END IF
C
               ELSE IF( FORMAT(KS)(1:5) .EQ. 'MKIII' ) THEN
                  IF( DENSITY(ISTA) .EQ. 'L' ) THEN
                     WRITE( IVEX, '( 5X, A, I5, 1X, A, A1 )' ) 
     1                   'record_density = ',
     2                   33333, 'bpi', SEP
                  ELSE
                     CALL ERRLOG(' VXWRDA: Unsupported density MKIII ')
                  END IF
               END IF
            END IF
C     
C           tapelength is simple
C
            IF( USETAPE(ISTA) ) THEN
               WRITE( IVEX, '( 5X, A, I6, 1X, A, A1 )' ) 
     1             'tape_length =', TPLENG(ISTA), 'ft', SEP
            END IF
C
C           tapemotion can be start/stop or adaptive but fixed params!
C           Use this for disks as well, as 0 second gaps seem to cause a
C           problem - allow switching it off by using VEXTEST in case
C           this gets fixed
C
            IF( USETAPE(ISTA) .OR. .NOT. VEXTEST ) THEN
               WRITE( IVEX, '( 5X, A, A, A1, I2, A, A1, I2, A, A1, 
     1             I3, A, A1 )' ) 'tape_motion = ', 
     2             'adaptive ', COL, 0, ' min', COL,
     3             0, ' min', COL, 10, ' sec', SEP
            END IF
         ELSE
C
C           S2 in minutes, and adaptive schedules
C
            IF( DENSITY(ISTA) .EQ. 'L' ) THEN
               VCRSPD = 'lp'
               DURATIO = INT((TPLENG(ISTA)*12.)/(60.*SPEEDL(KS)))
            ELSE IF( DENSITY(ISTA) .EQ. 'H' ) THEN
               VCRSPD = 'slp'
               DURATIO = INT((TPLENG(ISTA)*12.)/(60.*SPEEDH(KS)))
            ELSE
               CALL ERRLOG(' VXWRDA: Unknown density ')
            END IF
C
C           Duration in minutes
C
            WRITE( IVEX, '( A1, 4X, A )' ) COM, 
     1          '               duration    : spd :ntp'
            WRITE( IVEX, '( 5X, A, I4, 1X, A, A1, 1X, A3, 1X, A1,
     1          I2, A1 )' ) 
     2          'tape_length =',
     3          DURATIO, 'min', COL, VCRSPD, COL,
     4          STNDRIV(STANUM(ISTA)), SEP
C
C           only adaptive scheduling is allowed
C
            WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1          '              type     : early start : late stop'//
     2          '   : minimum gap'
            WRITE( IVEX, '( 5X, A, A, 1X, A1, F4.1, 1X, A,
     1          1X, A1, F4.1, 1X, A, 1X, A1, F4.1, 1X, A, A1 )' ) 
     2          'tape_motion = ', 'adaptive', COL,
     3          2.5, 'min', COL,
     3          0.1, 'min', COL,
     3          3.0, 'min', SEP
C
         END IF
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      IF( VIOLFS )
     1    CALL WLOG( 1,'VXWRDA: More than 2 headstacks '//
     2    'not supported in PCFS; this VEX will NOT run!!!!')
      RETURN
      END
