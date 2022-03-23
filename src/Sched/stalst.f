      SUBROUTINE STALST( MJD )
C
C     Subroutine for SCHED that writes a table of station information 
C     to summary file.  Include the tape initialization stuff.
C
C     June 8, 2018 RCW  Allow for STCODE longer than 2 characters in 
C     top row of the baseline length table.  Also clean up some implicit
C     type conversions that generated warnings.
C
      INCLUDE  'sched.inc'
C
      INTEGER    ISTA, ISCAT, IC1, IC2, BLENKM, LEN1, JSTA, JSCAT
      INTEGER    NNSTA, MJD, NMINP, NPREST, ISCN, IM, IP, ISCL, ISCL1
      REAL       BLENSQ, YEARS
      LOGICAL    ANYDISK, ANYELSE, AXWARN, NEW, MOREM, MOREP
      DOUBLE PRECISION  MINPPRT(6), PRESTPRT(6)
C ----------------------------------------------------------------------
C
      WRITE( ISUM, '( 1X, /, 1X, /, 1X, /, A, /, 1X, /, A,A, /, '//
     1         ' 87X, A /, 1X )' )
     1      'STATIONS USED IN SCHEDULE:',
     2      '   Station  Code   Latitude Longitude  Elevation ',
     3      '      X            Y            Z       Axis',
     4      'Offset'
      AXWARN = .FALSE.
      DO ISTA = 1, NSTA
          ISCAT = STANUM(ISTA)
          WRITE( ISUM, '( 3X, A8, 2X, A3, F10.5, F11.5, F9.0, 2X, ' //
     1           ' 3F13.3, F7.3 )' ) 
     2        STATION(ISCAT), STCODE(ISCAT),
     3        LAT(ISCAT)/RADDEG,
     4        LONG(ISCAT)/RADDEG,
     5        ELEV(ISCAT),
     6        XPOS(ISCAT), YPOS(ISCAT), ZPOS(ISCAT), AXOFF(ISCAT)
         IF( AXOFF(ISCAT) .EQ. 0.0 .AND. 
     1       STATION(ISCAT)(1:3) .NE. 'VLA' ) AXWARN = .TRUE.
      END DO
C
C     While we're poking at geometry data, issue a warning if
C     the axis offset is zero for one or more stations.  Don't do
C     this for the VLA (or add other phased arrays eventually).
C
      IF( AXWARN ) THEN
         CALL WLOG( 1, 'STALST:  WARNING - A station has zero '//
     1      'axis offset, which is unlikely. ' )
         CALL WLOG( 1, '         Accurate positions, including '//
     1      'axis offsets are needed for correlation.' )
         CALL WLOG( 1, '         Is your station location '//
     1      'information adequate?' )
         CALL WLOG( 1, '         See the summary file station '// 
     1      'list to see which stations are suspect.' )
      END IF
C
C     Write the rates and adjusted positions.
C
      WRITE( ISUM, '(  1X, /, 1X, /, A, I6 )' )
     1      '   Plate tectonic motion adjustments for MJD ', MJD
      WRITE( ISUM, '( A,A, /, 15X, A, A )' )
     1      '   Station  Code      Station motions (m/yr)  ',
     2      '           Adjusted positions ',
     3      '      X       Y       Z  ', 
     4      '   MJD0        X            Y            Z '
C
C     Not using the real value of 365.24... here because 365.25 is 
C     the number used when deriving the rates in SOLVE according 
C     to John Gipson of GSFC in email on Dec. 11, 2008.  That 
C     number is based on the defined value of 36525 for the 
C     Julian century that is the time unit for many astronomical 
C     calculations.
C
      DO ISTA = 1, NSTA
          ISCAT = STANUM(ISTA)
          YEARS = REAL( ( MJD - MJDRATE(ISCAT) ) / 365.25D0 ) 
          WRITE( ISUM, '( 3X, A8, 2X, A3, 3F8.4, I7, 1X, 3F13.3 )' )
     1        STATION(ISCAT), STCODE(ISCAT),
     2        DXPOS(ISCAT), DYPOS(ISCAT), DZPOS(ISCAT), MJDRATE(ISCAT),
     3        XPOS(ISCAT) + DXPOS(ISCAT) * YEARS, 
     4        YPOS(ISCAT) + DYPOS(ISCAT) * YEARS, 
     5        ZPOS(ISCAT) + DZPOS(ISCAT) * YEARS
      END DO
C
C     Make a matrix of baseline lengths, if there are any
C     baselines (for VLA schedules, there often won't be).
C     Only do first 30 stations across if there are more.
C
      NNSTA = NSTA
      IF( NNSTA .GT. 30 ) NNSTA = 30
      IF( NNSTA .GT. 1 ) THEN
         WRITE( ISUM, '( 1X, /, 1X, /, 1X, /, A, /, 1X )' )
     1         'BASELINE LENGTHS (km)'
         MSGTXT = ' '
C
C        Write the station codes across the top.
C
         ISCL1 = LEN1(STCODE(1))
         DO ISTA = 1, NNSTA
            ISCAT = STANUM(ISTA)
            ISCL = LEN1(STCODE(ISCAT))
            IC1 = 13 - ISCL1 + ( ISTA - 1 ) * 6
            IC2 = IC1 + ISCL - 1
            MSGTXT(IC1:IC2) = STCODE(ISCAT)
         END DO
C
C        Now write a line for each station.
C
         WRITE( ISUM, '( A, /, 1X )' ) MSGTXT(1:LEN1(MSGTXT))
         DO JSTA = 1, NSTA
            JSCAT = STANUM(JSTA)
            MSGTXT = '   ' // STCODE(JSCAT)
            DO ISTA = 1, NNSTA
               ISCAT = STANUM(ISTA)
               BLENSQ = REAL( 
     1                  ( ( XPOS(JSCAT) - XPOS(ISCAT) ) / 1000.0 )**2 +
     2                  ( ( YPOS(JSCAT) - YPOS(ISCAT) ) / 1000.0 )**2 +
     3                  ( ( ZPOS(JSCAT) - ZPOS(ISCAT) ) / 1000.0 )**2 )
               BLENKM = NINT( SQRT( BLENSQ ) )
               IC1 = 8 + ( ISTA - 1 ) * 6
               IC2 = IC1 + 5
               WRITE( MSGTXT(IC1:IC2), '( I5 )' ) BLENKM
            END DO
            WRITE( ISUM, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         END DO
      END IF
C
C     Recording systems.
C
      IF( .NOT. ( VLAONLY .OR. NOTAPE ) ) THEN
C
C        First find out what we have.
C
         ANYDISK = .FALSE.
         ANYELSE = .FALSE.
         DO ISTA = 1, NSTA      
            ANYDISK = ANYDISK .OR. USEDISK(ISTA)
            ANYELSE = ANYELSE .OR. .NOT. USEDISK(ISTA)
         END DO
         WRITE( ISUM, '( 1X, /, 1X, /, A )' )
     1      'RECORDING SYSTEM AND CALIBRATION INFORMATION:'
C
C        Tell the parameters related to recording:
C        These are scan dependent so collect the ones seen.
C        If there are more than 6, just write 'more' for the last.
C
         NMINP = 1
         NPREST = 1
         MOREM = .FALSE.
         MOREP = .FALSE.
         MINPPRT(1) = MINPAUSE(SCAN1)
         PRESTPRT(1) = PRESTART(SCAN1)
         DO ISCN = SCAN1 + 1, SCANL
            NEW = .TRUE.
            DO IM = 1, NMINP
               IF( MINPAUSE(ISCN) .EQ. MINPPRT(IM) ) NEW = .FALSE.
            END DO
            IF( NEW ) THEN
               IF( NMINP .GE. 5 ) THEN
                  MOREM = .TRUE.
               ELSE
                  NMINP = NMINP + 1
                  MINPPRT(NMINP) = MINPAUSE(ISCN)
               END IF
            END IF               
C
            NEW = .TRUE.
            DO IP = 1, NPREST
               IF( PRESTART(ISCN) .EQ. PRESTPRT(IP) ) NEW = .FALSE.
            END DO
            IF( NEW ) THEN
               IF( NPREST .GE. 5 ) THEN
                  MOREP = .TRUE.
               ELSE
                  NPREST = NPREST + 1
                  PRESTPRT(NPREST) = PRESTART(ISCN)
               END IF
            END IF
         END DO
         WRITE( ISUM, '( 1X, /, A, A )' )
     1      '  List of scan-dependent controls seen for recording ',
     2      'timing.'
C
         MSGTXT = '   MINPAUSE:'
         IC1 = LEN1( MSGTXT ) + 1
         DO IM = 1, NMINP
            WRITE( MSGTXT(IC1:IC1+5), '( F6.0)' ) MINPPRT(IM) / ONESEC
            IC1 = IC1 + 6
         END DO        
         IF( MOREM ) WRITE( MSGTXT(IC1:IC1+4), '( A )' ) ' more'
         WRITE( ISUM, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
         MSGTXT = '   PRESTART:'
         IC1 = LEN1( MSGTXT ) + 1
         DO IM = 1, NPREST
            WRITE( MSGTXT(IC1:IC1+5), '( F6.0)' ) PRESTPRT(IM) / ONESEC
            IC1 = IC1 + 6
         END DO        
         IF( MOREP ) WRITE( MSGTXT(IC1:IC1+4), '( A )' ) ' more'
         WRITE( ISUM, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
C        Deal with disk stations.
C
         IF( ANYDISK ) THEN
            WRITE( ISUM, '( 1X, /, A, /, A )' )
     1          '  DISKS - Stations potentially recording on disks.',
     2          '   Station    Drive type   DAR     NBBC    Tsys'
            DO ISTA = 1, NSTA
               IF( USEDISK(ISTA) ) THEN
                  ISCAT = STANUM(ISTA)
                  WRITE( ISUM, '( 3X, A8, 5X, A6, 5X, A5, I6,5X,A4 )' )
     1               STATION(ISCAT), DISK(ISCAT), DAR(ISCAT), 
     2               NBBC(ISCAT), TSCAL(ISCAT)
               END IF
            END DO              
         END IF
C
C        Others.
C
         IF( ANYELSE ) THEN
            WRITE( ISUM, '( 1X, /, A, /, A, /, 1X )' )
     1          '  OTHER - Stations with other recording systems.',
     2          '   Station    Drive type '
            DO ISTA = 1, NSTA
               IF( .NOT. ( USEDISK(ISTA) ) ) THEN
                  ISCAT = STANUM(ISTA)
                  WRITE( ISUM, '( 3X, A8, 5X, A6 )' )
     1               STATION(ISCAT),  RECORDER(ISCAT)
               END IF
            END DO              
         END IF
C
      END IF
C
      RETURN
      END



