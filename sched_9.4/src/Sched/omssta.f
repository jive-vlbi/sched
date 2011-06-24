      SUBROUTINE OMSSTA
C
C     Routine for SCHED that writes station information for OMS
C     at the VLBA correlator.
C
      INCLUDE       'sched.inc'
      INCLUDE       'schset.inc'
C
      INTEGER            ISTA, KSTA, ISCN 
      CHARACTER          TFORM*8, TIME1*8, TIME2*8
      DOUBLE PRECISION   START1, STOPL, START, STOP
      INTEGER            DAY1, DAY2, YEAR1, YEAR2
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OMSSTA starting' )
C
      DO ISTA = 1, NSTA
C
C        Get the first and last times.  Also use this to determine
C        if anything should be done for this station or if it
C        really wasn't used.
C
         START1 = 1.0D10
         STOPL = 0.D0
         DO ISCN = SCAN1, SCANL
            IF( STASCN(ISCN,ISTA) .AND. .NOT. NOREC(ISCN) ) THEN
               START1 = MIN( START1, STARTJ(ISCN) )
               STOPL  = MAX( STOPL,  STOPJ(ISCN) )
            END IF
         END DO
         IF( START1 .NE. 1.0D10 ) THEN
C
C           Now process the station.  Start by getting the index
C           in the station catalog.
C
            KSTA = STANUM(ISTA)
C
            WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1          'BEGIN = STATION_INFO'
C
            WRITE( IOMS, '( A, A )' )  '    STATION_ID         = ',
     1          STCODEU(KSTA)
            WRITE( IOMS, '( A, A )' )  '    STATION_NAME       = ',
     1          STATION(KSTA)
C
            WRITE( IOMS, '( A )' ) '    TAPES              =   0'
            WRITE( IOMS, '( A )' ) '    PASSES             =    0'
            WRITE( IOMS, '( A, F7.1 )' ) '    DISK               = ',
     1         TGBYTES(ISTA)
C
C           Get the first and last times.
C
            START1 = 1.0D10
            STOPL = 0.D0
            DO ISCN = SCAN1, SCANL
               IF( STASCN(ISCN,ISTA) .AND. .NOT. NOREC(ISCN) ) THEN
                  START1 = MIN( START1, STARTJ(ISCN) )
                  STOPL  = MAX( STOPL,  STOPJ(ISCN) )
               END IF
            END DO
            CALL TIMEJ( START1, YEAR1, DAY1, START )
            CALL TIMEJ( STOPL, YEAR2, DAY2, STOP )
            TIME1 = TFORM( START, 'T', 0, 2, 2, '::@' )
            TIME2 = TFORM( STOP, 'T', 0, 2, 2, '::@' )
            WRITE( IOMS, '( A, I4, A, I3.3, A, A )' ) 
     1             '    START              = ', YEAR1, '-', DAY1, 
     2             '/', TIME1
            WRITE( IOMS, '( A, I4, A, I3.3, A, A )' ) 
     1             '    STOP               = ', YEAR2, '-', DAY2, 
     2             '/', TIME2
C
            WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1          'END = STATION_INFO'
C
         END IF
C
      END DO
C
      RETURN
      END
