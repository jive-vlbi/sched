      SUBROUTINE GLSTDAY( ISCN, YEAR, DAY, TIME, RDAY )
C
C     Routine for SCHED's LST scheduling capability that takes the 
C     provided year, day, and time, when that time is an LST time, 
C     and finds the local sidereal day (RDAY) at the sidereal 
C     reference station.  The input DAY can be the day of year, if
C     a UT date was given, or a LST day number, in which case this 
C     routine just passes the number to the right variable.
C
      INCLUDE    'sched.inc'
C
      INTEGER            YEAR, DAY, RDAY, SIDDAY, ISCN, ERR
      DOUBLE PRECISION   TIME, RJDAY, LSTMID, LSTDAY
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GLSTDAY starting' )
C
      IF( DAY .LE. 366 ) THEN
C
C        Deal with a UT day.
C        Get the MJD and then the LST at 0 hr UT of DAY.
C
         CALL SLA_CLDJ( YEAR, 1, DAY, RJDAY, ERR )
         CALL SIDTIM( RJDAY, LONG(LSTSTA), TWOPI, SIDDAY, LSTMID,
     1                   LSTDAY )
C
C        Now make RDAY the modified Julian LST day number.
C        If the start LST was before the 0hr LST, assume that
C        the start was on the day before.  Also deal with the
C        short period when the UT day specification is ambiguous.
C
         RDAY = SIDDAY
         IF( TIME .GT. LSTMID / TWOPI .AND.
     1       TIME .LE. LSTMID / TWOPI + ( SIDR - 1.D0 ) ) THEN
            CALL WLOG( 1, 'GLSTDAY:  This is your lucky day!' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A, I8 )' )
     1                 '          You have specified an LST ',
     2                 'time and a UT date for scan ', ISCN
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '          There are about 3.9 minutes '
     1               //  'each day when this is not unique.' )
            CALL WLOG( 1, '          Your time is in that range.' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, I8, A, I8 )' )
     1                    '          Your Local Sidereal Day could ',
     2                 'be either ', SIDDAY, ' or ', SIDDAY + 1
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '          Set input parameter ''DAY'' to'
     1               //  ' the desired one and rerun SCHED.' )
            CALL ERRLOG( 'Need unique LST start day.' )
         ELSE IF( TIME * TWOPI .LT. LSTMID ) THEN
            RDAY = RDAY + 1
         END IF
         CALL WLOG( 1, 'GLSTDAY: CAUTION:  You have specified a '
     1       // 'UT start day for an LST schedule.' )
         CALL WLOG( 1, '    Unless this is for dynamic scheduling,'
     1          // ' please check scan dates carefully.' )
C
      ELSE
C
C        Start day was an LST day number.  Just send it back out.
C
         RDAY = DAY
      END IF
C
      RETURN
      END
