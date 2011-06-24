      SUBROUTINE FLAGWRT( UNIT, STCODEF, STARTF, STOPF, REASON )
C
C     Routine for SCHED called by FLAGS that actually writes the line
C     in the flagfile.  This saves a lot of repetition in FLAGS
C     when writing multiple types of flags.
C
      INTEGER            UNIT
      CHARACTER          STCODEF*(*), REASON*(*)
      DOUBLE PRECISION   STARTF, STOPF, START, STOP
C
      INTEGER            YEAR, DAY1, DAY2, LEN1
      CHARACTER          TIMER*26, TFORM*12
      CHARACTER          CSTART*8, CSTOP*8
C ----------------------------------------------------------------------
C     Get the day and time of ends of the flag.
C
      CALL TIMEJ( STARTF, YEAR, DAY1, START )
      CALL TIMEJ( STOPF, YEAR, DAY2, STOP )
C     
C     Write the time range specification.
C     
      CSTART = TFORM( START, 'T', 0, 2, 2, ',,@' )
      CSTOP  = TFORM( STOP, 'T', 0, 2, 2, ',,@' )
      WRITE( TIMER, '( I3, A, A, A, I4, A, A )' )
     1   DAY1, ',', CSTART,',', DAY2, ',',CSTOP
C     
C     Write the flag line to the flag file.
C
      WRITE( UNIT, '( 7A )' )
     1   'ant_name=''', STCODEF(1:LEN1(STCODEF)),
     2   ''' timerang=', TIMER, 
     3   '  reason=''', REASON(1:LEN1(REASON)), ''' /'
C
      RETURN
      END

