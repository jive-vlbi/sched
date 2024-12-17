      CHARACTER*9 FUNCTION VLBASTOP( STOPJ, ADDSEC, LASTDY, TWOPI,
     1    IUVBA ) 
C
C     Function mainly for VLBAEND that gets the TSTOP character 
C     string and updates the day if necessary.
C
C     Be careful where this is called because it will write the
C     day to the output file if it is needed.
C
      DOUBLE PRECISION  STOPJ, FRACD, ADDSEC, TWOPI
      INTEGER         LASTDY, DOY, IUVBA
      CHARACTER       TFORM*15, VLBAD*9, HUMAND*16
C -----------------------------------------------------------------
C     Get the time in fraction of a day and in human readable forms.
C
      CALL SCHDAY( STOPJ + ADDSEC / 86400.D0, VLBAD, 
     1             HUMAND, DOY, FRACD )
C
C     Get the stop time character string.
C
      VLBASTOP  = TFORM( FRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
C
C     Update the day number if needed.  Write to VLBA file.
C
      IF( DOY .NE. LASTDY ) THEN
         WRITE( IUVBA, '(''date='', A9 )' ) VLBAD
         LASTDY = DOY
      END IF
C
C     Done.
C
      RETURN
      END
