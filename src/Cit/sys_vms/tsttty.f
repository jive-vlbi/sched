      LOGICAL FUNCTION TSTTTY(LUNIT)
      INTEGER LUNIT
C
C Return TRUE if the specified Fortran unit is connected to a terminal.
C------------------------------------------------------------------------
      CHARACTER*255 FNAME
      LOGICAL ISTERM, TEST
C
      INQUIRE (UNIT=LUNIT, OPENED=TEST)
      IF (.NOT. TEST) OPEN (UNIT=LUNIT, STATUS='OLD', READONLY)
      INQUIRE (UNIT=LUNIT, NAME=FNAME)
      TSTTTY = ISTERM(FNAME)
      END
