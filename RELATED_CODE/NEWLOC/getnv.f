      INTEGER FUNCTION GETNV( INNAME, VNAME, NV, MV )
C
C     Function for NEWPOS to get the array index for a line from the 
C     VLA positions file.  I assume that there will be many repeats of the
C     same pad so we might have an existing entry to write over, or need
C     to add one.
C
      CHARACTER     INNAME*8, VNAME(*)*(*)
      CHARACTER     COMNAM*8
      INTEGER       MV, NV, IN
C
C ---------------------------------------------------------------------
C     Add 'VLA_' to the name and  look for a name match.
C
      COMNAM = 'VLA_'//INNAME
      GETNV = 0
      DO IN = 1, NV
         IF( COMNAM .EQ. VNAME(IN) ) THEN
            GETNV = IN
            RETURN
         END IF
      END DO
C
C     Name not recognized.
C
      IF( GETNV .EQ. 0 ) THEN
         WRITE(*,*) 'GETNV:  Unrecognized VLA pad: ', INNAME
         STOP
      END IF
C
      RETURN
      END


