      SUBROUTINE VXSTLI( IXX, NSTAXX, ISTAXX )
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a comment line to indicate which stations used by def
C     By H.J. van Langevelde, JIVE, 051296 
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C
      INTEGER   IXX,NSTAXX(MAXMOD,MAXMOD), ISTAXX(MAXSTA,MAXMOD,MAXMOD)
C
      INTEGER   ISTA, LPOS, LEN1, IMODE
C From commented code: ISET, VXGTST
      CHARACTER LINE*132
C ----------------------------------------------------------------------
C
      DO IMODE = 1, NMDVEX
C
C        Find the sched setup corresponding to this mode so we can check
C        the format
C        RCW Dec 2011.  Don't filter the FORMAT=NONE cases.
C        RCW Apr 2012.  These are comment lines so the total length
C        should not exceed 128 characters.
C
C         ISET = VXGTST( IMODE )
C         IF( FORMAT(ISET)(1:4) .NE. 'NONE' .OR. 
C     1       OBSTYP .EQ. 'PTVLBA' ) THEN
           IF ( NSTAXX(IXX,IMODE) .GT. 0 ) THEN
              WRITE( LINE, '( A1, A, I2, 4X, A )' ) 
     1            COM, ' mode = ', IMODE, 'stations = '
              DO ISTA = 1, NSTAXX(IXX,IMODE)
C
C                Break the line if this station is going to
C                push it over the 128 character comment limit.
C
                 LPOS = LEN1( LINE )
                 IF( LPOS + 1 + 
     1              LEN1(STCODE(STANUM(ISTAXX(ISTA,IXX,IMODE))))
     2              .GE. 128 ) THEN
                    WRITE(IVEX, '( A )' ) LINE(1:LPOS)
                    LINE = ' '
                    WRITE( LINE, '( A1 )' ) COM
                    LPOS = 25
                 END IF
C
C                Write the station code.
C
                 WRITE( LINE(LPOS+1:LPOS+3), '( A )' )
     1               STCODE(STANUM(ISTAXX(ISTA,IXX,IMODE)))
C
C                Reduce the spaces and add the colon.
C
                 LPOS = LEN1( LINE )
                 IF( ISTA .NE. NSTAXX(IXX,IMODE) ) THEN 
                    WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                    LPOS = LPOS+1
                 END IF
              END DO
C
C             Write out the line (or last line if it was broken).
C
              WRITE(IVEX, '( A )' ) LINE(1:LEN1(LINE))
C
           END IF
C         END IF
      END DO
C
      RETURN
      END
   
