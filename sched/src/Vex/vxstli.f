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
      INTEGER   ISTA, LPOS, LEN1, IMODE, ISET, VXGTST
      CHARACTER LINE*132
C ----------------------------------------------------------------------
C
      DO IMODE = 1, NMDVEX
C
C        Find the sched setup corresponding to this mode so we can check
C        the format
C
         ISET = VXGTST( IMODE )
         IF( FORMAT(ISET)(1:4) .NE. 'NONE' .OR. 
     1       OBSTYP .EQ. 'PTVLBA' ) THEN
           IF ( NSTAXX(IXX,IMODE) .GT. 0 ) THEN
              WRITE( LINE, '( A1, A, I2, 4X, A )' ) 
     1            COM, ' mode = ', IMODE, 'stations = '
              LPOS = LEN1(LINE)
              DO ISTA = 1, NSTAXX(IXX,IMODE)
                 WRITE( LINE(LPOS+1:LPOS+3), '( A )' )
     1               STCODE(STANUM(ISTAXX(ISTA,IXX,IMODE)))
C
C          reduce the spaces
C
                 LPOS = LEN1(LINE)
                 IF( ISTA .NE. NSTAXX(IXX,IMODE) ) THEN 
                    WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                    LPOS = LPOS+1
                 END IF
              END DO
C
              WRITE(IVEX, '( A )' ) LINE(1:LEN1(LINE))
           END IF
         END IF
      END DO
C
      RETURN
      END
   
