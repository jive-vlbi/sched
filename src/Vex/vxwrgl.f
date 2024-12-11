      SUBROUTINE VXWRGL
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the GL = $GLOBAL section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER   LEN1
      CHARACTER TMPEXP*32
C ----------------------------------------------------------------------
C
C     Write $GLOBAL section is trivial but probably mandatory
C
      TMPEXP = EXPCODE
      CALL VXSTNM(TMPEXP,.FALSE.)
      WRITE( IVEX, '( A, A1, /, 5X, A, A, A1 )' ) '$GLOBAL', SEP,
     1     'ref $EXPER = ', TMPEXP(1:LEN1(TMPEXP)), SEP
C
C     write the revision code in comment
C
      WRITE( IVEX, '( A1, 54X, A14 )' ) COM, '+------------+'
      WRITE( IVEX, '( A1, 25X, A, 10X, A1, F10.4, 2X, A1 )' ) COM, 
     1    'PI revision number:', '|', SCHVER, '|'
      WRITE( IVEX, '( A1, 54X, A14 )' ) COM, '+------------+' 
C
C     start with mode in comment
C
      IF( OBSMODE .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'mode:       ', 
     2     OBSMODE(1:LEN1(OBSMODE))      
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
