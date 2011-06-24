      CHARACTER*32 FUNCTION VXNMBB( IXX ) 
C
C     Routine specific for the VEX extension of SCHED. 
C     returns a name for an BBC block definition,
C     currently only writes simple consecutive numbering
C     By H.J. van Langevelde, JIVE, 140596 
C
C     Note by Cormac Reynolds (20090826): the logic in this routine does
C     not work for the LBA where the DC edge does not have to be the
C     same for both sidebands from a single BBC (e.g. you can get 2
C     USBs from a single BBC - they do not even have to be contiguous).
C     Digital systems! Anyway, it's just a name, so no dramas.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER ISET, IXX, NUNBBC, ICH
      CHARACTER NAME*32 
C ----------------------------------------------------------------------
C
      ISET = BBISSET( IXX )
      NAME = ' '
C
      NUNBBC = 0
      DO ICH=1,NCHAN(ISET)
         IF( MOD(ICH,2).EQ.0) THEN
            IF( BBSYN(ICH,ISET) .NE. BBSYN(ICH-1,ISET) .OR. 
     1          IFCHAN(ICH,ISET) .NE. IFCHAN(ICH-1,ISET) ) 
     2          NUNBBC = NUNBBC+1
         ELSE
            NUNBBC = NUNBBC + 1
         END IF
      END DO
      IF( NUNBBC .GT. 9 ) THEN
         WRITE( NAME(1:6), '( I2, A4 )' ) NUNBBC,'BBCs'
      ELSE
         WRITE( NAME(1:5), '( I1, A4 )' ) NUNBBC,'BBCs'
      ENDIF
C
C     Net sidebands does not occur in BBC comparison
C
      VXNMBB = NAME
C
      RETURN
      END
