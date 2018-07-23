      CHARACTER*32 FUNCTION VXNMFQ( IXX, XTRAFQ ) 
C
C     returns a name for an FREQ block definition
C     By H.J. van Langevelde, JIVE, 300496 
C     IXX, is number of LINK, necessary to find SET
C     XTRAFQ = .TRUE. used for extra frequencies detected
C     in main schedule, then we can use VXFQ links etc
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL XTRAFQ, DUALFRQ
      INTEGER IXX, ISET, ISIDE1, LPOS, LEN1, ICH
      INTEGER NPER, NFC1, NFCN
      CHARACTER NAME*32, FRTXT*16
      DOUBLE PRECISION SUMILO 
      REAL BBVAL
C ----------------------------------------------------------------------
C
      ISET = FQISSET( IXX )
C
C     First let's see if this is a dual Freq mode
C
      DUALFRQ = .FALSE.
      DO ICH = 2, NCHAN(ISET)
         IF( ABS(FIRSTLO(1,ISET) - FIRSTLO(ICH,ISET)) .GT. 1E3 )
     1        DUALFRQ = .TRUE.
      END DO
C
C     check pols and sidebands
C
      NAME = ' '
C
C     find first frequency. Mixed BW not allowed currently
C
      IF( XTRAFQ ) THEN
         SUMILO = VXLOSUM(1,IXX)
      ELSE
         IF( SIDE1(1,ISET) .EQ. 'U' ) THEN
            ISIDE1 = 1
         ELSE IF( SIDE1(1,ISET) .EQ. 'L' ) THEN
            ISIDE1 = -1
         ELSE
            CALL ERRLOG( 'VXNMFQ: First LO sideband problem.' )
         END IF
C
         SUMILO = FIRSTLO(1,ISET) + ISIDE1 * BBSYN(1,ISET)
      END IF
C
C     write a name with the freq in it.
C     Let the number of digits used depend on the tuning.
C
      IF( DUALFRQ ) THEN
         NAME(1:8) = 'DualFreq'
      ELSE
         NPER=7
         CALL FRCHAR( SUMILO, NPER, NFC1, NFCN, FRTXT )
         WRITE( NAME, '(A)' ) FRTXT(NFC1:NFCN)
         LPOS = LEN1( NAME ) + 1
         WRITE( NAME(LPOS:LPOS+2), '(A)' ) 'MHz'
      END IF
C
      LPOS = LEN1( NAME ) + 1
C
C     add the number of channels
C
      IF( NCHAN(ISET) .GT. 9 ) THEN
         WRITE( NAME(LPOS:LPOS+2), '( I2, A1 )' ) NCHAN(ISET),'x'
      ELSE
         WRITE( NAME(LPOS:LPOS+1), '( I1, A1 )' ) NCHAN(ISET),'x'
      ENDIF
C
      LPOS = LEN1( NAME ) + 1
C
C     and filter width
C
      BBVAL = BBFILT(1,ISET)
      IF( XTRAFQ) BBVAL = VXBBFILT(1,IXX)
C
C     Add 3 digit MHz for DDC and also change cutoff points to
C     x.50 from x.99 to better match the use of NINT for the wider
C     band cases.  RCW May 7, 2012.
C     Add 4 digit MHz for ALMA.  AJM Feb 1, 2016
C
      IF( BBVAL .GT. 999.50 ) THEN
         WRITE( NAME(LPOS:LPOS+6), '( I4, A3 )' )
     1        NINT( BBVAL ),'MHz'
      ELSE IF( BBVAL .GT. 99.50 ) THEN
         WRITE( NAME(LPOS:LPOS+5), '( I3, A3 )' ) 
     1        NINT( BBVAL ),'MHz'
      ELSE IF( BBVAL .GT. 9.50 ) THEN
         WRITE( NAME(LPOS:LPOS+4), '( I2, A3 )' ) 
     1        NINT( BBVAL ),'MHz'
      ELSE IF( BBVAL .GT. 0.99 ) THEN
         WRITE( NAME(LPOS:LPOS+3), '( I1, A3 )' ) 
     1        NINT( BBVAL ),'MHz'
      ELSE IF( BBVAL .GT. 0.099 ) THEN
         WRITE( NAME(LPOS:LPOS+5), '( I3, A3 )' ) 
     1        NINT( BBVAL*1e3 ),'kHz'
      ELSE
         WRITE( NAME(LPOS:LPOS+6), '( F4.1, A3 )' ) 
     1        BBVAL*1e3,'kHz'
      END IF
C
      VXNMFQ = NAME
C
      RETURN
      END

