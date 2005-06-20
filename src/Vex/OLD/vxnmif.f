      CHARACTER*32 FUNCTION VXNMIF( IIF, XTRAIF ) 
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 140596 
C     Returns a name for an IF block definition
C     Updated 230796, Huib
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL XTRAIF
      INTEGER ISET, IIF, LPOS, LEN1, ICH, PHMODE
      CHARACTER NAME*32, NMPOL*1, UPCAL*4
      LOGICAL DUALFRQ
C ----------------------------------------------------------------------
C
      ISET = IFISSET( IIF )
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
      IF( DUALFRQ ) THEN
         NAME(1:8) = 'DualFreq'
      ELSE
         NAME(1:3) = 'LO@'
C
         IF( FIRSTLO(1,ISET) .GT. 9.999E4 ) THEN
            WRITE( NAME(4:12), '( I6, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. 9.999E3 ) THEN
            WRITE( NAME(4:11), '( I5, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. 9.99E2 ) THEN
            WRITE( NAME(4:10), '( I4, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. 99.9 ) THEN
            WRITE( NAME(4:9), '( I3, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. 9.9 ) THEN
            WRITE( NAME(4:10), '( F4.1, A3 )' ) FIRSTLO(1,ISET), 'MHz'
C        zero LO and negative values can occur at 90 and 50 cm
         ELSE IF( FIRSTLO(1,ISET) .GE. 0.0 ) THEN
            WRITE( NAME(4:9), '( F3.1, A3 )' ) FIRSTLO(1,ISET), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. -10.0 ) THEN
            WRITE( NAME(4:10), '( F4.1, A3 )' ) FIRSTLO(1,ISET), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. -100.0 ) THEN
            WRITE( NAME(4:10), '( I3, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE IF( FIRSTLO(1,ISET) .GT. -1000.0 ) THEN
            WRITE( NAME(4:11), '( I4, A3 )' ) 
     1           NINT( FIRSTLO(1,ISET) ), 'MHz'
         ELSE
            WRITE( MSGTXT, '( A, G13.6, A )' )
     1          'VXNMIF: LO value (', FIRSTLO(1, ISET),
     2          ') does not fit name.'
            CALL WLOG( 1, MSGTXT )
            WRITE( NAME(4:11), '( A )' ) 'aBitIFfy'
         END IF
C     
      END IF
      NMPOL = '_'
      DO ICH = 1, NCHAN(ISET)
C
C        find out if all same pol or dual, can be RCP or LCP (after checkset)
C
         IF( NMPOL .EQ. '_' ) THEN
            NMPOL = POL(ICH,ISET)(1:1)
         ELSE 
            IF( NMPOL .NE. POL(ICH,ISET)(1:1) ) NMPOL = 'D'
         ENDIF
C
      END DO
C
      LPOS = LEN1( NAME ) + 1
C
      WRITE( NAME(LPOS:LPOS+4), '( A1, A3 )' )
     1     NMPOL,'Pol'
C
C     about p cal
C
      IF( XTRAIF ) THEN
         PHMODE = NINT(TONEINT(IIF))
      ELSE
         PHMODE = -1
         UPCAL = SPCAL(ISET)
         CALL UPCASE(UPCAL)
         IF( UPCAL .EQ. 'OFF' ) PHMODE = 0
         IF( UPCAL .EQ. '1MHZ' ) PHMODE = 1
         IF( UPCAL .EQ. '5MHZ' ) PHMODE = 5
         IF( UPCAL .EQ. ' ' ) PHMODE = 1      
      END IF
C
      LPOS = LEN1( NAME ) + 1
C
      IF( PHMODE .EQ. 0) NAME(LPOS:LPOS+5) = 'NoTone'
      IF( PHMODE .EQ. 1) NAME(LPOS:LPOS+5) = 'Tone/1'
      IF( PHMODE .EQ. 5) NAME(LPOS:LPOS+5) = 'Tone/5'     
C
      VXNMIF = NAME
C
      RETURN
      END
