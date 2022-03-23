      LOGICAL FUNCTION VXCFFQ( ISET, JSET )
C 
C     Routine specific for the VEX extension of SCHED. 
C     returns true if Freq block in 2 SCHED sets are identical
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISET, JSET, ICH ! ISIDE1, JSIDE1 (comment Sep 2013 RCW)
      LOGICAL IDENT
      CHARACTER UPCALI*4, UPCALJ*4
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.
C
C     first check n channels, different if different PCal:
C
      UPCALI = SPCAL(ISET)
      UPCALJ = SPCAL(JSET)
      CALL UPCASE(UPCALI)
      CALL UPCASE(UPCALJ)
      IF( UPCALI .NE. UPCALJ ) THEN
         IDENT = .FALSE.
C
C        or different number of channels
C
      ELSE IF( NCHAN(ISET) .NE. NCHAN(JSET) ) THEN
         IDENT = .FALSE.
      ELSE
C
C        check pols, first in ISET
C
         DO ICH = 1, NCHAN(ISET)
C
C           now compare pols, can only be RCP or LCP (chkset)
C
            IF( POL(ICH,ISET) .NE. POL(ICH,JSET) ) IDENT = .FALSE. 
C
C           Check the frequencies.
C           I removed the unnecessary calculation of ILOSUM and 
C           JLOSUM.  The values are already available as FREQREF.
C           RCW  Nov. 8, 2011
C
            IF( ABS(FREQREF(ICH,ISET)-FREQREF(ICH,JSET)) .GT. 1D-4 ) 
     1           IDENT = .FALSE.
C
C           check bandwidth
C
            IF( BBFILT(ICH,ISET) .NE. BBFILT(ICH,JSET) ) IDENT = .FALSE.
C
C           Check the net sideband.  We ran into a real schedule that 
C           shifted by one BBC bandwidth and flipped sideband causing
C           two different cases to appear to be the same.
C
            IF( NETSIDE(ICH,ISET) .NE. NETSIDE(ICH,JSET) ) 
     1           IDENT = .FALSE.
C
C           finally wiring should be identical
C
            IF( BBC(ICH, ISET) .NE. BBC(ICH, JSET)) IDENT = .FALSE.
C     
         END DO
C
C        and sample rate and number of bits
C
         IF( SAMPRATE(ISET) .NE. SAMPRATE(JSET) ) IDENT = .FALSE.
         IF( BITS(1,ISET) .NE. BITS(1,JSET) ) IDENT = .FALSE.
C
      END IF
C
      VXCFFQ = IDENT
C
      RETURN
      END
