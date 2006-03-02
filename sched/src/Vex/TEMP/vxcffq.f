      LOGICAL FUNCTION VXCFFQ( ISET, JSET )
C 
C     Routine specific for the VEX extension of SCHED. 
C     returns true if Freq block in 2 SCHED sets are identical
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISET, JSET, ISIDE1, JSIDE1, ICH
      LOGICAL IDENT
      CHARACTER UPCALI*4, UPCALJ*4
      DOUBLE PRECISION ILOSUM, JLOSUM
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
C           build up sky freqs for I...
C
            IF( SIDE1(ICH,ISET) .EQ. 'U' ) THEN
               ISIDE1 = 1
            ELSE IF( SIDE1(ICH,ISET) .EQ. 'L' ) THEN
               ISIDE1 = -1
            ELSE
               CALL ERRLOG( 'VXCFFQ: First LO sideband problem.' )
            END IF
C
            ILOSUM = FIRSTLO(ICH,ISET) + ISIDE1 * BBSYN(ICH,ISET)
C
C           build up sky freqs for J...
C
            IF( SIDE1(ICH,JSET) .EQ. 'U' ) THEN
               JSIDE1 = 1
            ELSE IF( SIDE1(ICH,JSET) .EQ. 'L' ) THEN
               JSIDE1 = -1
            ELSE
               CALL ERRLOG( 'VXCFFQ: First LO sideband problem.' )
            END IF
C
            JLOSUM = FIRSTLO(ICH,JSET) + JSIDE1 * BBSYN(ICH,JSET)
C
            IF( ABS(ILOSUM-JLOSUM) .GT. 1e-4 ) IDENT = .FALSE.
C
C           check bandwidth
C
            IF( BBFILT(ICH,ISET) .NE. BBFILT(ICH,JSET) ) IDENT = .FALSE.
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
