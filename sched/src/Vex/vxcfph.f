      LOGICAL FUNCTION VXCFPH( ISET, JSET )
C
C     returns true if PH block in 2 SCHED sets are identical
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISET, JSET, NITON, NJTON, ICH, I, J
      INTEGER ISIDE1, DUMMY(MAXCHN)
      INTEGER NTONI(MAXCHN), NTONJ(MAXCHN)
      INTEGER ITONI(MAXTON,MAXCHN), ITONJ(MAXTON,MAXCHN)
      LOGICAL IDENT 
      REAL TONIINT, TONJINT
      DOUBLE PRECISION LOISUM(MCHAN), LOJSUM(MCHAN)
      CHARACTER UPCAL*4
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Could be set, off or blank
C
      UPCAL = SPCAL(ISET)
      CALL UPCASE(UPCAL)
      IF ( UPCAL .EQ. 'OFF' ) TONIINT = 0.
      IF ( UPCAL .EQ. '1MHZ' ) TONIINT = 1.
      IF ( UPCAL .EQ. '5MHZ' ) TONIINT = 5.
      IF ( UPCAL .EQ. ' ' ) TONIINT = 1.
C      
      UPCAL = SPCAL(JSET)
      CALL UPCASE(UPCAL)
      IF ( UPCAL .EQ. 'OFF' ) TONJINT = 0.
      IF ( UPCAL .EQ. '1MHZ' ) TONJINT = 1.
      IF ( UPCAL .EQ. '5MHZ' ) TONJINT = 5.
      IF ( UPCAL .EQ. ' ' ) TONJINT = 1.
C
      IF( ABS(TONIINT-TONJINT) .GT. 1e-3 ) THEN
         IDENT = .FALSE.
      ELSE
C
C     but can also depend on all tones to be detected, first for ISET
C     start calculating Skyfeq and determine tones
C
         DO ICH = 1, NCHAN(ISET)
            IF( SIDE1(ICH,ISET) .EQ. 'U' ) THEN
               ISIDE1 = 1
            ELSE IF( SIDE1(ICH,ISET) .EQ. 'L' ) THEN
               ISIDE1 = -1
            ELSE
               CALL ERRLOG('VXCFPH: First LO sideband problem.' )
            END IF
            LOISUM(ICH) = FIRSTLO(ICH,ISET) + 
     1           DFLOAT(ISIDE1) * DBLE(BBSYN(ICH,ISET))
         END DO
C     
C        Now calculate find list of tones
C
         CALL VXTON2(-1, ISET, NCHAN(ISET), LOISUM,
     1       NETSIDE(1,ISET), BBFILT(1,ISET), 
     2       TONIINT, NITON, DUMMY,
     3       NTONI, ITONI )
C         
C        and the same for JSET:
C
         DO ICH = 1, NCHAN(JSET)
            IF( SIDE1(ICH,JSET) .EQ. 'U' ) THEN
               ISIDE1 = 1
            ELSE IF( SIDE1(ICH,JSET) .EQ. 'L' ) THEN
               ISIDE1 = -1
            ELSE
               CALL ERRLOG('VXCFPH: First LO sideband problem.' )
            END IF
            LOJSUM(ICH) =  FIRSTLO(ICH,JSET) + 
     1           DFLOAT(ISIDE1) * DBLE(BBSYN(ICH,JSET))
         END DO
C     
C        Now calculate value of tones to detect in JSET
C
         CALL VXTON2(-1, JSET, NCHAN(JSET), LOJSUM,
     1       NETSIDE(1,JSET), BBFILT(1,JSET), 
     2       TONIINT, NJTON, DUMMY,
     3       NTONJ, ITONJ )
C
C        now compare and find out if they are identical
C     
         IF( NITON .NE. NJTON ) THEN
            IDENT = .FALSE.
         ELSE
            DO I = 1, NITON
C
C              can't make this test very rigid, need to change to Double
C
               IF( NTONI(I) .NE. NTONJ(I) ) THEN
                  IDENT = .FALSE.
               ELSE
                  DO J = 1, NTONI(I)
                     IF( ITONI(J,I) .NE. ITONJ(J,I) )
     1                   IDENT = .FALSE.
                  END DO
               END IF
            END DO
         END IF
      END IF
C     
C     Because the phasecal has to know about which IF and has to be set 
C     to a new NPHVEX when there is a new FQ.
C
      IF( NCHAN(ISET) .NE. NCHAN(JSET) ) IDENT = .FALSE.
      DO ICH = 1, NCHAN(JSET)
         IF( ABS(LOISUM(ICH) - LOJSUM(ICH)) .GT. 1D-2 ) IDENT = .FALSE.
      END DO
C
      VXCFPH = IDENT 
C
      RETURN
      END
