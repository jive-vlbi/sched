      LOGICAL FUNCTION VXCFIF( ISET, JSET )
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 290596 
C     returns true if IF block in 2 SCHED sets are identical
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER ISET, JSET, I, J, ISTA
      INTEGER NICHAN, NJCHAN, ILIST(MCHAN), JLIST(MCHAN)
      LOGICAL NEWFND, IDENT
      CHARACTER UPCALI*4, UPCALJ*4, IDAR*5, JDAR*5
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     figure out how many unique IF's in ISET; look for codes in IFCHAN
C
      NICHAN = 0
      DO I = 1, NCHAN(ISET)
         NEWFND = .FALSE.
         IF( I .EQ. 1) THEN
            NEWFND = .TRUE.
         ELSE
            NEWFND = .TRUE.
            DO J = 1, NICHAN
               IF( IFCHAN(I,ISET) .EQ. IFCHAN(ILIST(J),ISET) )
     1             NEWFND = .FALSE.
            END DO
         END IF
         IF( NEWFND ) THEN
            NICHAN = NICHAN + 1
            ILIST( NICHAN ) = I
         END IF
      END DO
C
C     and in J for that matter
C
      NJCHAN = 0
      DO I = 1, NCHAN(JSET)
         NEWFND = .FALSE.
         IF( I .EQ. 1 ) THEN
            NEWFND = .TRUE.
         ELSE
            NEWFND = .TRUE.
            DO J = 1, NJCHAN
               IF( IFCHAN(I,JSET) .EQ. IFCHAN(JLIST(J),JSET) )
     1              NEWFND = .FALSE.
            END DO
         END IF
         IF( NEWFND ) THEN
            NJCHAN = NJCHAN + 1
            JLIST( NJCHAN ) = I
         END IF
      END DO
C
C     now compare, first the PCal
C
      UPCALI = SPCAL(ISET)
      UPCALJ = SPCAL(JSET)
      CALL UPCASE(UPCALI)
      CALL UPCASE(UPCALJ)
      IF( UPCALI .NE. UPCALJ ) THEN
         IDENT = .FALSE.
      ELSE IF( NICHAN .NE. NJCHAN ) THEN
         IDENT = .FALSE.
      ELSE 
         DO I = 1, NICHAN
C
C        this assumes they have to come out in same order, which is OK
C
            IF( ABS( FIRSTLO(ILIST(I),ISET) - 
     1          FIRSTLO(JLIST(I),JSET) ) .GT. 1E-4 ) 
     2          IDENT = .FALSE.
            IF( SIDE1(ILIST(I),ISET) .NE. SIDE1(JLIST(I),JSET) )
     1          IDENT = .FALSE.
            IF( IFCHAN(ILIST(I),ISET) .NE. IFCHAN(JLIST(I),JSET) ) 
     1          IDENT = .FALSE.
         END DO
      END IF
C
C     can also have different IF blocks if have different IF-coding
C     so need to find DAR for station involved
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) .OR. 
     1       STATION(STANUM(ISTA))(1:4) .EQ. 'VLBA' .AND.
     2       SETSTA(1,ISET)(1:4) .EQ. 'VLBA' .AND.
     3       .NOT. (VLBASSTA(ISTA,ISET) .OR. VLBASSET(ISET)) ) 
     4       IDAR = DAR(STANUM(ISTA))
      END DO
C 
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) .OR. 
     1       STATION(STANUM(ISTA))(1:4) .EQ. 'VLBA' .AND.
     2       SETSTA(1,JSET)(1:4) .EQ. 'VLBA' .AND.
     3       .NOT. (VLBASSTA(ISTA,JSET) .OR. VLBASSET(JSET)) ) 
     4       JDAR = DAR(STANUM(ISTA))
      END DO
C
C     the following statement covers MkIII/MkIV and VLBA/VLBAG
C
      IF( IDAR(1:3) .NE. JDAR(1:3) ) IDENT = .FALSE.
C
      VXCFIF = IDENT 
C
      RETURN
      END


