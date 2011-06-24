      CHARACTER*32 FUNCTION VXNMDA( IXX )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     function generates a name for DA block IXX
C     for VLBA & 1 drive only density and poss tapelength
C
C     Some tape stuff removed July 22, 2010 RCW.  It is possible
C     this whole routine isn't needed any more, but I didn't chase
C     that.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER IXX, KS, ISTA, LEN1, LPOS, I, ISCAT
      CHARACTER NAME*32
C ----------------------------------------------------------------------
C     
C     Find a station that uses this DAS def
C
      ISTA = -1
      DO I = 1, NSTA
         IF( ISTADA(I) .EQ. IXX ) ISTA = I
      END DO
C
      IF( ISTA .LT. 0 ) CALL ERRLOG(' VXNMDA: no station for DAS def ')
C
C     and a set in which this station occurs
C
      ISCAT = STANUM(ISTA)
      DO I = 1, NSET
         IF ( USED(I) .AND. (
     1       STATION(ISCAT) .EQ. SETSTA(1,I) )) KS = I
      END DO
C
C     name depends on density (and number of drives and tape length)
C
      WRITE( NAME, '( I1 )' ) STNDRIV(ISCAT)
C
      LPOS = LEN1(NAME)+1
      WRITE( NAME(LPOS:), '( A )' ) DAR( ISCAT )
      LPOS = LEN1(NAME)+1
      IF( RECORDER(ISCAT) .NE. DAR(ISCAT) ) THEN
         WRITE( NAME(LPOS:), '( A1 )' ) '+'
         LPOS = LPOS+1
         WRITE( NAME(LPOS:), '( A )' ) RECORDER(ISCAT)
      END IF
      LPOS = LEN1(NAME)+1
C
C     Deleted check of NHEADS that only went if USETAPE.
C
C     density & length
C
      LPOS = LEN1(NAME)+1
      NAME(LPOS:LPOS) = '<'
      LPOS = LPOS+1
C
C     Deleted S2 stuff and USETAPE stuff.
C
      IF ( USEDISK(ISTA) .AND. DISK(STANUM(ISTA)) .EQ. 'MARK5A' )
     1     THEN
         NAME(LPOS:LPOS+5) = 'Mark5A'
      ELSE IF ( USEDISK(ISTA) .AND. DISK(STANUM(ISTA)) .EQ. 'LBADR' )
     1     THEN
         NAME(LPOS:LPOS+4) = 'LBADR'
      END IF
C
      VXNMDA = NAME
C
      RETURN
      END
