      SUBROUTINE STTANT( VALUE, KC, KI )
C
C     Subroutine for SCHED called by SCHIN that processes TANT 
C     requests.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION   VALUE(*)
      INTEGER            KI(*), KEYPTR, ISTA, I, I1, I2
      LOGICAL            GOTTAS
      CHARACTER          KC(*)*(*), CHSTA*8, CHSTB*8
C -----------------------------------------------------------------
      GOTTAS = .FALSE.
C
C     Initialize.
C
      DO ISTA = 1, NSTA
         TANTS1(ISTA) = .FALSE.
         TANTS2(ISTA) = .FALSE.
      END DO
C
C     Decode the TANT requests.
C
      I1 = KEYPTR( 'TANTSTA1', KC, KI ) - 1
      I2 = KEYPTR( 'TANTSTA2', KC, KI ) - 1
      DO I = 1, MAXSTA
         CHSTA = ' '
         CHSTB = ' '
         WRITE( CHSTA, '(A8)' ) VALUE(I1+I) 
         WRITE( CHSTB, '(A8)' ) VALUE(I2+I) 
         CALL UPCASE( CHSTA )
         CALL UPCASE( CHSTB )
         DO ISTA = 1, NSTA
            IF( CHSTA .EQ. STANAME(ISTA) ) TANTS1(ISTA) = .TRUE.
            IF( CHSTB .EQ. STANAME(ISTA) ) TANTS2(ISTA) = .TRUE.
         END DO
      END DO
C
C     Warn that TANT requests will not be honored for files where 
C     I don't know how.
C
      DO ISTA = 1, NSTA
         IF( ( TANTS1(ISTA) .OR. TANTS2(ISTA) ) .AND. 
     1       ( CONTROL(STANUM(ISTA)) .NE. 'SNAP' .AND.
     2         CONTROL(STANUM(ISTA)) .NE. 'SN50' .AND.
     3         CONTROL(STANUM(ISTA)) .NE. 'VEX' .AND.
     4         CONTROL(STANUM(ISTA)) .NE. 'NRAO' ) ) THEN
            CALL WLOG( 1, 'SCHIN: TANT request for '//
     1          STATION(STANUM(ISTA))//' will be ignored.' )
         END IF
         IF( TANTS1(ISTA) .OR. TANTS2(ISTA) ) GOTTAS = .TRUE.
      END DO
C
C     If no tant stations were specified, set a few by default.
C     EB_VLBA can't make the request.  Green Bank no longer wants
C     this default.  OVRO is basically gone.
C
      IF( .NOT. GOTTAS ) THEN
         DO ISTA = 1, NSTA
            IF( STANAME(ISTA) .EQ. 'EFLSBERG' ) TANTS1(ISTA)=.TRUE.
C            IF( STANAME(ISTA) .EQ. 'EB_VLBA' ) TANTS1(ISTA)=.TRUE.
C            IF( STANAME(ISTA) .EQ. 'NRAO' ) TANTS1(ISTA)=.TRUE.
C            IF( STANAME(ISTA) .EQ. 'GB_VLBA' ) TANTS1(ISTA)=.TRUE.
C            IF( STANAME(ISTA) .EQ. 'OVRO' ) TANTS1(ISTA)=.TRUE.
         END DO
      END IF
C
      RETURN
      END
