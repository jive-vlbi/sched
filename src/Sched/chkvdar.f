      SUBROUTINE CHKVDAR( KS, SNBBC, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the VLBA DAR's.  It will only be called if the DAR is
C     of the VLBA type.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH, I, SNBBC
      LOGICAL           ERRS, BADLO
      DOUBLE PRECISION  TSTFRQ
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVDAR: Starting.')
C
C     IFDIST must be 0, 20, A, 20A, or A20.  
C
      DO I = 1, 4
         IF( IFDIST(I,KS) .NE. '0' .AND. IFDIST(I,KS) .NE. '20' .AND.
     1       IFDIST(I,KS) .NE. 'A' .AND. IFDIST(I,KS) .NE. '20A' 
     2       .AND. IFDIST(I,KS) .NE. 'A20' ) THEN
            MSGTXT = 'CHKVDAR: Invalid IFDIST '//IFDIST(I,KS)// 
     1             ' specified '
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check items that depend on observation type.
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN 
         IF( SAMPRATE(KS) .NE. 0.25 .AND. SAMPRATE(KS) .NE. 0.5 .AND. 
     1       SAMPRATE(KS) .NE. 1.0  .AND. SAMPRATE(KS) .NE. 2.0 .AND. 
     2       SAMPRATE(KS) .NE. 4.0  .AND. SAMPRATE(KS) .NE. 8.0 .AND.  
     3       SAMPRATE(KS) .NE. 16.  .AND. SAMPRATE(KS) .NE. 32. ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.3 )' )
     1           'CHKVDAR: Invalid SAMPRATE specified: ', SAMPRATE(KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
         IF( BITS(1,KS) .NE. 1 .AND. BITS(1,KS) .NE. 2 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5 )' ) 'CHKVDAR: BITS must be 1 or 2'
     1           // ' - Requested: ', BITS(1,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END IF
C
C     Check channel parameters.
C
      DO ICH = 1, NCHAN(KS)
C
C        IF channel assignment.
C
         IF( IFCHAN(ICH,KS) .NE. 'A' .AND. 
     1       IFCHAN(ICH,KS) .NE. 'B' .AND.
     2       IFCHAN(ICH,KS) .NE. 'C' .AND. 
     3       IFCHAN(ICH,KS) .NE. 'D' ) THEN
            CALL WLOG( 1, 'CHKVDAR: IFCHAN ''' // IFCHAN(ICH,KS) //
     1          ''' not A,B,C, or D' )
            ERRS = .TRUE.
         END IF
C
C        BBC assignment.
C
         IF( BBC(ICH,KS) .LT. 1 ) THEN
            CALL WLOG( 1, 'CHDVDAR: BBC number less than 1! ')
            ERRS = .TRUE.
         END IF
         IF( BBC(ICH,KS) .GT. SNBBC ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, I3, 2A )' )
     1         'CHKVDAR: Channel', ICH, ' uses a BBC (', BBC(ICH,KS),
     2         ') that does not exist at some station.'
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        BBC synthesizer setting.  Getting a good double precision
C        is a bit tricky.  Round at a level below what we are testing.
C
         TSTFRQ = DNINT ( DBLE( BBSYN(ICH,KS) ) * 1.D4 ) / 1.D4
         IF( BADLO( 'BBSYN', TSTFRQ, 0.01D0, 0, 0.D0, 0.D0,
     1       500.D0, 1000.D0, MSGTXT ) ) THEN
            ERRS = .TRUE.
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F9.2, 2A )' )
     1         '        FIRSTLO and first sideband are:', 
     2         FIRSTLO(ICH,KS), '  ', SIDE1(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        BBC filter bandwidth.
C
         IF( BBFILT(ICH,KS).NE.0.0625 .AND. BBFILT(ICH,KS).NE.0.125
     1     .AND. BBFILT(ICH,KS).NE.0.250 .AND. BBFILT(ICH,KS).NE.0.5 
     2     .AND. BBFILT(ICH,KS).NE.1.0   .AND. BBFILT(ICH,KS).NE.2.0 
     3     .AND. BBFILT(ICH,KS).NE.4.0   .AND. BBFILT(ICH,KS).NE.8.0   
     4     .AND. BBFILT(ICH,KS).NE.16. ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2 )' )
     1         'CHDVDAR: Invalid BBFILTER specified: ',
     2         BBFILT(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Check the frequency request if there is frequency switching.
C
         IF( FRSWITCH(KS) ) THEN
            TSTFRQ = DNINT ( DBLE( BBSYN2(ICH,KS) ) * 1.D4 ) / 1.D4
            IF( BADLO( 'BBSYN2', TSTFRQ, 0.01D0, 0, 0.D0, 0.D0,
     1          500.D0, 1000.D0, MSGTXT ) ) THEN
               ERRS = .TRUE.
            END IF
         END IF
C
      END DO
C
      RETURN
      END
