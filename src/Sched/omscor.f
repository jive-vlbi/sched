      SUBROUTINE OMSCOR
C
C     Write correlation parameters into the OMS file.
C     OMS at Socorro correlator.
C
      INCLUDE       'sched.inc'
C
      INTEGER       IFFT, IAVG, MINFFT, LEN1
      CHARACTER     CPOL*3
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OMSCOR starting' )
C
      WRITE( IOMS, '( 1X, /, A, /, 1X )' ) 
     1       'BEGIN = CORRELATOR_PARAMETERS'
      CPOL = 'NO '
      IF( CORPOL ) CPOL = 'YES'
      WRITE( IOMS, '( A, A )' )  '    POLARIZATION      = ', CPOL
      WRITE( IOMS, '( A, F11.6 )' ) '    TIME_AVERAGE      = ', CORAVG
      IF( CAEXACT ) THEN
         WRITE( IOMS, '( A )' ) '    TWEAK_INT_TIME    = NO '
      ELSE
         WRITE( IOMS, '( A )' ) '    TWEAK_INT_TIME    = YES '
      END IF
C
C     Only do some parameters only for the Socorro hardware correlator
C
      IF( CORREL(1:6) .EQ. 'FXCORR' .AND. .NOT. NOTAPE ) THEN
C
C        Get the FFT size and spectral averaging.
C
         IF( CORPOL ) THEN
            MINFFT = 256
         ELSE
            MINFFT = 512
         END IF
         IF( 2 * CORCHAN .GE. MINFFT ) THEN
            IFFT = 2 * CORCHAN
            IAVG = 1
         ELSE
            IFFT = MINFFT
            IAVG = MINFFT / ( 2 * CORCHAN )
         END IF
C   
C        Write the desired information
C   
         WRITE( IOMS, '( A, I8 )' ) '    FFT_SIZE          = ', IFFT
         WRITE( IOMS, '( A, I4 )' ) '    SPECTRAL_AVERAGE  = ', IAVG
         WRITE( IOMS, '( A, A )' )  '    WINDOW            = ', CORWTFN
         IF( CORAV2 .NE. 0.0 .AND. CORAV2 .NE. CORAVG ) THEN
            WRITE( IOMS, '( A, F11.6 )' ) '    ALT_AVERAGE_TIME  = ', 
     1           CORAV2
            IF( CAEXACT2 ) THEN
               WRITE( IOMS, '( A )' ) '    TWEAK_ALT_INT_TIME = NO '
            ELSE
               WRITE( IOMS, '( A )' ) '    TWEAK_ALT_INT_TIME = YES '
            END IF

         END IF
C
      ELSE IF( ( CORREL(1:4) .EQ. 'VLBA' .OR. 
     1     CORREL(1:7) .EQ. 'SOCORRO' .OR.
     2     CORREL(1:8) .EQ. 'VLBADIFX' ) .AND. 
     3     .NOT. NOTAPE ) THEN
C
C        Get the FFT size and spectral averaging for VLBADIFX.  This 
C        correlator can, in principle, do any size FFT, but is more
C        efficient with 256 or so so it seems like a reasonable 
C        value for the minimum.  If someone later wants smaller ones,
C        we can provide a mechanism for specifying them.
C
         MINFFT = 256
         IF( 2 * CORCHAN .GE. MINFFT ) THEN
            IFFT = 2 * CORCHAN
            IAVG = 1
         ELSE
            IFFT = MINFFT
            IAVG = MINFFT / ( 2 * CORCHAN )
         END IF
C   
C        Write the desired information
C   
         WRITE( IOMS, '( A, I8 )' ) '    FFT_SIZE          = ', IFFT
         WRITE( IOMS, '( A, I4 )' ) '    SPECTRAL_AVERAGE  = ', IAVG
         WRITE( IOMS, '( A, A )' )  '    WINDOW            = ', CORWTFN
         IF( CORAV2 .NE. 0.0 .AND. CORAV2 .NE. CORAVG ) THEN
            WRITE( IOMS, '( A, F8.3 )' ) '    ALT_AVERAGE_TIME  = ', 
     1           CORAV2
         END IF
C
      END IF
C
      WRITE( IOMS, '( A, A )' ) '    OUTPUT_TAPE       = ', CORTAPE
      WRITE( IOMS, '( A, A )' ) '    OUTPUT_FORMAT     = ', CORDFMT
      IF( CORSHIP(1) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    SHIPPING          = ', CORSHIP(1)(1:LEN1(CORSHIP(1)))
      IF( CORSHIP(2) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    SHIPPING          = ', CORSHIP(2)(1:LEN1(CORSHIP(2)))
      IF( CORSHIP(3) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    SHIPPING          = ', CORSHIP(3)(1:LEN1(CORSHIP(3)))
      IF( CORSHIP(4) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    SHIPPING          = ', CORSHIP(4)(1:LEN1(CORSHIP(4)))
      IF( CORNOTE(1) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    NOTE              = ', CORNOTE(1)(1:LEN1(CORNOTE(1)))
      IF( CORNOTE(2) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    NOTE              = ', CORNOTE(2)(1:LEN1(CORNOTE(2)))
      IF( CORNOTE(3) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    NOTE              = ', CORNOTE(3)(1:LEN1(CORNOTE(3)))
      IF( CORNOTE(4) .NE. ' ' ) WRITE( IOMS, '( A, A )' )
     1     '    NOTE              = ', CORNOTE(4)(1:LEN1(CORNOTE(4)))
      WRITE( IOMS, '( A, F8.2 )' ) '    DATA_OUTPUT_RATE  = ', 
     1     MAXDR / 1000.0
      WRITE( IOMS, '( A, F8.1 )' ) '    DATA_SET_SIZE     = ',
     1     DATASIZE / 1.D6
C
      WRITE( IOMS, '( A, A )' ) '    SOURCE_POS_ORIGIN = ',
     1        CORSRCS(1:LEN1(CORSRCS))
      WRITE( IOMS, '( 1X, /, A, /, 1X )' ) 
     1        'END = CORRELATOR_PARAMETERS'
C
      RETURN
      END

