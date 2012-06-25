      SUBROUTINE GETCOR( VALUE, KC, KI )
C
C     This is a routine for SCHED that processes and checks the input 
C     correlator requests and creates the output for including in 
C     the output summary file.
C
      INCLUDE 'sched.inc'
C
      INTEGER            KI(*), KEYPTR, LEN1, SPLEN, ICH, MCH
      LOGICAL            MISCOR
      DOUBLE PRECISION   VALUE(*)
      CHARACTER          KC(*)*8, KCHAR*256, CHPOL*3, CAEX*8
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GETCOR starting.' )
      MISCOR = .FALSE.
C
C     Decode the name of the requested correlator.  Make upper case and
C     remove leading blanks for easier testing later.
C
      CORREL     = KCHAR( 'CORREL', 62, .FALSE., VALUE, KC, KI )
      CALL UPCASE( CORREL )
      MCH = LEN1( CORREL )
      ICH = 1
   30 CONTINUE
      IF( CORREL(ICH:ICH) .EQ. ' ' ) THEN
         ICH = ICH + 1
         IF( ICH .LT. MCH ) THEN
           GO TO 30
         END IF
      END IF
      MCH = LEN1( CORREL )
      CORREL = CORREL(ICH:MCH)
C
C     Check for known correlators.
C
      IF( .NOT. NOTAPE ) THEN
         IF( CORREL(1:7) .NE. 'SOCORRO' .AND. 
     1       CORREL(1:8) .NE. 'VLBADIFX' .AND.
     2       CORREL(1:4) .NE. 'VLBA' .AND. 
     3       CORREL(1:8) .NE. 'HAYSTACK' .AND. 
     4       CORREL(1:4) .NE. 'BONN' .AND. 
     5       CORREL(1:4) .NE. 'JIVE' .AND. 
     6       CORREL(1:10) .NE. 'WASHINGTON' .AND. 
     7       CORREL(1:4) .NE. 'USNO' .AND. 
     8       CORREL(1:3) .NE. 'JPL' .AND. 
     9       CORREL(1:7) .NE. 'BOLOGNA' .AND. 
     A       CORREL(1:6) .NE. 'MITAKA' .AND.
     A       CORREL(1:9) .NE. 'PENTICTON' .AND.
     B       CORREL(1:3) .NE. 'LBA' .AND.
     C       CORREL(1:6) .NE. 'FXCORR' .AND.
     D       CORREL(1:3) .NE. 'ASC' .AND.
     E       CORREL(1:5) .NE. 'OTHER' ) THEN
            CALL WLOG( 1, ' ** WARNING: ' // CORREL(1:LEN1(CORREL)) //
     1        ' is not a recognized correlator.' )
            CALL WLOG( 1, '     Recognized correlators are: '//
     1        'SOCORRO, VLBADIFX, VLBA, HAYSTACK, BONN, JIVE,' )
            CALL WLOG( 1, '       USNO, JPL, BOLOGNA, MITAKA, '//
     1             'PENTICTON, LBA, FXCORR, ASC, and OTHER' )
            MISCOR = .TRUE.
         END IF
      END IF
C
C     Decode other parameters.
C
C     Average requests, including an alternate request (was mainly for
C     space baselines that might want a shorter integration than the
C     ground baselines.  Also extract the "exact" request.
C
      CORAVG     = VALUE( KEYPTR( 'CORAVG',  KC, KI ) )
      CORAV2     = VALUE( KEYPTR( 'CORAVG2',  KC, KI ) )
      WRITE( CAEX, '(A8)' ) VALUE( KEYPTR( 'CORAVG',  KC, KI ) + 1 )
      CALL UPCASE( CAEX )
      CAEXACT = CAEX(1:5) .EQ. 'EXACT'
      WRITE( CAEX, '(A8)' ) VALUE( KEYPTR( 'CORAVG2',  KC, KI ) + 1 )
      CALL UPCASE( CAEX )
      CAEXACT2 = CAEX(1:5) .EQ. 'EXACT'
C
C
      CORCHAN    = VALUE( KEYPTR( 'CORCHAN', KC, KI ) )
      CORFFT     = VALUE( KEYPTR( 'CORCHAN', KC, KI ) + 1 )
      IF( CORFFT .EQ. 0 ) CORFFT = MAX( 128, CORCHAN )
      CORNANT    = VALUE( KEYPTR( 'CORNANT', KC, KI ) )
      CHPOL      = KCHAR( 'CORPOL', 3, .TRUE., VALUE, KC, KI )
      CALL UPCASE( CHPOL )
      CORPOL     = CHPOL(1:2) .EQ. 'ON '
      CORWTFN    =  KCHAR( 'CORWTFN', 16, .TRUE., VALUE, KC, KI )
      CALL UPCASE( CORWTFN )
      CORTAPE    = KCHAR( 'CORTAPE', 16, .TRUE., VALUE, KC, KI )
      CALL UPCASE( CORTAPE )
      CORDFMT    = KCHAR( 'CORDFMT', 8, .TRUE., VALUE, KC, KI )
      CALL UPCASE( CORDFMT )
      CORSRCS    = KCHAR( 'CORSRCS',  64, .TRUE., VALUE, KC, KI )
      CORSHIP(1) = KCHAR( 'CORSHIP1', 64, .FALSE., VALUE, KC, KI )
      CORSHIP(2) = KCHAR( 'CORSHIP2', 64, .FALSE., VALUE, KC, KI )
      CORSHIP(3) = KCHAR( 'CORSHIP3', 64, .FALSE., VALUE, KC, KI )
      CORSHIP(4) = KCHAR( 'CORSHIP4', 64, .FALSE., VALUE, KC, KI )
      CORNOTE(1) = KCHAR( 'CORNOTE1', 128, .FALSE., VALUE, KC, KI )
      CORNOTE(2) = KCHAR( 'CORNOTE2', 128, .FALSE., VALUE, KC, KI )
      CORNOTE(3) = KCHAR( 'CORNOTE3', 128, .FALSE., VALUE, KC, KI )
      CORNOTE(4) = KCHAR( 'CORNOTE4', 128, .FALSE., VALUE, KC, KI )
C
C     Check the parameters.  Especially make sure the required ones
C     are present.  None of this matters if not recording (NOTAPE).
C
      IF( .NOT. NOTAPE ) THEN
C
C        Check the imputs common to all correlators.  These are 
C        were in the SOCDEF and CORDEF, but got consolidated for
C        ease of understanding.
C
         CALL CHKCOR
C
C        Check the ones that are different for Socorro and elsewhere.
C
         IF( CORREL(1:7) .EQ. 'SOCORRO' .OR. 
     1       CORREL(1:4) .EQ. 'VLBA' .OR.
     2       CORREL(1:7) .EQ. 'VLBADIFX' .OR.
     3       CORREL(1:6) .EQ. 'FXCORR' ) THEN
            CALL SOCDEF( CHPOL )
         ELSE
            CALL CORDEF( CHPOL )
         END IF
C
      END IF
C
C     Make up printout array.
C
      CORSTUFF(1) = 
     1   'CORRELATION REQUESTS (Defaults in parentheses): '
C
      IF( CORREL(1:7) .EQ. 'SOCORRO' .OR.
     1    CORREL(1:4) .EQ. 'VLBA' .OR.
     3    CORREL(1:8) .EQ. 'VLBADIFX' ) THEN
         CORSTUFF(2) = 
     1      '  Correlator (Required if recording):          ' // 
     2      CORREL(1:LEN1(CORREL)) // 
     3      '    (VLBA DiFX software correlator)'
      ELSE IF( MISCOR ) THEN
         CORSTUFF(2) = 
     1      '  Correlator (Required if recording):          ' // 
     2      CORREL(1:LEN1(CORREL)) // '    (Unknown correlator)'
      ELSE
         CORSTUFF(2) = 
     1      '  Correlator (Required if recording):          ' // 
     2      CORREL(1:LEN1(CORREL))
      END IF
C
      WRITE( CORSTUFF(3), '( A, F8.3, A )' ) 
     1   '  Correlator average time (2 sec):            ', 
     2   CORAVG, ' sec.'
      WRITE( CORSTUFF(4), '( A, F8.3, A )' ) 
     1   '    Alternate average time (for spacecraft).  ',
     2   CORAV2, ' sec.'
      WRITE( CORSTUFF(5), '( A, I8 )' ) 
     1  '  Output spectral channels per baseband (16):    ', CORCHAN
      WRITE( CORSTUFF(6), '( A, I8 )' ) 
     1  '     Correlator FFT size (128):                  ', CORFFT
      WRITE( CORSTUFF(7), '( A, I8 )' ) 
     1  '  Number of antennas to be correlated:           ', CORNANT
      CORSTUFF(8) = 
     1  '  Polarization (ON):                           ' // CHPOL
      CORSTUFF(9) = 
     1  '  Correlator weighting function (UNIFORM):     ' // CORWTFN
      CORSTUFF(10) = 
     1  '  Distribution tape (DAT):                     ' // CORTAPE
      CORSTUFF(11) = 
     1  '  Distribution format (FITS):                  ' // CORDFMT
C
      SPLEN = LEN1( CORSRCS )      
      IF( SPLEN .LE. 13 ) THEN
         CORSTUFF(12) = 
     1      '  Source positions from:             '// CORSRCS(1:SPLEN)
         CORSTUFF(13) = '   '
      ELSE
         CORSTUFF(12) = '  Source positions from:'
         CORSTUFF(13) = '  '//CORSRCS
      END IF
      CORSTUFF(14) = '  Shipping address for correlator output: ' 
      CORSTUFF(15) = '      ' // CORSHIP(1)
      CORSTUFF(16) = '      ' // CORSHIP(2)
      CORSTUFF(17) = '      ' // CORSHIP(3)
      CORSTUFF(18) = '      ' // CORSHIP(4)
      CORSTUFF(19) = '  Correlator Notes: ' // CORNOTE(1)
      CORSTUFF(20) = '      ' // CORNOTE(2)
      CORSTUFF(21) = '      ' // CORNOTE(3)
      CORSTUFF(22) = '      ' // CORNOTE(4)
      CORSTUFF(23) = ' '
C
      RETURN
      END

