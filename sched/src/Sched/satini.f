      SUBROUTINE SATINI( INSCH )
C
C     Routine to collect satellite information.
C     Mainly it gets associated sets of satellite names and
C     ephemeris files.  This routine should only be called once.
C
      INCLUDE 'sched.inc'
C
C     For KEYIN.
C
      INTEGER           MSKEY, MODE, INSCH, KEYPTR, I
      PARAMETER         (MSKEY= 300)
      INTEGER           KI(MSKEY)
      CHARACTER         KC(MSKEY)*8, KCHAR*80
      DOUBLE PRECISION  KD(2*MSKEY), ENDMARK
      LOGICAL           GOTKEYS
      SAVE              KI, KD, KC, ENDMARK, GOTKEYS
C
      DATA          (KI(I),I=1,3)   / MSKEY, 0, 3 /
      DATA          GOTKEYS         / .FALSE. /
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SATINI: starting.' )
C
      CALL KPACK( '/       ', ENDMARK )
C
      IF( .NOT. GOTKEYS ) THEN
C
C        Set up to read the input information.  Only do once (can get
C        into this routine more than once if RESTART is pressed while
C        in plot mode.
C
         CALL KEYCHR( 'SATNAME', ' ', 12, KD, KC, KI )
         CALL KEYADD( 'SATNUM', UNSET, 1, KD, KC, KI )
         CALL KEYCHR( 'SATFILE', 'NONE', 128, KD, KC, KI )
         CALL KEYCHR( 'KERFILE', 'NONE', 128, KD, KC, KI )
         CALL KEYADD( 'ENDSAT', UNSET, 1, KD, KC, KI )
      END IF
      KD( KEYPTR( 'ENDSAT', KC, KI ) ) = UNSET     
C
C     Loop through the input groups.  NSAT was initialized in SCHIN.
C
  100 CONTINUE
         MODE = 0
         CALL KEYIN( KD(MSKEY+1), KD, KI(2), ENDMARK, MODE, INSCH, 6 )
         IF( MODE .EQ. 1 .OR. 
     1       KD( KEYPTR( 'ENDSAT', KC, KI ) ) .EQ. 0.D0 ) THEN
            GO TO 200
         ELSE
            NSAT = NSAT + 1
            SATNAME(NSAT) = KCHAR( 'SATNAME', 12, .TRUE., KD, KC, KI )
            SATNUM(NSAT)  = KD( KEYPTR( 'SATNUM', KC, KI ) )
            SATFILE(NSAT) = KCHAR( 'SATFILE', 80, .FALSE., KD, KC, KI )
            CALL ENVIR( SATFILE(NSAT) )
            KERFILE(NSAT) = KCHAR( 'KERFILE', 80, .FALSE., KD, KC, KI )
            CALL ENVIR( KERFILE(NSAT) )
         END IF
         GO TO 100
C
  200 CONTINUE
      RETURN
      END
