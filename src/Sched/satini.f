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
      INTEGER           MSKEY, MODE, INSCH, KEYPTR, I, ISF, KTLE
      INTEGER           LEN1
      PARAMETER         (MSKEY= 300)
      INTEGER           KI(MSKEY)
      CHARACTER         KC(MSKEY)*8, KCHAR*256
      DOUBLE PRECISION  KD(2*MSKEY), ENDMARK, BLANK8, NONE
      LOGICAL           GOTKEYS, SATERR
      SAVE              KI, KD, KC, ENDMARK, GOTKEYS, BLANK8, NONE
C
      DATA          (KI(I),I=1,3)   / MSKEY, 0, 3 /
      DATA          GOTKEYS         / .FALSE. /
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SATINI: starting.' )
C
      IF( .NOT. GOTKEYS ) THEN
C
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK8 )
         CALL KPACK( 'NONE    ', NONE )
C
C        Set up to read the input information.  Only do once (can get
C        into this routine more than once if RESTART is pressed while
C        in plot mode.
C
         CALL KEYCHR( 'SATNAME', ' ', 12, KD, KC, KI )
         CALL KEYADD( 'SATNUM', UNSET, 1, KD, KC, KI )
         CALL KEYCHR( 'SATFILE', 'NONE', 128, KD, KC, KI )
         CALL KEYCHR( 'TLEFILE', 'NONE', 128, KD, KC, KI )
         CALL KEYCHR( 'KERFILE', 'NONE', 128, KD, KC, KI )
         CALL KEYADD( 'ENDSAT', UNSET, 1, KD, KC, KI )
      END IF
      KD( KEYPTR( 'ENDSAT', KC, KI ) ) = UNSET     
C
C     Loop through the input groups.  NSAT was initialized in SCHIN.
C
  100 CONTINUE
         MODE = 0
         SATERR = .FALSE.
C
C        Initialize the file names, to be sure if one is not specified
C        it is blank.  Whether or not SATFILE or TLEFILE is there is how
C        SCHED tells with reading routines to call.  Allow the KERFILE
C        to carry from one satellite to another because it doesn't need
C        to change from one to another usually.  Recall that the KD array
C        is double precision, not character.  Characters are in holerith
C        (Keyin was designed a very long time ago).  Note used KTLE because
C        ITLE is the unit number for reading the TLE file.
C
         ISF = KEYPTR( 'SATFILE', KC, KI )
         KTLE = KEYPTR( 'TLEFILE', KC, KI )
         KD(ISF) = NONE
         KD(KTLE) = NONE
         DO I = 1, 15
            KD(ISF+I) = BLANK8
            KD(KTLE+I) = BLANK8
         END DO
C
         CALL KEYIN( KD(MSKEY+1), KD, KI(2), ENDMARK, MODE, INSCH, 6 )
         IF( MODE .EQ. 1 .OR. 
     1       KD( KEYPTR( 'ENDSAT', KC, KI ) ) .EQ. 0.D0 ) THEN
            GO TO 200
         ELSE
            NSAT = NSAT + 1
            SATNAME(NSAT) = KCHAR( 'SATNAME', 12, .TRUE., KD, KC, KI )
            SATNUM(NSAT)  = KD( KEYPTR( 'SATNUM', KC, KI ) )
            SATFILE(NSAT) = KCHAR( 'SATFILE', 128, .FALSE., KD, KC, KI )
            CALL ENVIR( SATFILE(NSAT) )
            TLEFILE(NSAT) = KCHAR( 'TLEFILE', 128, .FALSE., KD, KC, KI )
            CALL ENVIR( TLEFILE(NSAT) )
            KERFILE(NSAT) = KCHAR( 'KERFILE', 128, .FALSE., KD, KC, KI )
            CALL ENVIR( KERFILE(NSAT) )
C
C           Reflect the input data to the log file
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2, A, I8 )' ) 
     1                    'SATINI: Satellite', NSAT, SATNAME(NSAT),
     2                    SATNUM(NSAT)
            CALL WLOG( 0, MSGTXT )
            CALL WLOG( 0, '        KERFILE: ' //
     1          KERFILE(NSAT)(1:LEN1(KERFILE(NSAT))) )
            CALL WLOG( 0, '        SATFILE: ' //
     1          SATFILE(NSAT)(1:LEN1(SATFILE(NSAT))) )
            CALL WLOG( 0, '        TLEFILE: ' //
     1          TLEFILE(NSAT)(1:LEN1(TLEFILE(NSAT))) )
C
C           Sanity check:
C
            IF( SATFILE(NSAT)(1:4) .NE. 'NONE' .AND.
     1          TLEFILE(NSAT)(1:4) .NE. 'NONE' ) THEN
               MSGTXT = ' '
               MSGTXT = 'A satellite can only have a SATFILE'//
     1              ' or a TLEFILE, not both.' 
               CALL WLOG( 1, MSGTXT )
               SATERR = .TRUE.
            END IF
            IF( SATFILE(NSAT)(1:4) .EQ. 'NONE' .AND.
     1          TLEFILE(NSAT)(1:4) .EQ. 'NONE' ) THEN
               MSGTXT = ' '
               MSGTXT = 'SATINI:  A satellite must have a SATFILE'//
     1              ' or a TLEFILE.' 
               CALL WLOG( 1, MSGTXT )
               SATERR = .TRUE.
            END IF
            IF( SATERR ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, 2X, A )' ) 
     1            '  The problem occurred with satellite ', NSAT, 
     2            SATNAME(NSAT) 
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' )
     1           '     SATFILE:   ', SATFILE(I)(1:LEN1(SATFILE(I)))
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' )
     1           '     TLEFILE:   ', TLEFILE(I)(1:LEN1(SATFILE(I)))
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' One of the file names should be ''NONE'', ' //
     1             '  which is the default.'
               CALL ERRLOG( MSGTXT )
            END IF
C
         END IF
         GO TO 100
C
  200 CONTINUE
      RETURN
      END
