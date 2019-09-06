      SUBROUTINE PLOTTY( MKFILES, RESTART, DIDRST )
C
C     Routine for sched that selects plot type from terminal
C     input instead of XWindow.
C
      INCLUDE 'sched.inc'
C
      CHARACTER   TYPE*8, XAXIS*8, YAXIS*8, PLOTFILE*80, LASTPF*80
      CHARACTER   PLSRC*12, PLSTA*8, ANSWER*8
      LOGICAL     MKFILES, DOEXIT, POPEN, RESTART, COLOR, SCREEN
      INTEGER     I1, I2, LEN1, I, ISET, KSET, PGBEG
      INTEGER     ISRC, KSRC, ISTA, KSTA, LANSW, IC1, IC2
      DOUBLE PRECISION  RXMIN, RXMAX, RYMIN, RYMAX
C
C     For KEYIN.
C
      INTEGER           MPLK, MODE
      PARAMETER         (MPLK=50)
      INTEGER           KI(MPLK), KEYPTR
      CHARACTER         KC(MPLK)*8, KCHAR*256
      DOUBLE PRECISION  KD(2*MPLK), ENDMARK, BLANK
      LOGICAL           GOTKEYS, DIDRST
C
      SAVE              GOTKEYS, ENDMARK, BLANK, POPEN
      SAVE              KD, KI, KC
C
      DATA          (KI(I),I=1,3)   / MPLK, 0, 3 /
      DATA          GOTKEYS / .FALSE. /
C ------------------------------------------------------------------
      IF( DEBUG ) CALL PUTOUT( 'PLOTTY: Starting. ' )
C
      IF( .NOT. PLOT ) THEN
         MKFILES = .TRUE.
      ELSE
C
         IF( .NOT. GOTKEYS ) THEN
            CALL KPACK( '/       ', ENDMARK )
            CALL KPACK( '        ', BLANK )
C
C           Set up the KEYIN inputs.
C
            CALL KEYCHR( 'PLotfile', '/xs', 80, KD, KC, KI )
            CALL KEYCHR( 'TYpe', 'UV', 2, KD, KC, KI )
            CALL KEYCHR( 'XAxis', 'GST', 8, KD, KC, KI )
            CALL KEYCHR( 'YAxis', 'EL', 8, KD, KC, KI )
            CALL KEYADD( 'XLeft', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'XRight', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'YBottom', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'YTop', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'SEtnum', 0.D0, 1, KD, KC, KI )
            CALL KEYCHR( 'SOurce', 'ALL', 12, KD, KC, KI )
            CALL KEYCHR( 'STation', 'ALL', 8, KD, KC, KI )
            CALL KEYADD( 'REstart', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'EXit', UNSET, 1, KD, KC, KI )
            CALL KEYADD( 'FInish', UNSET, 1, KD, KC, KI )
            GOTKEYS = .TRUE.
            POPEN = .FALSE.
         END IF
C
C        Initializations for after each restart.
C
         LASTPF = ' '
         KD( KEYPTR( 'REstart', KC, KI ) ) = UNSET
C
C        Give instructions concerning KEYIN input.
C
C        First list the setups.
C
         CALL PUTOUT( ' ' )
         CALL PUTOUT( ' ==================  PLOTTING  ===============' )
         CALL PUTOUT( ' Setup numbers will be needed.  They are: ' )
         CALL PUTOUT( ' Number    Setup ' )
         DO ISET = 1, NSETF
            I2 = LEN1( SETFILE(ISET) )
            IF( I2 .GT. 80 - 7 ) THEN
               I1 = I2 - ( 80 - 7 ) + 1
            ELSE
               I1 = 1
            END IF
            WRITE( MSGTXT, '( I5, A, A )' ) ISET, ': ', 
     1              SETFILE(ISET)(I1:I2)
            CALL PUTOUT( MSGTXT )
         END DO
C
C        Now give the other parameters.
C
         CALL PUTOUT( ' ' )
         CALL PUTOUT( 'INPUT PARAMETERS: (2 characters enough) ' )
         CALL PUTOUT( '  PLotfile:  Plot file name.' )
         CALL PUTOUT( '  TYpe:      Type of plot (UV, XY, RD)' )
         CALL PUTOUT( '               Scale both with XRight etc.' )
         CALL PUTOUT( '               RD is RA-Dec.' )
         CALL PUTOUT( '  XAxis:     For XY - X axis ( UT, GST,'
     1                                  // ' AZ, EL, HA, PA ).' )
         CALL PUTOUT( '  YAxis:     For XY - Y axis ('
     1                                  // ' AZ, EL, HA, PA ).' )
         CALL PUTOUT( '              Only sensible combinations work.' )
         CALL PUTOUT( '  XLeft:     X axis minimum (times in hh:mm:ss'
     1                             // ' from start of first day.)' )
         CALL PUTOUT( '  XRight:    X axis maximum.' //
     1                             '  Set -9999. to get default.' )
         CALL PUTOUT( '  YBottom:   Y axis minimum.')
         CALL PUTOUT( '  YTop:      Y axis maximum.')
         CALL PUTOUT( '  SEtnum:    Setup number (See list. 0 = all).' )
         CALL PUTOUT( '  SOurce:    Source name for UV plot (or ALL).' )
         CALL PUTOUT( '  STation:   Station for XY plots (or ALL).' )
         CALL PUTOUT( '  REstart:   Reread input file. ' //
     1                    ' Experiment defaults will not be reset.' )
         CALL PUTOUT( '  EXit:      Quit program.' //
     1                                 ' Write only .sum file.' )
         CALL PUTOUT( '  FInish:    Finish program writing all files. '
     1                            // ' Not allowed after RESTART. ' )
         CALL PUTOUT( ' ' )
         CALL PUTOUT( 'Some useful KEYIN features: ' )
         CALL PUTOUT( '   SAVE <filename>:  Saves current inputs to '
     1                            // 'file' )
         CALL PUTOUT( '   @<filename>:      Gets inputs from a file.' )
         CALL PUTOUT( '   SHOW:             Displays current inputs.' )
C
  100    CONTINUE
C
C           Get input.
C
            MODE = 0
            CALL KEYIN( KD(MPLK+1), KD, KI(2), ENDMARK, MODE, 5, 6 )
            IF( MODE .EQ. 1 ) THEN
               MKFILES = .TRUE.
               GO TO 990
            END IF
C
C           Go back and read inputs over again?
C
            RESTART = KD( KEYPTR( 'REstart', KC, KI ) ) .EQ. 0.D0
            IF( RESTART ) THEN
               CLOSE( UNIT=ISUM, STATUS='DELETE' )
               DIDRST = .TRUE.
               MKFILES = .FALSE.
               GO TO 990
            END IF
C
C           Exit?  Write output files?  Don't allow output files
C           after restarts.
C
            DOEXIT = KD( KEYPTR( 'EXit', KC, KI ) ) .EQ. 0.D0
            MKFILES = KD( KEYPTR( 'FInish', KC, KI ) ) .EQ. 0.D0
            IF( MKFILES .AND. MISSING ) THEN
               DOEXIT = .TRUE.
               MKFILES = .FALSE.
               CALL PUTOUT( 'PLOTTY: Will not write output files '//
     1              ' because of missing information (cover?)' )
            END IF
            IF( MKFILES .AND. DIDRST ) THEN
               DOEXIT = .TRUE.
               MKFILES = .FALSE.
               CALL PUTOUT( 'PLOTTY: Will not write output files' //
     1                   ' after RESTART.  Parameters might be wrong.' )
               CALL PUTOUT( 'PLOTTY: Rerun SCHED with input file.' )
            END IF
            IF( DOEXIT .OR. MKFILES ) GO TO 990
C
C           Prepare for plotting.
C
            PLOTFILE = KCHAR( 'PLotfile', 80, .FALSE., KD, KC, KI )
            TYPE  = KCHAR( 'TYpe', 2, .TRUE., KD, KC, KI )
            XAXIS = KCHAR( 'XAxis', 8, .TRUE., KD, KC, KI )
            YAXIS = KCHAR( 'YAxis', 8, .TRUE., KD, KC, KI )
            RXMIN = KD( KEYPTR( 'XLeft', KC, KI ) )
            RXMAX = KD( KEYPTR( 'XRight', KC, KI ) )
            RYMIN = KD( KEYPTR( 'YBottom', KC, KI ) )
            RYMAX = KD( KEYPTR( 'YTop', KC, KI ) )
            KSET  = KD( KEYPTR( 'SEtnum', KC, KI ) )
            PLSRC = KCHAR( 'SOurce', 12, .TRUE., KD, KC, KI )
            PLSTA = KCHAR( 'STation', 8, .TRUE., KD, KC, KI )
C
C           Get source numbers
C
            IF( PLSRC .EQ. 'ALL' ) THEN
               KSRC = 0
            ELSE
               DO ISRC = 1, NSRC
                  IF( PLSRC .EQ. SRCNAME(ISRC) ) THEN
                     KSRC = ISRC
                     GO TO 200
                  END IF
               END DO
               CALL PUTOUT( 'PLOTTY: '//PLSRC//
     1              ' is not in this schedule.' )
               GO TO 100
  200          CONTINUE
            END IF
C
C           Get the station number.
C
            IF( PLSTA .EQ. 'ALL' ) THEN
               KSTA = 0
            ELSE
               DO ISTA = 1, NSTA
                  IF( STANAME(ISTA) .EQ. PLSTA ) THEN
                     KSTA = ISTA
                     GO TO 300
                  END IF
               END DO
               CALL PUTOUT( 'PLOTSKY: '//PLSTA//
     1                      ' is not in this schedule. ' )
               GO TO 100
  300          CONTINUE
            END IF
C
C           Open the plot file if not already open.
C
            IF( PLOTFILE .NE. LASTPF ) THEN
               IF( POPEN ) THEN
                  CALL PGIDEN
                  CALL PGEND
                  POPEN = .FALSE.
               END IF
               IF( PGBEG( 0, PLOTFILE, 1, 1 ) .NE. 1 ) THEN
                  CALL PUTOUT( ' Bad plot file ' )
                  GO TO 100
               ELSE              
                  LASTPF = PLOTFILE
                  POPEN = .TRUE.
               END IF
            END IF
C
C           Get the plot type.  If hardcopy, have to use different
C           background and fill colors to get plots right.  If no
C           color capability, don't set colors.`
C
            CALL PGQINF( 'HARDCOPY', ANSWER, LANSW )
            SCREEN = ANSWER(1:3) .EQ. 'NO'
            CALL PGQCOL( IC1, IC2 )
            COLOR = IC2 .GT. 1
C
C           Go to the active subroutines.
C
            IF( TYPE .EQ. 'UV' ) THEN
               CALL PLOTUV( 12, KSET, KSRC, KSTA, SCREEN, COLOR,
     1                      RXMIN, RXMAX, RYMIN, RYMAX )
            ELSE IF( TYPE .EQ. 'XY' ) THEN
               IF( PLSTA .EQ. 'NONE' ) THEN
                  CALL PUTOUT( 'PLOTTY: Please specify station.' )
                  GO TO 100
               ELSE
                  CALL PLOTXY( KSET, KSRC, KSTA, XAXIS, YAXIS,
     1               SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
               END IF
            ELSE IF( TYPE .EQ. 'RD' ) THEN
                  CALL PLOTRD( SCREEN, COLOR, 
     1               RXMIN, RXMAX, RYMIN, RYMAX )
            ELSE
               CALL PUTOUT( 'PLOTTY:  Invalid TYPE. ' )
               GO TO 100
            END IF
C
C           Return for another command set.
C
            GO TO 100
C
  990    CONTINUE
C
         IF( POPEN )  THEN
            CALL PGEND
            POPEN = .FALSE.
         END IF
C
      END IF
C
      RETURN
      END
