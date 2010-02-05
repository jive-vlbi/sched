      SUBROUTINE TPINI( IUTP, TPFILE )
C
C     Routine for SCHED to get tape initialization parameters for each
C     station.  KEYIN is used for input.  The input can either come from
C     the main SCHED input stream in a group following one containing 
C     the parameter TAPEINI (must be specified alone like SRCCAT and
C     STACAT), or from an input file (whose name was provided to the
C     main input stream by parameter TAPEFILE).  The input file can have
C     input data for many experiments.  TPINI will look for the record
C     for which EXPT is the same as EXPCODE from the main stream.
C
C     See the help file for details of the meaning of the parameters.
C
C     The inputs are obtained either from the main schedule after the
C     parameter TAPEINI is specified or from the file given by 
C     TAPEFILE.  SCHIN calls this routine with TPFILE set to the
C     following options with their implied actions.
C       TPFILE = 'NEXTLINE'  => call KEYIN on main input stream and use
C                               don't test EXPT.
C       TPFILE = 'NONE'      => don't call KEYIN.  Set up defaults.
C       TPFILE = anything else => open TPFILE and look for EXPT.
C
C     Store the values read temporarily in the RTP variables specified
C     in tpinit.inc.  Later they will be transfered on a per-station
C     basis to the arrays in sched.inc.  That transfer must wait 
C     because the station list is not necessarily known when this
C     routine is called.  It is done because some values may need
C     to change - for example NHDPOS will be set smaller for Mark IV
C     stations doing two head recordings, but that should not affect
C     other stations that might be included in a "DEFAULT" input set.
C
      INCLUDE 'sched.inc'
      INCLUDE 'tpinit.inc'
C
      LOGICAL           GOTDEF, GOTEXP
      INTEGER           VLBOPE, IER, JSTA, I, IUTP
      CHARACTER         TPFILE*80, RESULT*255, CNAME*50
      INTEGER           KEYPTR, IS, IR, IP, ID, II, IL, IN, IH, IB, IM
      SAVE              IS, IP, ID, II, IL, IN, IH, IB, IM
C
C     For defaults.
C
      INTEGER           MDEF
      PARAMETER         (MDEF=5)
      CHARACTER         DFSTA(MDEF)*8, DFDEN(MDEF)*1
      INTEGER           DFLENG(MDEF), DFNDR(MDEF), DFNHD(MDEF)
      SAVE              DFSTA, DFDEN, DFLENG, DFNDR, DFNHD
C
C     For KEYIN.
C
      INTEGER           MTPKEY, MODE, LEN1
      PARAMETER         (MTPKEY= 100+10*MAXSTA)
      INTEGER           KI(MTPKEY)
      CHARACTER         KC(MTPKEY)*8, KCHAR*256
      DOUBLE PRECISION  KD(2*MTPKEY), ENDMARK, BLANK
      LOGICAL           GOTKEYS, GOTMEDIA
      SAVE              KI, KD, KC, GOTKEYS, ENDMARK, BLANK, GOTMEDIA
C
      DATA          (KI(I),I=1,3)   / MTPKEY, 0, 3 /
      DATA          GOTKEYS         / .FALSE. /
      DATA          GOTMEDIA        / .FALSE. /
C
C     Set default tape lengths and station names.  Set default values in
C     last element of arrays.   Use 0 for DFNDR so that the station
C     catalog value is used.
C
      DATA (DFSTA(I), DFLENG(I), DFNDR(I), DFNHD(I), DFDEN(I), I=1,MDEF)
     1        / 'VLBA',    17600, 0, 14, 'H',
     2          'VLA',     17600, 0, 14, 'H',
     3          'EB_VLBA', 17600, 0, 14, 'H',
     4          'GB_VLBA', 17600, 0, 14, 'H',
     5          'DEFAULT', 17600, 0, 14, 'H' /
C ---------------------------------------------------------------
C     Set the input variables on first call.
C
      IF( .NOT. GOTKEYS ) THEN
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK )
         CALL KEYADD( 'TPSTA', BLANK, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'MEDIA', BLANK, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'TPTIME', -1.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'TPDRIVE', 1.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'TPINDEX', 1.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'TPLENGTH', 17600.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'NDRIVES', 0.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'NHEADPOS', 14.D0, MAXSTA, KD, KC, KI )
         CALL KEYADD( 'DENSITY', BLANK, MAXSTA, KD, KC, KI )
         CALL KEYCHR( 'OBSCODE', 'DEFAULT', 8, KD, KC, KI )
         CALL KEYADD( 'HEADMODE', BLANK, MAXSTA, KD, KC, KI )
C
         IF( DEBUG ) THEN
            WRITE( MSGTXT, '( A, I5, A, I5, A, I5 )' )
     1           'TPINI: MTPKEY: ', KI(1), '  Nkeys: ', KI(2),
     2        '  NKEYS: ', KI(3)
            CALL WLOG( 0, MSGTXT )
         END IF
      END IF
C
C     Get starting points for reading back the arrays.
C     Repeat on new call because variables not saved.
C
      IS = KEYPTR( 'TPSTA', KC, KI ) - 1
      IR = KEYPTR( 'MEDIA', KC, KI ) - 1
      IP = KEYPTR( 'TPTIME', KC, KI ) - 1
      ID = KEYPTR( 'TPDRIVE', KC, KI ) - 1
      II = KEYPTR( 'TPINDEX', KC, KI ) - 1
      IL = KEYPTR( 'TPLENGTH', KC, KI ) - 1
      IB = KEYPTR( 'DENSITY', KC, KI ) - 1
      IN = KEYPTR( 'NDRIVES', KC, KI ) - 1
      IH = KEYPTR( 'NHEADPOS', KC, KI ) - 1
      IM = KEYPTR( 'HEADMODE', KC, KI ) - 1
      GOTKEYS = .TRUE.
C
      GOTDEF = .FALSE.
      GOTEXP = .FALSE.
C
      IF( TPFILE .EQ. 'NONE' ) THEN
C
C        No tape initialization given.  Set defaults.
C        Since this whole tape initialization is basically obsolete
C        in the disk era, don't both users about using the defults.
C
C         IF( MARK2 .OR. VLBITP ) 
C     1       CALL WLOG( 0, 'TPINI:   '//
C     2         'Using default tape initialization information.' )
C
         RTPOBS  = 'DEFAULT'
         RNTPSTA = MDEF
         DO JSTA = 1, RNTPSTA
            RTPSTA(JSTA)   = DFSTA(JSTA)
            RMEDIA(JSTA)   = ' '
            RTPLENG(JSTA)  = DFLENG(JSTA)
            RTPSINDX(JSTA) = 1
            RTPSDRIV(JSTA) = 1
            RTPTIME(JSTA)  = -1.0D0
            RNDRIVE(JSTA)  = DFNDR(JSTA)
            RNHDPOS(JSTA)  = DFNHD(JSTA)
            RDENSITY(JSTA) = DFDEN(JSTA)
            RHEADMOD(JSTA) = ' '
         END DO
         GOTDEF = .TRUE.
C
      ELSE
C
C        Get tape initialization information from main input stream
C        if TPFILE is 'NEXTLINE' or from a file TPFILE.
C        Use unit IUTP which might be IUTAP or IUSCH.
C
         IF( TPFILE .EQ. 'NEXTLINE' ) THEN
C
            CALL WLOG( 0, 'TPINI:   Reading tape initialization '//
     1         'information from SCHED input file.' )
C
         ELSE
C
C           Open an external tape initialization file.
C
            IER = VLBOPE( IUTP, TPFILE, 'TEXT', 'OLD', RESULT )
            CNAME = TPFILE
            IF( IER .EQ. 0 ) THEN
               CALL WLOG( 1, RESULT )
               CALL ERRLOG( 'TPINI: Cannot find tape initialization '//
     1             'file ' // CNAME )
            END IF
            CALL WLOG( 0, 'TPINI: Reading tape initialization file: '//
     1              CNAME )
C
         END IF
C
C        Read the input file until desired experiment is found.
C
C        Store default settings if OBSCODE='DEFAULT' is encountered and
C        ultimately use these if the desired experiment is not found.
C
C        For input stream data, only call KEYIN once.
C
  100    CONTINUE
C
C           Initialize the input values of the station names to blank 
C           to help sense if the station list has changed.  This is
C           needed to deal with the case where the station list shrinks.
C
            DO JSTA = 1, MAXSTA
               KD(IS+JSTA) = BLANK
            END DO
C
C           Get input data.
C
            MODE = 0
            CALL KEYIN( KD(MTPKEY+1), KD, KI(2), ENDMARK, MODE, IUTP, 6)
            IF( MODE .EQ. 1 ) GO TO 200
C
C           Look for desired obscode.
C           For in-line data, do not yet know EXPCODE.  Set up for
C           later comparison.  Assume we have a match for now
C           and do a test later.
C
            RTPOBS =  KCHAR( 'OBSCODE', 8, .TRUE., KD, KC, KI )
            IF( TPFILE .EQ. 'NEXTLINE' ) THEN
               GOTDEF = .TRUE.
            ELSE
               IF( RTPOBS .EQ. EXPCODE ) GOTEXP = .TRUE.
               IF( RTPOBS .EQ. 'DEFAULT' ) GOTDEF = .TRUE.
            END IF
C
            IF( GOTEXP .OR. GOTDEF ) THEN
C
C              Redetermine the station list and number of stations if
C              new stations were specified.
C
               IF( KD(IS+1) .NE. BLANK ) THEN
                  RNTPSTA = 0
                  DO JSTA = 1, MAXSTA
                     IF( KD(IS+JSTA) .NE. BLANK ) THEN
                        RNTPSTA = RNTPSTA + 1
                        WRITE( RMEDIA(RNTPSTA), '(A)' ) KD(JSTA+IR)
                        CALL UPCASE( RMEDIA(RNTPSTA) )
                        IF( RMEDIA(RNTPSTA) .NE. ' ' ) GOTMEDIA = .TRUE.
                        WRITE( RTPSTA(RNTPSTA), '(A)' ) KD(JSTA+IS)
                        IF( LEN1( RTPSTA(RNTPSTA) ) .LE. 2 ) THEN
                           CALL WLOG( 1, 'TPINI: TPSTA must be the '//
     1                       'station name, not code.' )
                           CALL ERRLOG(  '       ''' // 
     1                       RTPSTA(RNTPSTA)(1:2) //
     2                       ''' is too short to be a station name.' )
                        END IF
                        CALL UPCASE( RTPSTA(RNTPSTA) )
                        RTPTIME(RNTPSTA)  = KD(JSTA+IP)
                        RTPSDRIV(RNTPSTA) = KD(JSTA+ID)
                        RTPSINDX(RNTPSTA) = KD(JSTA+II)
                        RTPLENG(RNTPSTA)  = KD(JSTA+IL)
                        RNDRIVE(RNTPSTA)  = KD(JSTA+IN)
                        RNHDPOS(RNTPSTA)  = KD(JSTA+IH)
                        WRITE( RDENSITY(RNTPSTA), '(A1)' ) KD(IB+JSTA)
                        CALL UPCASE( RDENSITY(RNTPSTA) )
                        WRITE( RHEADMOD(RNTPSTA), '(A8)' ) KD(IM+JSTA)
                        CALL UPCASE( RHEADMOD(RNTPSTA) )
                     END IF
                  END DO
               END IF
            END IF
C
C           Return for next experiment input if not in-line and
C           experiment not yet found.
C
            IF( .NOT. GOTEXP .AND. TPFILE .NE. 'NEXTLINE' ) THEN
               GO TO 100
            END IF
C
C        Input loop over.
C
  200    CONTINUE
C
C        Close tape initialization file if it was external.
C
         IF( TPFILE .NE. 'NEXTLINE' ) THEN
            CLOSE( UNIT=IUTP )
         END IF
C
C        Tweak the tails of the old timers with ancient templates.
C
         IF( .NOT. GOTMEDIA ) THEN
            CALL WLOG( 1, 'TPINI:   *** Do you really need TAPEINI ' //
     1            'data?' )
            CALL WLOG( 1, '             The vast majority of ' //
     1            'users do not.' )
            CALL WLOG( 1, '             Most TAPEINI sections ' //
     1            'betray the use of ancient templates.' )
            CALL WLOG( 1, '             TAPEINI sections can cause ' //
     1            'more harm than good by overriding current ' //
     2            'catalog information.' )
      END IF
C
      END IF
C
C     Tell user what we have.  Make this silent unless debugging.
C
      IF( GOTEXP ) THEN
         IF( DEBUG ) CALL WLOG( 0,
     1     'TPINI:   Got tape initialization data for '// EXPCODE )
      ELSE IF( TPFILE .NE. 'NEXTLINE' .AND. GOTDEF ) THEN
         IF( DEBUG ) CALL WLOG( 0, 'TPINI:   '//
     1     'Got tape initialization data for default experiment.' )
      ELSE IF( TPFILE .EQ. 'NEXTLINE' .AND. GOTDEF ) THEN
         IF( DEBUG ) CALL WLOG( 0,
     1     'TPINI:   Got tape initialization data. ' )
      ELSE
         CALL ERRLOG( 'TPINI:   Don''t have tape initialization '//
     1                'information.' )
      END IF
C
C     Do some checks.
C
      DO JSTA = 1, RNTPSTA
C
C        The number of drives will be checked against the station
C        catalog in TPTPNS.
C
C        Index position too high.
C
         IF( RTPSINDX(JSTA) .GT. RNHDPOS(JSTA) ) THEN
            CALL ERRLOG( 'TPINI: Starting head index greater than '//
     1          'number allowed at '//RTPSTA(JSTA) )
         END IF
C
C        Get a good density default.
C
         IF( RDENSITY(JSTA) .EQ. ' ' ) THEN
            IF( RTPLENG(JSTA) .GT. 12000 ) THEN
               RDENSITY(JSTA) = 'H'
            ELSE
               RDENSITY(JSTA) = 'L'
            END IF
         ELSE IF( RDENSITY(JSTA) .NE. 'H' .AND. 
     1            RDENSITY(JSTA) .NE. 'L' ) THEN
            CALL ERRLOG( 'TPINI: Invalid density specification: '''//
     1          RDENSITY(JSTA)//'''' )
         END IF
C
      END DO
C
C     That's all, folks.
C
      RETURN
      END
