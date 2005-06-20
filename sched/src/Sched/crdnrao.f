      SUBROUTINE CRDNRAO( ISCN, ISTA, FIRSTS )
C
C     Writes 'NRAO' format card images.
C       VSST cards get Ta and Ts.
C       PEAK cards cause antenna to peak up on source.
C       VOBS cards cause antenna to sit on source until stop time.
C
C     This routine is not really being maintained because I don't
C     know of anyone using it any more.  March 2003 RCW.
C
      INCLUDE  'sched.inc'
C
      INTEGER           IREP       ! Repeat counter for Tant.
      INTEGER           ISCN, ISTA, LSCN, LEN1, DAY, YEAR, I, J
      CHARACTER         S*1, SEQ*3, GMT*3, VSST*4, VOBS*4
      CHARACTER         PEAK*4, ITWO*2
      DOUBLE PRECISION  START, STOP
      LOGICAL           FIRSTS, DOTAPE
C      CHARACTER         TSYSCRD*76
      CHARACTER         TFORM*15, RASCH*15, DECSCH*15
      CHARACTER         EPOCH*6, CTIME*15, MARKIII*7
      SAVE              S, SEQ, GMT, VSST, VOBS, PEAK, ITWO, LSCN
C      SAVE              TSYSCRD
C
      DATA         S, SEQ, GMT, VSST, VOBS, PEAK, ITWO, MARKIII
     1             /'S', 'SEQ', 'GMT', 'VSST', 'VOBS', 'PEAK', '2',
     2             'MARKIII' /
      DATA         LSCN / 0 /
C  -----------------------------------------------------------------
      IF( DEBUG .AND. ( FIRSTS .OR. ISCN .LE. 3 ) )
     1    CALL WLOG( 0, 'CRDNRAO: Starting.' )
C
      IF( FIRSTS ) THEN
         CALL WLOG( 1, 'CRDNRAO:  NRAO format card output no being ' //
     1       'maintained.' )
         CALL WLOG( 1, '          If you need it, contact SCHED ' //
     1       'supporters to get support resumed.' )
         CALL WLOG( 1, '          Meanwhile, be very careful you ' //
     1       'are getting what you need.' )
      END IF
C
      IF( FIRSTS ) THEN
         CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, START )
         LSCN = ISCN
         WRITE(IULOC, '(''C  SCHEDULE FOR '',A,'' AT '', A, /, ' //
     1        '  ''C '',A)') EXPCODE, STATION(STANUM(ISTA)), EXPT
         CTIME = TFORM( START, 'T', 0, 2, 2, '::@' )
         WRITE(IULOC, '(''C  START AT '',A8, I8, '' DAY'', I4 )' )
     1          CTIME, YEAR, DAY
C
         DO I = 1, MCOVER
            J = MAX( LEN1( COVER(I) ), 1 )
            WRITE( IULOC, '( A )' ) 'C '//COVER(I)(1:J)
         END DO
C
      END IF
C
C     Add a TSYS card at end and quit if schedule done.
C     No longer doing this.
C
      IF( ISCN .EQ. -999 ) THEN
C         IF( LSCN .NE. 0 ) THEN
C            IF( .NOT. NOTSYS(LSCN) ) THEN
C               WRITE(IULOC,'(A)') TSYSCRD
C            END IF
C         END IF
      ELSE 
C
C        Write cards for the scan.
C
C        If there has been a source change, write out another VSST card.
C        ** This removed.  With demise of Mark II, most scans are
C        now short and loss of time between scans is more critical.
C        Therefore only do the calibration at the start of the scan.
C
C         IF( SCNSRC(ISCN) .NE. SCNSRC(LSCN) .AND. 
C     1       .NOT. FIRSTS .AND. .NOT. NOTSYS(LSCN) ) THEN
C            WRITE(IULOC,'(A)') TSYSCRD
C         END IF
C
C        Record last scan number.
C
         LSCN = ISCN
C
C     Get RA, DEC, and epoch for schedule.
C     STUBBED AT THE MOMENT BECAUSE I DON'T KNOW HOW TO FLAG EPOCHS.
C     WILL NEED TO DEAL WITH PROPER MOTION IF RESURECTED.
C
C         IF( C2000(SRCNUM(ISCN)).EQ.'*' ) THEN
C            RASCH = TFORM( RA2000(SRCNUM(ISCN)), 'T', 1, 2, 4, '@@@' )
C            DECSCH = TFORM( D2000(SRCNUM(ISCN)), ' ', 1, 2, 2, '@@@' )
C            EPOCH = '(2000)'
C         ELSE IF( C1950(SRCNUM(ISCN)).EQ.'*' ) THEN
            RASCH = TFORM( RA1950(SRCNUM(ISCN)), 'T', 0, 2, 4, '@@@' )
            DECSCH = TFORM( D1950(SRCNUM(ISCN)), ' ', 1, 2, 2, '@@@' )
            EPOCH = '(1950)'
C         ELSE
C            RASCH = TFORM( RAP(SRCNUM(ISCN)), 'T', 1, 2, 4, '@@@' )
C            DECSCH = TFORM( DECP(SRCNUM(ISCN)), ' ', 1, 2, 2, '@@@' )
C            EPOCH = '(date)'
C         END IF
C
C        Tape change.
C
         DOTAPE = MOD( TPDAT(1,ISCN,ISTA), 10 ) .GT. 0
         IF( DOTAPE ) THEN
            WRITE(IULOC,'(''C ------- '',A,'' -------'')')
     +            'Tape Change'
         END IF
C
C        Comment
C
         IF( ANNOT(ISCN) .NE. ' ' ) THEN
            WRITE( IULOC, '(''C ------- '',A,'' -------'')' )
     +           ANNOT(ISCN)(1:LEN1(ANNOT(ISCN)))
         END IF
C
C        Set up Ta measurement.
C
         IREP = 0
         IF( ( TANT1(ISCN) .AND. TANTS1(ISTA) ) .OR. 
     1       ( TANT2(ISCN) .AND. TANTS2(ISTA) ) ) IREP = 2
C
C        Now the cards.
C
C        Peak up the pointing if requested.
C
         IF( DOPEAK(ISCN) .GE. 0 )
     1          WRITE( IULOC, '( A1, 1X, A12, 1X, A1, 1X, ' //
     2          ' A8, 1X, A7, T48, A3, T73, A4)' )  S,
     3          SCNSRC(ISCN), ITWO, RASCH, DECSCH, SEQ, PEAK
C
C
C        Bob Vance suggested changing to the MARKIII proceedure.
C        This is a combination of VSST and VOBS.
C        He can customize things if necessary.  However, if
C        NOTSYS(ISCN) is specified, only do the VOBS proceedure.
C
C        MARKIII card.
C
         CALL TIMEJ( STOPJ(ISCN), YEAR, DAY, STOP )
         CTIME = TFORM( STOP, 'T', 0, 2, 2, '@@@' )
         IF( NOTSYS(ISCN) ) THEN
            WRITE( IULOC, '( A1, 1X, A12, 1X, A1, 1X, ' //
     1         ' A8, 1X, A7, T48, A3, 1X, A6, T73, A4)' )  S,
     2         SCNSRC(ISCN), ITWO, RASCH, DECSCH, 
     3         GMT, CTIME, VOBS
         ELSE
            WRITE( IULOC, '( A1, 1X, A12, 1X, A1, 1X,' //
     1         'A8, 1X, A7, T48, A3, 1X, A6, T62, I1, T73, A)' )  S,
     2         SCNSRC(ISCN), ITWO, RASCH, DECSCH, 
     3         GMT, CTIME, IREP, MARKIII
         END IF
C
C        Here is the old code in case I need it again:
C
C         IF( .NOT. NOTSYS(ISCN) ) THEN
C            WRITE( TSYSCRD, '( A1, 1X, A12, 1X, A1, 1X,' //
C     1          'A8, 1X, A7, T48, A3, T62, I1, T73, A4)')  S, 
C     2          SCNSRC(ISCN), ITWO, RASCH, DECSCH, 
C     3          SEQ, IREP, VSST
C            WRITE(IULOC,'(A)') TSYSCRD
C         END IF
C
C        VOBS card.
C
C         CALL TIMEJ( STOPJ(ISCN), YEAR, DAY, STOP )
C         CTIME = TFORM( STOP, 'T', 0, 2, 2, '@@@' )
C         WRITE( IULOC, '( A1, 1X, A12, 1X, A1, 1X,' //
C     1      'A8, 1X, A7, T48, A3, 1X, A6, T73, A4)' )  S,
C     2      SCNSRC(ISCN), ITWO, RASCH, DECSCH, 
C     3      GMT, CTIME, VOBS
C  
      END IF
C
      RETURN
      END
