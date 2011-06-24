      SUBROUTINE LISTFREQ
C
C     Routine for SCHED, called by GETFREQ, that makes a table of
C     the frequencies available in the frequency catalog.  The
C     user can provide limits for the frequency ranges.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schfreq.inc'
C
      INTEGER      IF, KF, I, NS, VLBOPE, IOERR, LEN1
      CHARACTER    OPSTAT*4, OPTEXT*256, PRLST*80 
      CHARACTER    FMTA*60, FMT1*60, FMT2*60, FMT3*60
      LOGICAL      DOIT, EXISTS, DOVLA(MFREQ)
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'LISTFREQ starting. ' )
C
C     Only do anything if something was requested.
C
      IF( FREQLIST(1) .NE. UNSET ) THEN
C
C        Set default for second frequency if needed.
C
         IF( FREQLIST(2) .EQ. UNSET ) FREQLIST(2) = FREQLIST(1)
C
C        Open the frequencies file.
C
         PRLST = 'frequencies.list'
         INQUIRE( FILE=PRLST, EXIST=EXISTS )
         IF( EXISTS ) THEN
            OPSTAT = 'OLD'
         ELSE
            OPSTAT = 'NEW'
         END IF
         IOERR = VLBOPE( IFRQLST, PRLST, 'TEXT', OPSTAT, OPTEXT )
         IF( IOERR .NE. 1 ) CALL ERRLOG( 'LISTFREQ: Open problem: '//
     1           OPTEXT )
C
C        Make tables header for main table.
C
         WRITE( IFRQLST, '( A )' )  ' FREQUENCY SETUPS'
         WRITE( IFRQLST, '( A, 2F10.2, A )' )  
     1         '    Frequency range: ', FREQLIST, ' MHz. '
         WRITE( IFRQLST, '( A , A )' )
     1         '    File: ', FREQFILE(1:LEN1(FREQFILE))
         WRITE( IFRQLST, '( A )' )
     1         '    Note that groups with low ''Prio'' are prefered.'
         WRITE( IFRQLST, '( 1X, /, A , A )' )
     1         ' Entry Prio IF   Low Freq  High Freq  Pol   ',
     2         'FirstLO   FE  Stations/Synth '       
C
C        Set up for variable format.
C
         FMTA = '( I5, I5, 3X, A2, 2F10.2, 3X, A3, F10.2, 2X, A4'
         FMT2 = FMTA(1:LEN1(FMTA)) // ' )'
         FMT3 = FMTA(1:LEN1(FMTA)) // ', 1X, 3F6.1 )'
C
C        Loop through frequency groups.
C
         DO KF = 1, NFREQ
C
C           Check frequency range.
C
            DOIT = .FALSE.
            DOVLA(KF) = .FALSE.
            DO IF = 1, FNIF(KF)
               IF( FREQLIST(1) .LT. FRF2(IF,KF) .AND.
     1             FREQLIST(2) .GT. FRF1(IF,KF) ) DOIT = .TRUE.
            END DO
C
C           If in range, write table entry.
C
            IF( DOIT ) THEN
               NS = 1
               DOVLA(KF) = FSTNAM(1,KF)(1:3) .EQ. 'VLA'
               DO I = 2, MFSTA
                  IF( FSTNAM(I,KF) .NE. ' ' ) NS = I
               END DO
               WRITE( FMT1, '( 2A, I2, A )' ) 
     1              FMTA(1:LEN1(FMTA)), ',', NS, '(2A) )'
               WRITE( IFRQLST, '( ''----'', A12, 64(''-'') )' )
     1              FRNAME(KF)
               DO IF = 1, FNIF(KF)
                  IF( IF .EQ. 1 .AND. .NOT. DOVLA(KF) ) THEN
                     WRITE( IFRQLST, FMT1 ) KF, PRIO(KF), 
     1                  FIFNAM(IF,KF), FRF1(IF,KF), FRF2(IF,KF),
     2                  FPOL(IF,KF), FLO1(IF,KF), FFE(IF,KF),
     3                  (' ', FSTNAM(I,KF)(1:LEN1(FSTNAM(I,KF))),I=1,NS)
                  ELSE IF( IF .EQ. 2 .AND. .NOT. DOVLA(KF) ) THEN
                     WRITE( IFRQLST, FMT3 ) KF, PRIO(KF), 
     1                  FIFNAM(IF,KF), FRF1(IF,KF), FRF2(IF,KF),
     2                  FPOL(IF,KF), FLO1(IF,KF), FFE(IF,KF),
     3                  FSYN(1,KF), FSYN(2,KF), FSYN(3,KF)
                  ELSE IF( DOVLA(KF) ) THEN
                     WRITE( IFRQLST, FMT1 ) KF, PRIO(KF), 
     1                  FIFNAM(IF,KF), FRF1(IF,KF), FRF2(IF,KF),
     2                  FPOL(IF,KF), FLO1(IF,KF), FFE(IF,KF),
     3                  ' ', FVCHNSTA(IF,KF)(1:LEN1(FVCHNSTA(IF,KF)))
                  ELSE
                     WRITE( IFRQLST, FMT2 ) KF, PRIO(KF), 
     1                  FIFNAM(IF,KF), FRF1(IF,KF), FRF2(IF,KF),
     2                  FPOL(IF,KF), FLO1(IF,KF), FFE(IF,KF)
                  END IF
               END DO
               IF( FRNOTE(KF) .NE. ' ' ) WRITE( IFRQLST, '( 2A )' )
     1              '  ', FRNOTE(KF)(1:LEN1(FRNOTE(KF)))
            END IF
         END DO
         WRITE( IFRQLST, '( 80(''-''), /, 80(''-'') )' )
C
C        Make tables of VLA special parameters.
C
         WRITE( IFRQLST, '( 1X, /, 1X, /, A )' )
     1         ' VLA Parameters.'
         WRITE( IFRQLST, '( 1X, /, A )' )
     1         ' Entry  BAND  BW    FEAB   FECD   SYNA    ' //
     2         'SYNB   FLUKEA  FLUKEB FEFILTER'
         WRITE( IFRQLST, '( 80(''-'') )' )
         DO KF = 1, NFREQ
            IF( DOVLA(KF) ) THEN
               WRITE( IFRQLST, '( I5, 4X, A2, 2X, A4, 2F7.2, '//
     1                  ' 2F8.1, 2F8.2, 3X, A4 )' )
     2              KF, FVBAND(KF), FVBW(KF), FVFEAB(KF), FVFECD(KF), 
     3              FVSYNA(KF), FVSYNB(KF), FVFLKA(KF), FVFLKB(KF), 
     4              FVFILT(KF)
            END IF
         END DO
         WRITE( IFRQLST, '( 80(''-''), /, 80(''-'') )' )
C
C        Now rewind the output file and spill it to the screen
C
         REWIND( UNIT=IFRQLST )
  200    CONTINUE
            READ( IFRQLST, '(A)', END=210 ) MSGTXT
            CALL WLOG( 1, MSGTXT )
            GO TO 200
  210    CONTINUE
C
         CALL WLOG(1, 'LISTFREQ: The above table was written to ' //
     1       'file ''frequencies.list''.' )
C
C        Close the frequencies file.
C
         CLOSE( UNIT=IFRQLST )
C
      END IF
C
      RETURN
      END
