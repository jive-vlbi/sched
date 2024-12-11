      SUBROUTINE RDPEAK( IUPEAK )
C
C     Subroutine for SCHED, called by INPUT, that reads the PEAKFILE
C     which contains instructions related to automatic insertion
C     or use of reference pointing scans.
C     IUPEAK is the input unit number.  In call because can be 
C     separate file (IUPK) or in-line in main keyin input (INSCH)
C     Assume that the file only needs to be opened if IUPEAK=IUPK.
C     Also assume that if IUPEAK.NE.IUPK, that we really must read
C     the peaking input (in-line).
C
C     Code maintenance June 2009 - move the input setup to after 
C     deciding whether to skip the routine.  RCW
C
      INCLUDE      'sched.inc'
      INCLUDE      'schpeak.inc'
C
      INTEGER           IER, VLBOPE, ISTA, ISRC, IUPEAK
      INTEGER           ISCN, LEN1
      CHARACTER         RESULT*256
C
      INTEGER           MSP, I, I1
      LOGICAL           GOTKEYS
      PARAMETER         (MSP = 210+MPKSTA+2*MPKSRC)
      INTEGER           KI(MSP), KEYPTR, MODE
      CHARACTER         KC(MSP)*8, KCHAR*256, FILEUP*80, FILENAME*80
      DOUBLE PRECISION  KD(MSP*2), ENDMRK, BLANK
      SAVE              GOTKEYS
C
      DATA   (KI(I),I=1,3)     / MSP, 0, 3 /
      DATA   GOTKEYS   / .FALSE. /
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'RDPEAK starting' )
C
C     On first call, initialize group counter and PKGROUP.  Good to 
C     do even if not reading information.  At least NPKGRP is used 
C     to tell other routines not to look for information.
C
      IF( .NOT. GOTKEYS ) THEN
         NPKGRP = 0
         DO ISTA = 1, NSTA
            PKGROUP(ISTA) = 0
         END DO
      END IF
C     
C     Just return if not going to do anything else.
C     If supposed to read a file, but the file is "NONE", return.
C     First set DOPOINT based on whether pointing will be used
C     at all.  This only really works in the call from INPUT
C     as AUTOPEAK etc may not yet be set at the time of the 
C     inline call.  Also the scans won't be read by then so NSCANS may
C     be zero.  DOPOINT will be set to true temporarily in an in-line
C     call to trigger reading the file.
C
      IF( IUPEAK .EQ. IUPK ) THEN
         DOPOINT = AUTOPEAK .OR. IUPEAK .NE. IUPK
         DO ISCN = 1, NSCANS
            IF( POINT(ISCN) .GE. 0 ) DOPOINT = .TRUE.
         END DO
      ELSE
         DOPOINT = .TRUE.
      END IF
C
C     Don't want to read if PEAKFILE=NONE.
C
      FILEUP = PEAKFILE
      CALL UPCASE( FILEUP )
C
      IF( ( IUPK .EQ. IUPEAK .AND. FILEUP .EQ. 'NONE' ) .OR. 
     1      .NOT. DOPOINT ) GO TO 999
C
C     Need to worry about peaking.
C
C     Set up the input parameters.  
C
      IF( .NOT. GOTKEYS ) THEN
         CALL KPACK( '/       ', ENDMRK )
         CALL KPACK( '        ', BLANK )
         CALL SCHDEFS( 'refpointing', FILENAME )
         CALL KEYCHR( 'SRCFILE', FILENAME, 80, KD, KC, KI )
         CALL KEYCHR( 'SETUP', ' ', 80, KD, KC, KI )
         CALL KEYCHR( 'SETUPL', ' ', 80, KD, KC, KI )
         CALL KEYADD( 'LINEINIT', UNSET, 1, KD, KC, KI )
         CALL KEYADD( 'MINFREQ', 60.D3, 1, KD, KC, KI )
         CALL KEYADD( 'MINEL', 30.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DWELL', 60.D0, 1, KD, KC, KI )
         CALL KEYADD( 'STATIONS', BLANK, MPKSTA, KD, KC, KI )
         CALL KEYCHR( 'LINENAME', ' ', 8, KD, KC, KI )
         CALL KEYCHR( 'VLAMODE', ' ', 2, KD, KC, KI )
         CALL KEYCHR( 'ENDPEAK', ' ', 2, KD, KC, KI )
         DO I = 1, MPKSRC
            CALL KEYCHR( 'SOURCES', ' ', 12, KD, KC, KI )
         END DO
         GOTKEYS = .TRUE.
      END IF
C
C     Open the peak file.
C
      IF( IUPK .EQ. IUPEAK ) THEN
         IER = VLBOPE( IUPEAK, PEAKFILE, 'TEXT', 'OLD', RESULT )
         IF( IER .NE. 1 ) THEN
            CALL WLOG( 1, RESULT )
            CALL WLOG( 1, 'RDPEAK: Automatic insertion and/or '//
     1         'conversion of scans was requested.' )
            CALL WLOG( 1, 
     1        '        PEAKFILE needed, but could not be opened.'  )
            CALL WLOG( 1, 
     1        '        Problem is with file: ' )
            CALL WLOG( 1, PEAKFILE )
            CALL WLOG( 1, 
     1        '        Note that file peak.cmd could be used' )
            CALL WLOG( 1, '        It is with the standard catalogs.' )
            CALL ERRLOG( 
     1          ' Fix PEAKFILE or do not invoke AUTOPEAK or POINT.' )
         END IF 
      END IF
C
C     Loop over input groups.
C
  100 CONTINUE
C
C        Reset the station list to none.
C 
         I1 = KEYPTR( 'STATIONS', KC, KI ) - 1
         DO ISTA = 1, MPKSTA
            KD(ISTA+I1) = BLANK
         END DO
C 
C        Reset the source list to none.
C 
         I1 = KEYPTR( 'SOURCES', KC, KI ) - 1
         DO ISRC = 1, MPKSRC
            KD(2*ISRC+I1-1) = BLANK
            KD(2*ISRC+I1) = BLANK
         END DO
C
C        Jump here if have read line frequencies so that any 
C        parameters already set for this group won't be reset.
C
  200    CONTINUE
C
C        Get the inputs.
C
         MODE = 0
         CALL KEYIN( KD(MSP+1), KD, KI(2), ENDMRK, MODE, IUPEAK, 6 )
         IF( MODE .EQ. 1 ) GO TO 900
         IF( KD( KEYPTR( 'ENDPEAK', KC, KI ) ) .EQ. 0.D0 ) GO TO 900
C
C        Decode any line frequencies provided.
C
         I1 = KEYPTR( 'LINEINIT', KC, KI )
         IF( KD(I1) .NE. UNSET ) THEN
            IF( DEBUG ) CALL WLOG( 0, 'RDPEAK: Reading line freqs.' )
            CALL RFREQ( IUPEAK )
            KD(I1) = UNSET
            GO TO 200
         END IF
C
C        Got a group.
C
         NPKGRP = NPKGRP + 1
         IF( NPKGRP .GT. MPKGRP ) THEN
            WRITE( MSGTXT, '( A, I3, A, A )' ) 
     1           'RDPEAK:  Too many reference pointing groups (Max ',
     2           MPKGRP, ') in: ', PEAKFILE(1:LEN1(PEAKFILE))
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Decode the inputs.
C
         PKLINES(NPKGRP) = KCHAR( 'LINENAME', 8, .TRUE., KD, KC, KI )
         PVLAMODE(NPKGRP) = KCHAR( 'VLAMODE', 2, .TRUE., KD, KC, KI )
         PKDWELL(NPKGRP) = KD( KEYPTR( 'DWELL', KC, KI ) ) / 86400.D0
         PKMINFQ(NPKGRP) = KD( KEYPTR( 'MINFREQ', KC, KI ) )
         PKMINEL(NPKGRP) = KD( KEYPTR( 'MINEL', KC, KI ) )
C
C        The setup files.  Add them to the overall list if needed.  They
C        will be read along with the rest later by GETSET.  Note that 
C        SETUP is for continuum sources, SETUPL is for line sources
C        (Those with CALCODE='L' - they want narrow bandwidth).
C
         PSETFILE(NPKGRP) = KCHAR( 'SETUP', 80, .FALSE., KD, KC, KI )
         CALL ENVIR( PSETFILE(NPKGRP) )
         PLSETFIL(NPKGRP) = KCHAR( 'SETUPL', 80, .FALSE., KD, KC, KI )
         CALL ENVIR( PLSETFIL(NPKGRP) )
C
C        If SETUPL was not given, set it to SETUP, but write a note to that
C        effect.
C
         IF( PLSETFIL(NPKGRP) .EQ. ' ' ) THEN
            PLSETFIL(NPKGRP) = PSETFILE(NPKGRP)
            CALL WLOG( 1, 'RDPEAK:  WARNING:  Your peak command file '//
     1          'does not have a separate setup file' )
            CALL WLOG( 1, '         for spectral line (narrow band) '//
     1          'sources.  You may encounter errors' )
            CALL WLOG( 1, '         because the digital backends '//
     1          'require sample rate = 2 times bandwidth ' )
            CALL WLOG( 1, '         and the sample rate can only be '//
     1          'changed with a new setup file.' )
         END IF
C
C        See if these setups have been requested before.  If not, add them
C        to SETFILE list.  Also get the index of the setup for use in making
C        the pointing scans.
C
         PKLSET(NPKGRP) = 0
         IF( NSETF .GT. 0 ) THEN
            DO I = 1, NSETF
               IF( PSETFILE(NPKGRP) .EQ. SETFILE(I) ) THEN
                  PKLSET(NPKGRP) = I
               END IF
            END DO
         END IF
C
         IF( PKLSET(NPKGRP) .EQ. 0 ) THEN
            IF( NSETF .LT. MAXSET ) THEN
               NSETF = NSETF + 1
               SETFILE(NSETF) = PSETFILE(NPKGRP)
               PKLSET(NPKGRP) = NSETF
            ELSE
               CALL ERRLOG( 'RDPEAK: Exceeded limit on number of setup '
     1              // 'files while adding ones needed for reference '
     2              // 'pointing.' )
            END IF
         END IF
C
C        Do the line pointing setup.
C
         PKLSETL(NPKGRP) = 0
         IF( NSETF .GT. 0 ) THEN
            DO I = 1, NSETF
               IF( PLSETFIL(NPKGRP) .EQ. SETFILE(I) ) THEN
                  PKLSETL(NPKGRP) = I
               END IF
            END DO
         END IF
C
         IF( PKLSETL(NPKGRP) .EQ. 0 ) THEN
            IF( NSETF .LT. MAXSET ) THEN
               NSETF = NSETF + 1
               SETFILE(NSETF) = PLSETFIL(NPKGRP)
               PKLSETL(NPKGRP) = NSETF
            ELSE
               CALL ERRLOG( 'RDPEAK: Exceeded limit on number of setup '
     1              // 'files while adding line ones needed for '
     2              // 'reference pointing.' )
            END IF
         END IF
C
C        The station list.
C
         I1 = KEYPTR( 'STATIONS', KC, KI ) - 1
         NPKSTA(NPKGRP) = 0
         DO I = 1, MPKSTA
            ISTA = NPKSTA(NPKGRP) + 1
            IF( ISTA .GT. MPKSTA .AND. KD(I+I1) .NE. BLANK ) THEN
               WRITE( MSGTXT, '( A, I3, A, A )' ) 
     1           'RDPEAK:  Too many reference pointing stations (Max ',
     2           MPKSTA, ') in: ', PEAKFILE(1:LEN1(PEAKFILE))
               CALL ERRLOG( MSGTXT )
            END IF
            IF( KD(I+I1) .NE. BLANK ) THEN
               NPKSTA(NPKGRP) = ISTA
               WRITE( PKSTA(ISTA,NPKGRP), '(A8)' ) KD(I+I1)
               CALL UPCASE( PKSTA(ISTA,NPKGRP) )
            ELSE
               GO TO 700
            END IF
         END DO
  700    CONTINUE
C
C        If no stations were given, use the same list as the last
C        group.  
C
         IF( NPKSTA(NPKGRP) .EQ. 0 .AND. NPKGRP .GT. 1 ) THEN
            DO ISTA = 1, NPKSTA(NPKGRP-1)
               PKSTA(ISTA,NPKGRP) = PKSTA(ISTA,NPKGRP-1)
            END DO
            NPKSTA(NPKGRP) = NPKSTA(NPKGRP-1)
         ELSE IF( NPKSTA(NPKGRP) .EQ. 0 ) THEN
            CALL ERRLOG( 'RDPEAK:  First peak group has no stations.' )
         END IF
C
C        The source list.
C
         I1 = KEYPTR( 'SOURCES', KC, KI ) - 1
         NPKSRC(NPKGRP) = 0
         DO I = 1, MPKSRC
            ISRC = NPKSRC(NPKGRP) + 1
            IF( ISRC .GT. MPKSRC .AND. KD(2*I+I1-1) .NE. BLANK ) THEN
               WRITE( MSGTXT, '( A, I3, A, A )' ) 
     1           'RDPEAK:  Too many reference pointing sources (Max ',
     2           MPKSRC, ') in: ', PEAKFILE(1:LEN1(PEAKFILE))
               CALL ERRLOG( MSGTXT )
            END IF
            IF( KD(2*I+I1-1) .NE. BLANK ) THEN
               NPKSRC(NPKGRP) = ISRC
               WRITE( PKSRC(ISRC,NPKGRP), '( A8, A4 )' ) 
     1             KD(2*I+I1-1), KD(2*I+I1)
               CALL UPCASE( PKSRC(ISRC,NPKGRP) )
            ELSE
               GO TO 800
            END IF
         END DO
  800    CONTINUE
C
C        If no sources were given, use the same list as the last
C        group.  
C
         IF( NPKSRC(NPKGRP) .EQ. 0 .AND. NPKGRP .GT. 1 ) THEN
            DO ISRC = 1, NPKSRC(NPKGRP-1)
               PKSRC(ISRC,NPKGRP) = PKSRC(ISRC,NPKGRP-1)
            END DO
            NPKSRC(NPKGRP) = NPKSRC(NPKGRP-1)
         ELSE IF( NPKSRC(NPKGRP) .EQ. 0 ) THEN
            CALL ERRLOG( 'RDPEAK:  First peak group has no sources.' )
         END IF
C
C        Return for next point.
C
         GO TO 100
C
  900 CONTINUE
      IF( NPKGRP .EQ. 0 ) THEN
         CALL WLOG( 0,
     1        'RDPEAK:  No pointing instruction groups found in ' )
         CALL WLOG( 0,  PEAKFILE )
      END IF
C
C     Get the parameters that aren't supposed to change.  For now,
C     this is just the pointing source catalog.
C
      PSRCFILE = KCHAR( 'SRCFILE', 80, .FALSE., KD, KC, KI )
      CALL ENVIR( PSRCFILE )
C
C     Various loose ends requiring the station and source catalogs
C     will be tied up in PKFINISH called from DEFAULTS.
C
  999 CONTINUE
      RETURN
      END











