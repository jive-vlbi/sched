      SUBROUTINE GETFREQ
C
C     Routine for SCHED, called by GETSET and DIVERT, that reads the 
C     catalog of known frequency setups (FREQFILE).  It stores the 
C     information in arrays in the schfreq include file.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schfreq.inc'
C
      INTEGER           I, IIF, I1, I2, I3, I4, I5
      INTEGER           VLBOPE, LEN1, IER
      DOUBLE PRECISION  BLANK, ZZZZ
      CHARACTER         RESULT*256
C
C     For KEYIN
C
      INTEGER           MFRQV, MODE, KEYPTR
      LOGICAL           GOTKEYS
      PARAMETER         (MFRQV=150)
      INTEGER           KI(MFRQV)
      CHARACTER         KC(MFRQV)*8, KCHAR*256
      DOUBLE PRECISION  KD(2*MFRQV), ENDMARK
      SAVE              GOTKEYS, KI, KC, KD, ENDMARK, BLANK, ZZZZ
C
      DATA          (KI(I),I=1,3)   / MFRQV, 0, 3 /
      DATA          GOTKEYS / .FALSE. /
C ---------------------------------------------------------------
C     Set the input variables.
C
      IF( .NOT. GOTKEYS ) THEN
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK )
         CALL KPACK( 'ZZZZ    ', ZZZZ )
         CALL INFREQ( DEBUG, KD, KC, KI )
         GOTKEYS = .TRUE.
      END IF
C
C     Assume that, if the subroutine is entered more than once,
C     it is a restart and we should reload the freq catalog.
C
      NFREQ = 0
C
C     Open frequencies catalog.  Indicate the attempt before 
C     doing it so output messages make more sense.
C
      CALL WLOG( 0, 'GETFREQ: Reading frequency file:  '//
     1              FREQFILE(1:LEN1(FREQFILE) ) )
      IER = VLBOPE( IFRQ, FREQFILE, 'TEXT', 'OLD', RESULT )
      IF( IER .NE. 1 ) THEN
         CALL WLOG( 1, RESULT )
         CALL ERRLOG( 'GETFREQ: Problem opening frequency file' )
      END IF
C
C     Read the groups from the frequence catalog.
C
  100 CONTINUE
         MODE = 0
C
C        Set defaults for all parameters to 0.D0 after every group.  
C        Then set a few individual ones, mostly the character strings.
C        Don't reset VERSION, which is the first parameter.  
C        Start the resetting with STATION
C
         I1 = KEYPTR( 'STATIONs', KC, KI )
         DO I = I1, KI(2)
            KD(I) = 0.D0
         END DO
C
C        Catalog version.  Set default to previous value.
C
         FREQVER = KCHAR( 'VERSION', 20, .FALSE., KD, KC, KI )
C
C        Station names.
C
         I1 = KEYPTR( 'STATIONs', KC, KI ) - 1
         DO I = 1, MFSTA
            KD(I1+I) = BLANK
         END DO
C
C        Name and note.
C
         I1 = KEYPTR( 'NAME', KC, KI ) - 1
         DO I = 1, 2
            KD(I1+I) = BLANK
         END DO
C
         I1 = KEYPTR( 'NOTE', KC, KI ) - 1
         DO I = 1, 10
            KD(I1+I) = BLANK
         END DO
C
C        Channel character information.
C
         I1 = KEYPTR( 'IFNAME', KC, KI ) - 1
         I2 = KEYPTR( 'POL',    KC, KI ) - 1
         I3 = KEYPTR( 'FE',     KC, KI ) - 1
         I4 = KEYPTR( 'CHNSTA', KC, KI ) - 1
         I5 = KEYPTR( 'ALTIFN', KC, KI ) - 1
         DO I = 1, MFIF
            KD(I1+I) = BLANK
            KD(I2+I) = BLANK
            KD(I3+I) = BLANK
            KD(I4+I) = BLANK
            KD(I5+I) = BLANK
         END DO
C
         KD( KEYPTR( 'DUALX', KC, KI ) ) = UNSET
         KD( KEYPTR( 'FEFILTER', KC, KI ) ) = ZZZZ
         KD( KEYPTR( 'VLABW', KC, KI ) ) = ZZZZ
         KD( KEYPTR( 'VLABAND', KC, KI ) ) = ZZZZ
C
C        Now read the input.
C
         CALL KEYIN( KD(MFRQV+1), KD, KI(2), ENDMARK, MODE, IFRQ, 6 )
C
C        Assume this input was the last if an EOF was found.
C
         IF( MODE .EQ. 1 ) GO TO 999
C
C        Have a new setup group.  Increment NFREQ. 
C        with other routines that use setup information.
C
         NFREQ = NFREQ + 1
         IF( NFREQ .GT. MFREQ ) THEN
            CALL ERRLOG( 'GETFREQ: Too many frequency groups. ' )
         END IF
C
C        Catalog version.
C
         FREQVER = KCHAR( 'VERSION', 20, .FALSE., KD, KC, KI )
C
C        Station names.
C
         I1 = KEYPTR( 'STATIONs', KC, KI ) - 1
         DO I = 1, MFSTA
            WRITE( FSTNAM(I,NFREQ), '(A8)' ) KD(I1+I)
            CALL UPCASE( FSTNAM(I,NFREQ) )
         END DO
C
C        Name and note.
C
         FRNAME(NFREQ) = KCHAR( 'NAME', 12, .FALSE., KD, KC, KI )
         FRNOTE(NFREQ) = KCHAR( 'NOTE', 80, .FALSE., KD, KC, KI )
C
C        Priority.
C
         PRIO(NFREQ) = KD( KEYPTR( 'PRIOrity', KC, KI ) )
C
C        Get IF data.  Determine the number of IF's from 
C        whether they have names.
C
         I1 = KEYPTR( 'IFNAME', KC, KI ) - 1
         I2 = KEYPTR( 'ALTIFN', KC, KI ) - 1
         DO IIF = 1, MFIF
            WRITE( FIFNAM(IIF,NFREQ), '(A2)' ) KD(I1+IIF)
            WRITE( FALTIF(IIF,NFREQ), '(A2)' ) KD(I2+IIF)
            IF( FIFNAM(IIF,NFREQ) .NE. ' ' ) FNIF(NFREQ) = IIF
         END DO
         I1 = KEYPTR( 'FE', KC, KI ) - 1
         I2 = KEYPTR( 'POL', KC, KI ) - 1
         DO IIF = 1, FNIF(NFREQ)
            WRITE( FFE(IIF,NFREQ), '(A4)' ) KD(I1+IIF)
            CALL DWCASE( FFE(IIF,NFREQ) )
            WRITE( FPOL(IIF,NFREQ), '(A3)' ) KD(I2+IIF)
            CALL UPCASE( FPOL(IIF,NFREQ) )
         END DO
         I1 = KEYPTR( 'RF1', KC, KI ) - 1
         I2 = KEYPTR( 'RF2', KC, KI ) - 1
         I3 = KEYPTR( 'LO1', KC, KI ) - 1
         I4 = KEYPTR( 'CH1RF1', KC, KI ) - 1
         I5 = KEYPTR( 'CH1RF2', KC, KI ) - 1
         DO IIF = 1, FNIF(NFREQ)
            FRF1(IIF,NFREQ) = KD(I1+IIF)
            FRF2(IIF,NFREQ) = KD(I2+IIF)
            FLO1(IIF,NFREQ) = KD(I3+IIF)
            FCH1RF1(IIF,NFREQ) = KD(I4+IIF)
            FCH1RF2(IIF,NFREQ) = KD(I5+IIF)
c
c           Temporary sanity check for the RDBE freq_RDBE.dat.
c
c put in front:     double precision tf1, tf2, tl1
c            tf1 = frf1(iif,nfreq)
c            tf2 = frf2(iif,nfreq)
c            tl1 = flo1(iif,nfreq)
c            if( tf1 .gt. tl1 .and. tf2 .gt. tl1 ) then
c               if( abs( tf1 - ( tl1 + 512.d0 ) ) .gt. 0.01 .or.
c     1             abs( tf2 - ( tl1 + 1024.d0 ) ) .gt. 0.01 ) then
c                  write(*,'(A,2I6,3F8.1)') 'getfreq freqs right?',
c     1                  iif, nfreq, tf1, tf2, tl1
c               end if
c            else if( tf1 .lt. tl1 .and. tf2 .lt. tl1 ) then
c               if( abs( tf1 - ( tl1 - 1024.d0 ) ) .gt. 0.01 .or.
c     1             abs( tf2 - ( tl1 - 512.d0 ) ) .gt. 0.01 ) then
c                  write(*,'(A,2I6,3F8.1)') 'getfreq freqs right?',
c     1                  iif, nfreq, tf1, tf2, tl1
c               end if
c            else
c               write(*,'(A,2I6,3F8.1)') 'getfreq goofy?',
c     1         iif, nfreq, tf1, tf2, tl1
c            end if
         END DO
C
C        Various obvious parameters.
C
         FDUALX(NFREQ)  = KD( KEYPTR( 'DUALX', KC, KI ) ) .EQ. 0.D0
         I1 = KEYPTR( 'SYN', KC, KI ) - 1
         FSYN(1,NFREQ)  = KD(I1+1)
         FSYN(2,NFREQ)  = KD(I1+2)
         FSYN(3,NFREQ)  = KD(I1+3)
C
C        50 cm filter.
C
         I1 = KEYPTR( 'LCP50CM', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            FLCP50CM(NFREQ) = 'NARROW'
         ELSE
            WRITE( FLCP50CM(NFREQ), '(A6)' ) KD(I1)
            CALL UPCASE( FLCP50CM(NFREQ) )
         END IF
         I1 = KEYPTR( 'RCP50CM', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            FRCP50CM(NFREQ) = 'NARROW'
         ELSE
            WRITE( FRCP50CM(NFREQ), '(A6)' ) KD(I1)
            CALL UPCASE( FRCP50CM(NFREQ) )
         END IF
C
C        Get the VLA specific parameters.
C        This is now gone.  See SCHED 10.1 or earlier for what
C        was here.
C
C        Go back for next group.
C
         GO TO 100
C
C     Close up for all cases.
C
  999 CONTINUE
C
C     Close frequency file
C
      CLOSE( UNIT=IFRQ )
C
C     Print out some portion of the list if requested.
C
      CALL LISTFREQ
C
C     That's all folks!
C
      RETURN
      END

