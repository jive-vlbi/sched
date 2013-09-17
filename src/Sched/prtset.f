      SUBROUTINE PRTSET( KS, IUNIT )
C
C     Routine for SCHED that prints a summary of the information in
C     a setup group.  The calling routine should determine which 
C     groups to print.  Called by SCHSUM, ERRSET, CHKSC1, 
C     and STAFILES.  The output unit will vary by calling routine.
C
C     KS is the setup group to print.  IUNIT is the output unit for 
C     the print.  It is the sum file for SCHSUM, the 
C
C     Since this will be the first use of the .sum file, open it here.
C
C     This routine is called by ERRSET, so be sure that neither it,
C     nor anything it calls, also calls ERRSET!  Guess how I found
C     that possibility.

      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'schfreq.inc'
C
      INTEGER           LEN1, KS, KF, IP, ICH, JCH 
      INTEGER           IUNIT, ISTA
      CHARACTER         FMT*80
      LOGICAL           GOTFRQ, NEWFRQ, SIDEINV
C ---------------------------------------------------------------------
      ISTA = ISCHSTA(ISETSTA(KS))
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, I5 )' ) 
     1      'PRTSET: Starting - setup, unit: ', KS, IUNIT
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Write the setup file name.
C
      WRITE( IUNIT, '( 1X, /, 1X, /, 2A )' )
     1       ' ======== Setup file: ', SETNAME(KS)(1:LEN1(SETNAME(KS))) 
C
C     Give matching entry in frequency catalog.  Can be more than
C     one entry since differents channels can use different ones
C     in some special cases.
C
      GOTFRQ = .FALSE.
      DO ICH = 1, NCHAN(KS)
         GOTFRQ = GOTFRQ .OR. IFREQNUM(ICH,KS) .GE. 1
      END DO
      IF( GOTFRQ ) THEN
         WRITE( IUNIT, '( A, A, A )' )
     1     '   Matching groups in ',  FREQFILE(1:LEN1(FREQFILE)), ':'
         DO ICH = 1, NCHAN(KS)
            KF = IFREQNUM(ICH,KS)
            NEWFRQ = .TRUE.
            IF( ICH .GE. 2 ) THEN
               DO JCH = 1, ICH - 1
                  IF( KF .EQ. IFREQNUM(JCH,KS) ) NEWFRQ = .FALSE.
               END DO
            END IF
            MSGTXT = ' '
            IF( NEWFRQ ) THEN
               WRITE( MSGTXT(1:18), '( 5X, A )' )
     1            FRNAME(KF)(1:LEN1(FRNAME(KF)))
               IF( FRNOTE(KF) .NE. ' ' ) THEN
                  WRITE( MSGTXT(22:256), '( A )' ) 
     1                FRNOTE(KF)(1:LEN1(FRNOTE(KF)))
               END IF
               WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            END IF
         END DO
C
C        Warn if incomplete overlap.
C
         IF( BESTOVER(KS) .LT. TOTBW(KS) ) THEN
            SETMSG = ' '
            WRITE( IUNIT, '( A, F7.2, A, F7.2, A )' )
     1         '    ****** Only ', BESTOVER(KS), ' of ', TOTBW(KS), 
     2         ' MHz total bandwidth is within the IFs'
            WRITE( IUNIT, '( A, A )' ) 
     1         '           in the setup file before any FREQ or ',
     2         'DOPPLER shifts.' 
         END IF
C
C     Warn if there was no matching frequency catalog entry.
C
      ELSE
         WRITE( IUNIT, '( A )' )
     1     ' --- WARNING ---  This group does not match an entry'
     2     // ' in the frequency catalog.'
         WRITE( IUNIT, '( A )' )
     1     '                  This might be ok because the catalog'
     2     // ' is not complete.'
         WRITE( IUNIT, '( A )' )
     1     '                  But be very careful to be sure that'
     2     // ' the setup is correct.'
      END IF
C
C     Write the rest only if it was used in the experiment.
C
      IF( .NOT. VLAONLY ) THEN
C
C        Group number and station and some other items.
C
         WRITE( IUNIT, '( 1X, /, A, I4, 2A, A, I5, /,  ' //
     1       ' 3A, I1, A, F7.3 )' ) 
     2       '   Setup group: ', KS, 
     3       '         Station: ', SETSTA(1,KS),
     4       '          Total bit rate: ', NINT( TOTBPS(KS) ),
     5       '   Format: ', FORMAT(KS),
     6       '          Bits per sample: ', BITS(1,KS),
     7       '         Sample rate:', SAMPRATE(KS)
C
C        Various items, especilly related to tapes.
C
         IF( NOTAPE .OR. .NOT. RECUSED(KS) ) THEN
            WRITE( IUNIT, '( A, I3, A, A )' )
     1           '   Number of channels:', NCHAN(KS),
     2           '    DBE type: ', DBE(KS)
         ELSE
            WRITE( IUNIT, '( A, I3, A, A, T54, A, F6.2 )' )
     1           '   Number of channels:', NCHAN(KS),   
     2           '    DBE type: ', DBE(KS),
     3           '   Speedup factor: ', SPEEDUP(KS)
         END IF
C
         IF( .NOT. NOTAPE ) THEN
C
C           For tape, give the tape speeds and time per pass.
C           For disk, just note that disk is being used.
C           With one station per setup, this is now much simpler.
C           and old subroutine PRTTPP has been relegated to the
C           archive.
C
            IF( FORMAT(KS) .NE. 'NONE' .AND. ISTA .NE. 0 ) THEN
               IF( USEDISK(ISTA) ) THEN
                  WRITE( IUNIT, '( 1X, /, A, F7.3, A, F6.2, A )' )
     1               '   Disk used to record data.'
               END IF
            END IF
C
C           Say something about setups used for other than recording.
C
            IF( .NOT. RECUSED(KS) ) THEN
               WRITE( IUNIT, '( 1X, /, A )' )
     1           '   Setup not used for recording data.'
            END IF
C
         END IF
C
C        Warning about sideband inversion.
C
         SIDEINV = .FALSE.
         DO ICH = 1, NCHAN(KS)
            IF( CORINV(ICH,KS) .NE. 0.D0 ) SIDEINV = .TRUE.
         END DO
         IF( SIDEINV ) THEN
            WRITE( IUNIT, '( 1X, /, A, A )' )
     1         '   Frequencies shifted and sidebands ',
     2         'inverted because RDBE_PFB can only'
            WRITE( IUNIT, '( A, A )' )
     1         '   do LSB.  Use a correlator, ',
     2         'such as DiFX, that can invert sidebands.'
         END IF
C
C        Frequency info.
C
         CALL PRTFREQ( KS, IUNIT )
C
C        Track info.
C
         IF( .NOT. NOTAPE .AND. RECUSED(KS) ) THEN
            WRITE( IUNIT, '( 1X, /, A )' ) '   Track assignments are: '
            FMT = ' '
            IF( NCHAN(KS) .EQ. 1 ) THEN
               FMT =   '( A, I1, A, I3 )'
            ELSE
               WRITE( FMT, '( A, I3, A )' )  '( A, I1, A, I3, ',
     1              NCHAN(KS) - 1, '('','', I3 ) )' 
            END IF
            DO IP = 1, TAPEMODE(KS)
               WRITE( IUNIT, FMT ) '    track', IP, '= ', 
     1              ( TRACK(ICH,IP,KS), ICH = 1, NCHAN(KS) )
            END DO
            WRITE( IUNIT, '( A, A )' ) '    barrel=', BARREL(KS)
         END IF
C
      END IF
C
      IF( DEBUG ) CALL WLOG( 0, 'PRTSET: Done with setup.' )
C
      RETURN
      END
