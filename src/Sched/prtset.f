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
      INTEGER           LEN1, KS, KF, IP, ICH, BITRATE, IUNIT, ICHR
      INTEGER           ISTA
      CHARACTER         FMT*80, IFNAME(4)*9
      LOGICAL           ERRS, SPATCH
      DOUBLE PRECISION  VLO(4), VFLO(4), VBP(2,4)
      SAVE              IFNAME
      DATA              IFNAME / 'VLA IF A:', 'VLA IF B:', 
     1                           'VLA IF C:', 'VLA IF D:' /
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
     1       ' Setup file: ', SETNAME(KS)(1:LEN1(SETNAME(KS))) 
C
C     Give matching entry in frequency catalog.
C
      IF( IFREQNUM(KS) .GE. 1 ) THEN
         KF = IFREQNUM(KS)
         WRITE( IUNIT, '( A, A, A, A )' )
     1     '   Matches group ', FRNAME(KF)(1:LEN1(FRNAME(KF))),
     2     ' in ', FREQFILE(1:LEN1(FREQFILE))
         IF( FRNOTE(KF) .NE. ' ' ) THEN
            WRITE( IUNIT, '( 4X, A )' ) FRNOTE(KF)(1:LEN1(FRNOTE(KF)))
         END IF
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
         BITRATE =  NCHAN(KS) * BITS(1,KS) * SAMPRATE(KS)
         WRITE( IUNIT, '( 1X, /, A, I4, 2A, A, I4, /,  ' //
     1       ' 3A, I1, A, F7.3 )' ) 
     2       '   Setup group: ', KS, 
     3       '         Station: ', SETSTA(1,KS),
     4       '          Total bit rate: ', BITRATE, 
     5       '   Format: ', FORMAT(KS),
     6       '          Bits per sample: ', BITS(1,KS),
     7       '         Sample rate:', SAMPRATE(KS)
C
C        Various items, especilly related to tapes.
C
         IF( NOTAPE .OR. .NOT. RECUSED(KS) ) THEN
            WRITE( IUNIT, '( A, I3 )' )
     1           '   Number of channels:', NCHAN(KS)   
         ELSE IF( USETAPE(ISTA) ) THEN
            WRITE( IUNIT, '( A, I3, A, I3, A, F6.2 )' )
     1          '   Number of channels:', NCHAN(KS),   
     2          '    Passes/head pos: ', TAPEMODE(KS),
     3          '       Speedup factor: ', SPEEDUP(KS)
         ELSE
            WRITE( IUNIT, '( A, I3, 24X, A, F6.2 )' )
     1          '   Number of channels:', NCHAN(KS),   
     2          '       Speedup factor: ', SPEEDUP(KS)
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
               IF( USETAPE(ISTA) .AND. DENSITY(ISTA) .EQ. 'H' ) THEN
                  WRITE( IUNIT, '( 1X, /, A, F7.3, A, F6.2, A )' )
     1               '   Tape used at high density at ', 
     2                SPEEDH(KS), ' ips.  '
                  IF( SPEEDH(KS) .NE. 0.0 ) THEN
                     WRITE( IUNIT, '( A, F7.2, A )' )
     1                   '   Time per pass is ', 
     2                   TPLENG(ISTA) * 12.0 / ( SPEEDH(KS) * 60.0 ),
     3                   ' minutes.'
                  END IF
               ELSE IF( USETAPE(ISTA) .AND. DENSITY(ISTA) .EQ. 'L' ) 
     1             THEN
C
C                 I don't think low density tape is still in the 
C                 system, but I'll leave this just in case.
C
                  WRITE( IUNIT, '( 1X, /, A, F7.3, A, F6.2, A )' )
     1               '   Tape used at low density at ', 
     2                SPEEDL(KS), ' ips.  '
                  IF( SPEEDL(KS) .NE. 0.0 ) THEN
                     WRITE( IUNIT, '( A, F7.2, A )' )
     1                   '   Time per pass is ', 
     2                   TPLENG(ISTA) * 12.0 / ( SPEEDL(KS) * 60.0 ),
     3                   ' minutes.'
                  END IF
C
               ELSE IF( USEDISK(ISTA) ) THEN
                  WRITE( IUNIT, '( 1X, /, A, F7.3, A, F6.2, A )' )
     1               '   Disk used to record data.'
               END IF
C	     
C              Add warning about two tapes.
C	     
               IF( USETAPE(ISTA) .AND. TWOHEAD .AND. 
     1             NHEADS(ISETSTA(KS)) * STNDRIV(ISETSTA(KS)) .GE. 2 ) 
     2             THEN
C
                  IF( STNDRIV(ISETSTA(KS)) .GE. 2 .AND.
     1                FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
                     MSGTXT = '   Two tape drives will be used.'
                  ELSE IF( NHEADS(ISETSTA(KS)) .GE. 2 .AND.
     1                FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
                     MSGTXT = '   Two heads will be used.'
                  END IF
                  ICHR = LEN1( MSGTXT ) + 1
                  IF( NCHAN(KS)*BITS(1,KS)*SAMPRATE(KS) .GT. 256.0 )
     1                THEN
                     MSGTXT(ICHR:) = '  Required by this setup.'
                  ELSE
                     MSGTXT(ICHR:) = 
     1                 '  Required by some other setup.'
                  END IF
                  ICHR = LEN1( MSGTXT )
                  WRITE( IUNIT, '( A )' ) MSGTXT(1:ICHR)
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
C     Write some VLA specific stuff if the station is the VLA.
C
      IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' ) THEN
         IF( VLALOFI(KS) ) THEN
            WRITE( IUNIT, '( 1X, /, A )' ) '   VLA specific '//
     1         'information: (LO and FI cards will be written.)'
         ELSE
            WRITE( IUNIT, '( 1X, /, A )' ) '   VLA specific '//
     1        'information: (LO and FI cards not needed.  '//
     2        'Defaults shown.)'
         END IF
         WRITE( IUNIT, '( A, A, T26, A, A, T49, 2A )' )
     1      '     VLABAND:       ', VLABAND(KS), 'VLABW:       ', 
     2      VLABW(KS), 'FEFILTER: ', FEFILTER(KS)
         WRITE( IUNIT, '( A, F8.2, T26, A, F8.2, T49, 2A )' )
     1      '     VLAFEAB: ', VLAFEAB(KS), 'VLAFECD: ', VLAFECD(KS),
     2      'VLAIF:  ', VLAIF(KS)
         WRITE( IUNIT, '( A, F8.2, T26, A, F8.2, T49, 2A )' )
     1      '     VLASYNA: ', VLASYNA(KS), 'VLASYNB: ', VLASYNB(KS),
     2      'VLAROT: ', VLAROT(KS)
         WRITE( IUNIT, '( A, F8.2, T26, A, F8.2, T49, A, I4 )' )
     1      '     FLUKEA:  ', FLUKEA(KS), 'FLUKEB:  ', FLUKEB(KS),
     2      'FLUKESET: ', FLUKESET(KS)
C
C        Get details of VLA bands.
C
         CALL VLAFREQ( KS, VLO, VFLO, VBP, ERRS )
C
         WRITE( IUNIT, '( A )' ) 
     1           '     Details for VLA IFs: LO Sum ' //
     1           '      Frequency span    VLBI FIRSTLO '
         DO ICH = 1, 4
            WRITE( IUNIT, '( 11X, A, 2X, F10.4, F12.4, A, F10.4,  ' //
     1         'F12.4 )' ) IFNAME(ICH), VLO(ICH), VBP(1,ICH), 
     2         '-', VBP(2,ICH), VFLO(ICH)
         END DO
         WRITE( IUNIT, '( A )' )
     1       '     With new digital patch panel, VLA IF = VLB IF.'
C
C        Try to detect any special patching requirements.
C
         SPATCH = .FALSE.
         DO ICH = 1, NCHAN(KS)
            IF( IFCHAN(ICH,KS) .EQ. 'A' .AND. 
     1          POL(ICH,KS) .EQ. 'LCP' ) SPATCH = .TRUE.
            IF( IFCHAN(ICH,KS) .EQ. 'B' .AND. 
     1          POL(ICH,KS) .EQ. 'LCP' )  SPATCH = .TRUE.
            IF( IFCHAN(ICH,KS) .EQ. 'C' .AND. 
     1          POL(ICH,KS) .EQ. 'RCP' )  SPATCH = .TRUE.
            IF( IFCHAN(ICH,KS) .EQ. 'D' .AND. 
     1          POL(ICH,KS) .EQ. 'RCP' )  SPATCH = .TRUE.
         END DO
         IF( SPATCH) THEN
             WRITE( IUNIT, '( A )' )
     1       '  --- WARNING ---  Special patch required. '//
     2       'Contact VLA.'
         END IF
      END IF
C
      IF( DEBUG ) CALL WLOG( 0, 'PRTSET: Done with setup.' )
C
      RETURN
      END

