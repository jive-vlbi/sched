      SUBROUTINE TPTPNS
C
C     Subroutine for SCHED called by SCHIN that transfers the tape
C     initialization information to station dependent cells in the
C     arrays in sched.inc.  The information was temporarily stored
C     in the RTP arrays in tpinit.inc by TPINI.  This two step
C     process was used because the station list was not necessarily
C     known when the tape initialization information was read.
C
C     Also resolve which NDRIVES input to use.
C
      INCLUDE 'sched.inc'
      INCLUDE 'tpinit.inc'
C
      INTEGER       ITPSTA, IDEF, VLADEF, VLBADEF, ISTA, LEN1
      INTEGER       IDK
      LOGICAL       ISLBA
C --------------------------------------------------------------
C     A station called 'DEFAULT' may be included with default settings.
C
      IDEF = -1
      VLADEF = -1
      VLBADEF = -1
      DO ITPSTA = 1, RNTPSTA
         IF( RTPSTA(ITPSTA) .EQ. 'DEFAULT' ) IDEF = ITPSTA
         IF( RTPSTA(ITPSTA) .EQ. 'VLA' .AND. 
     1       LEN1( RTPSTA(ITPSTA) ) .EQ. 3 ) VLADEF = ITPSTA
         IF( RTPSTA(ITPSTA) .EQ. 'VLBA' .AND. 
     1       LEN1( RTPSTA(ITPSTA) ) .EQ. 4 ) VLBADEF = ITPSTA
      END DO
C
C     Now get RTPNS for each station.  This is the index from the
C     station numbers to the RTP array elements.
C
      DO ISTA = 1, NSTA
         RTPNS(ISTA) = -1
         DO ITPSTA = 1, RNTPSTA
            IF( STATION(STANUM(ISTA)) .EQ. RTPSTA(ITPSTA) ) 
     1          RTPNS(ISTA) = ITPSTA
         END DO
C
C        Take a generic vlba or vla default if appropriate.
C
         IF( RTPNS(ISTA) .EQ. -1 ) THEN
            IF( STATION(STANUM(ISTA))(1:3) .EQ. 'VLA' ) 
     1          RTPNS(ISTA) = VLADEF
            IF( STATION(STANUM(ISTA))(1:4) .EQ. 'VLBA' ) 
     1          RTPNS(ISTA) = VLBADEF
         END IF
C
C        Take the full default if still have nothing.
C
         IF( RTPNS(ISTA) .EQ. -1 ) THEN
            RTPNS(ISTA) = IDEF
         END IF
C
C        Die if no defaults apply.
C
         IF( RTPNS(ISTA) .EQ. -1 ) 
     1      CALL ERRLOG( 'TPTPNS: No tape initialization information '//
     2          'for station: '//STATION(STANUM(ISTA)) )
      END DO
C
C     The number of drives at a station can be given in either the
C     station catalog (STNDRIV) or the tapeini stuff (NDRIVE).  Later
C     in SCHED, only STNDRIV will be used since it can be different
C     for each station.  Give the TAPEINI input
C     priority if given, but take the station catalog value if not.
C     However don't let the TPINI value exceed the catalog value - ie
C     assume that the catalog gives the maximum number of drives.
C     The defaults are 0 in the TAPEINI and 1 in the catalog.
C
      DO ISTA = 1, NSTA
         IF( RNDRIVE(RTPNS(ISTA)) .GT. STNDRIV(STANUM(ISTA)) ) THEN
            CALL ERRLOG( 'TPTPNS: Tape initialization input '//
     1          'specifies more drives than exist at '//
     2          STATION(STANUM(ISTA)) )
         ELSE IF( RNDRIVE(RTPNS(ISTA)) .NE. 0 ) THEN
            STNDRIV(STANUM(ISTA)) = RNDRIVE(RTPNS(ISTA))
C
C           Check if the starting drive is too high.
C
            IF( RTPSDRIV(RTPNS(ISTA)) .GT. STNDRIV(STANUM(ISTA)) ) THEN
               CALL ERRLOG( 'TPTPNS: Starting drive number greater than'
     1            // ' number of drives at '//STATION(STANUM(ISTA)) )
            END IF
         END IF
      END DO
C
C     Now fill the arrays stored in sched.inc.
C     Some of these should be eliminated eventually.
C
      NTPSTA = NSTA
      TPOBS  = RTPOBS
      DO ISTA = 1, NSTA
         TPTIME(ISTA)   = RTPTIME(RTPNS(ISTA))
         TPSDRIV(ISTA)  = RTPSDRIV(RTPNS(ISTA))
         TPSINDX(ISTA)  = RTPSINDX(RTPNS(ISTA))
         TPLENG(ISTA)   = RTPLENG(RTPNS(ISTA))
         NDRIVE(ISTA)   = RNDRIVE(RTPNS(ISTA))
         NHDPOS(ISTA)   = RNHDPOS(RTPNS(ISTA))
         DENSITY(ISTA)  = RDENSITY(RTPNS(ISTA))         
         HEADMODE(ISTA) = RHEADMOD(RTPNS(ISTA))
         MEDIA(ISTA)    = RMEDIA(RTPNS(ISTA))
         CALL UPCASE( MEDIA(ISTA) )
         IDK = INDEX( MEDIA(ISTA), 'DISC' )
         IF( IDK .NE. 0 ) THEN
            MEDIA(ISTA)(IDK:IDK+3) = 'DISK'
         END IF
      END DO
C
C     Deal with MEDIA defaults etc.
C
      ANYTAPE = .FALSE.
      ALLDISK = .TRUE.
C
      DO ISTA = 1, NSTA
C
C        Set USETAPE and USEDISK based on RMEDIA and RECORDER.
C
         IF( MEDIA(ISTA) .NE. ' ' .AND.
     1       MEDIA(ISTA) .NE. 'TAPE' .AND.
     2       MEDIA(ISTA) .NE. 'DISK' ) THEN
            WRITE( MSGTXT, '( 4A )' ) 'TPTPNS: Invalid MEDIA (',
     1           MEDIA(ISTA), ' for ', STATION(STANUM(ISTA)), ')'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( '         Be sure the requested medium '//
     1          'matches an option in the station file.' )
         END IF
C
C        Determine whether to use tape.  This is the default if
C        tape is available in the station catalog and MEDIADEF was
C        not set to DISK
C
         USETAPE(ISTA) = VLBITP .AND. 
     1        RECORDER(STANUM(ISTA)) .NE. 'NONE' .AND.
     2        ( INDEX( MEDIA(ISTA), 'TAPE' ) .NE. 0 .OR.
     3          DISK(STANUM(ISTA)) .EQ. 'NONE' .OR.
     4          (  MEDIA(ISTA) .EQ. ' ' .AND. 
     5           MEDIADEF(STANUM(ISTA)) .EQ. 'TAPE' ) )
         IF( USETAPE(ISTA) ) ANYTAPE = .TRUE.
C
C        Determine whether to use disk.
C
         USEDISK(ISTA) = VLBITP .AND. 
     1        DISK(STANUM(ISTA)) .NE. 'NONE' .AND.
     2        ( INDEX( MEDIA(ISTA), 'DISK' ) .NE. 0 .OR.
     3          RECORDER(STANUM(ISTA)) .EQ. 'NONE' .OR.
     4          (  MEDIA(ISTA) .EQ. ' ' .AND. 
     5          ( MEDIADEF(STANUM(ISTA)) .EQ. 'DISK' .OR. 
     6            MEDIADEF(STANUM(ISTA)) .EQ. 'NONE' ) ) )
C        LBA recorders use disks but have different constraints to Mk5
         ISLBA = .FALSE.
         IF (DISK(STANUM(ISTA))(1:5) .EQ. 'LBADR') ISLBA = .TRUE.
         ALLDISK = ALLDISK .AND. USEDISK(ISTA) .AND. .NOT. ISLBA
C       write(*,*) 'tptpns ', ista, usetape(ista), usedisk(ista),
C     1  vlbitp, recorder(stanum(ista)), media(ista), 
C     2  disk(stanum(ista)), mediadef(stanum(ista)), staname(ista)
C
C        Don't allow a station to have both disk and tape.
C        Walter Brisken does not want to support this in the VLBA
C        on-line system and VEX does not support it.
C
         IF( USETAPE(ISTA) .AND. USEDISK(ISTA) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( 3A )' )
     1         'TPTPNS: ',STANAME(ISTA), 
     2         ' has been specified to use both tape and disk.'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '        This is not allowed.  ' )
            CALL ERRLOG( '        Choose Tape OR Disk' )
         END IF
C
C        If VLBITP, insist that we use one for recording 
C        observations.
C
         IF( VLBITP .AND. ( 
     1       .NOT. USETAPE(ISTA) .AND.
     2       .NOT. USEDISK(ISTA) ) ) THEN
            WRITE( MSGTXT, '( 2A )' ) 
     1         'TPTPNS:  **** You have specified OBSTYPE = VLBI ',
     2         'or equivalent.'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 4A )' )
     1         '              But your combination of MEDIA in the ', 
     2         'tape initialization input (', MEDIA(ISTA), ')'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 3A )' )
     1         '              and the available drives at ', 
     2         STATION(STANUM(ISTA)), ' are not compatible.'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'TPTPNS:  You probably need to fix MEDIA' )
C
         END IF
C
      END DO
C
      RETURN
      END
