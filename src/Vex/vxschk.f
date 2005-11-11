      SUBROUTINE VXSCHK( ISCN, TAPOFF, WARNFS, WARNTS, WARNTSOF, 
     1                  NTSYS, NTSYSON, TSYSGAP, WARNBANK)
      IMPLICIT NONE
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 061200
C     This routine checks several scan related       
C     timing issues for the $CHED section, including
C     tape (and disk) issues. It returns a common TAPOFF
C
C     For all stations in scan:
C        tape offsets are all equal
C     For all VEX scans
C         check scan is 15s
C         long scans should set Warnts, unless last reverse
C         Very long scans with disks should set Warnbank
C
C     For any scan but first
C        check early start doesn't overlap
C     For above and VEX control
C        36s tape reversal
C        40s mode change
C        OBSOLETE: tape change time
C        37m postpass
C
C     Note 29/12/00 Craig suggested to make sure one knows what 
C     last station is (LASTISCN in SCHOPT) I think however, that
C     is not urgent
C
      INTEGER ISCN, LASTSCN
      DOUBLE PRECISION TAPOFF
      LOGICAL WARNFS

      INCLUDE 'sched.inc'      
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'

      INTEGER ISTA
      LOGICAL TPOINI, LASTPS, WARNTS(MAXSTA), WARNBANK
C
C     Tape information from TPDAT.
C
      INTEGER TPPASS, TPDIR, TPINDX, TPHEAD, TPDRIV
      LOGICAL DOTAPE, DOFAST, DOREW
      INTEGER LAPASS, LADIR, LAINDX, LAHEAD, LADRIV
      LOGICAL DDTAPE, DDFAST, DDREW
      INTEGER NXPASS, NXDIR, NXINDX, NXHEAD, NXDRIV
      LOGICAL NXTAPE, NXFAST, NXREW
C
      LOGICAL PASSOK(MAXSTA), WARNTSOF(MAXSTA)
      SAVE PASSOK
      INTEGER LASTTSYS(MAXSTA), LASTTSON(MAXSTA), DATLAT, I 
      INTEGER NTSYSON(MAXSTA), NTSYS(MAXSTA), TSYSGAP(MAXSTA), THISGAP
      INTEGER LASTGAP(MAXSTA)
      SAVE LASTTSYS, LASTTSON, LASTGAP
      DATA LASTTSYS /MAXSTA*0/
      DATA LASTTSON /MAXSTA*0/
      DATA LASTGAP /MAXSTA*0/
C
      IF ( DEBUG .AND. ISCN .LE. 2) 
     .    CALL WLOG( 1,'VXSCHK:  Checking VEX tapes ')
C
C    Don't know offset and isn't set
C
      TAPOFF = 0.
      TPOINI = .FALSE.
C
C     Major loop over all stations in scan
C
      DO ISTA = 1, NSTA
C     
C     Will Need some station dependent values
C
         CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, 
     1       DOFAST, DOREW, TPPASS, TPDRIV, TPDIR, 
     2       TPINDX, TPHEAD )
C
C
C
         IF( STASCN(ISCN,ISTA) ) THEN
C        
C        Current VEX does not allow for different tape starts, so 
C        check and set the overall offset TPOFF
C
            IF( TPOINI ) THEN
C           Remember tpstart is already the offset from startj
               IF( ABS(TPSTART(ISCN,ISTA) - TAPOFF)
     .             .GT. 0.1d0/86400D0 ) THEN
                  CALL PRTSCN( ISCN )
                  WRITE(MSGTXT, '( A, I3, A )' ) 
     .                'Station tape starts differ,'// 
     .                ' set MINPAUSE to ',
     .                INT(TPSTART(ISCN,ISTA)*86400d0
     .                /SPEEDUP(NSETUP(ISCN,ISTA))),
     .                's to produce VEX file!'
                  CALL ERRLOG( MSGTXT )
               END IF
            ELSE
               TAPOFF = TPSTART(ISCN,ISTA) 
               TPOINI = .TRUE.
            END IF
C        
C        Other checks depend on this not being the first scan
C        Close the STASCN loop
C
         END IF
         IF( STASCN(ISCN,ISTA) .AND.
     1       CONTROL(STANUM(ISTA)) .EQ. 'VEX') THEN
C
C        scans should be longer than 15s, according to NRV
C        
            IF( NINT( ( STOPJ(ISCN) - (STARTJ(ISCN)-TAPOFF) ) 
     1          * 86400d0) .LT. 15 ) THEN
               CALL PRTSCN( ISCN )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Scan length < 15s',
     2             'for station ',STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         Currently FS supports '//
     1             'minimal scan length of 15s')
               WARNFS = .TRUE.
            END IF
C 
C           continuous motion longer than 90 min should cause a *MAJOR* 
C           warning because bank switches can only occur during gaps in
C           recording. Ignore the fact that stations are independent
C           and just check for gaps in the schedule.
C
C
C           Find the last scan that this station participated in (will
C           be ISCN if this is the first scan for this station).
            LASTSCN = 0
            DO I = SCAN1, ISCN-1
              IF( STASCN(I, ISTA) ) LASTSCN = I
            END DO
            IF( LASTSCN .EQ. 0 ) LASTSCN = ISCN
C
C           Only need to check for long passes at disk stations
C
            IF( USEDISK(ISTA) ) THEN
C              Default the last gap in recording to be at start of scan 1
               IF( LASTGAP(ISTA) .EQ. 0 ) THEN
                  LASTGAP(ISTA) = SCAN1
               END IF
               IF( NINT( ( (STARTJ(ISCN)-TAPOFF) - STOPJ(LASTSCN) ) 
     1             * 86400d0) .GT. 10 .OR. LASTSCN .EQ. ISCN ) THEN
C                 This means there is a gap before this scan 
                  LASTGAP(ISTA) = ISCN
               END IF
               IF( NINT( ( (STOPJ(ISCN)) - STARTJ(LASTGAP(ISTA)) ) 
     1             * 1440d0) .GT. 90 ) THEN
C                 This means there was no recent gap - issue a warning
                  WARNBANK = .TRUE.
               END IF
            END IF
C
C 
C           scans longer than 15 min should cause a warning
C           because Tsys is only measured at scan start
C           Unfortunately we still like PI to avoid postpasses
C
            IF( NINT( ( STOPJ(ISCN) - (STARTJ(ISCN)-TAPOFF) ) 
     1          * 1440d0) .GT. 15 ) THEN  
               LASTPS = .FALSE.
               IF ( ISCN .NE. SCANL ) THEN
                  CALL TPPACK( 'UNPACK', TPDAT(1,(ISCN+1),ISTA), 
     2                NXTAPE, NXFAST, NXREW, NXPASS, 
     3                NXDRIV, NXDIR, NXINDX, NXHEAD )
                  LASTPS = NXTAPE
               END IF
               IF( .NOT. LASTPS ) THEN 
                  WARNTS(ISTA) = .TRUE.
C                 And record the longest gap for information
                  THISGAP = NINT( ( STOPJ(ISCN) - 
     1                      (STARTJ(ISCN)-TAPOFF) ) * 1440d0)
                  IF( THISGAP .GT. TSYSGAP(ISTA) ) THEN
                     TSYSGAP(ISTA) = THISGAP
                  END IF
               END IF
            END IF
C        
C           In fact, continuous motion also prevents the Tsys from being
C           measured (cal diode only fired during gaps in recording), so try 
C           to prevent greater than 15 mins of continuous motion
C           Assume a gap of more than 10 seconds means that the cal diode
C           will be fired.
C           Ignore the TAPOFF of the LASTTSYS scan (small error, not
C           enough to worry about).
C           Check if the time between the beginning of the last Tsys scan
C           and the end of the current scan is greater than 15
C           minutes. 
C           Allow continuous last passes to prevent having to postpass.
C
C           Default the last on-source Tsys to be scan 1
            IF( LASTTSON(ISTA) .EQ. 0 ) THEN
               LASTTSON(ISTA) = SCAN1
            END IF
C
C           Default Last Tsys scan to first scan
C
            IF( LASTTSYS(ISTA) .EQ. 0 ) THEN
              LASTTSYS(ISTA) = SCAN1
            END IF
C
C           check if there was a Tsys gap this scan
C
            IF( NINT( ( (STARTJ(ISCN)-TAPOFF) - STOPJ(LASTSCN) ) 
     1          * 86400d0) .GT. 10 .OR. LASTSCN .EQ. ISCN ) THEN
C              This means there is a TSYS in this scan 
               LASTTSYS(ISTA) = ISCN
               NTSYS(ISTA) = NTSYS(ISTA) + 1
C
C              We also like to encourage the Tsys to
C              be on-source, so check for that here. This is station
C              dependent (slew times differ). Make sure there is an
C              on-source Tsys every 15 minutes for every antenna.
C
               DATLAT = IDNINT( ( TONSRC(ISCN,ISTA) - 
     .            (STARTJ(ISCN) - TAPOFF) ) 
     1            * 86400D0 )
               IF( DATLAT .LE. 0 )  THEN
                  LASTTSON(ISTA) = ISCN
                  NTSYSON(ISTA) = NTSYSON(ISTA) + 1
               ELSE IF( NINT( ( (STOPJ(ISCN)) - STARTJ(LASTTSON(ISTA)) )
     1            * 1440d0) .GT. 15 ) THEN  
                  WARNTSOF(ISTA) = .TRUE.
               END IF
            END IF
C
C           Check if there is no recent Tsys for this station - issue 
C           a warning if so and its not the last pass.
            IF( NINT( ( (STOPJ(ISCN)) - STARTJ(LASTTSYS(ISTA)) ) 
     1          * 1440d0) .GT. 15 ) THEN
               LASTPS = .FALSE.
               IF ( ISCN .NE. SCANL ) THEN
                  CALL TPPACK( 'UNPACK', TPDAT(1,(ISCN+1),ISTA), 
     1                NXTAPE, NXFAST, NXREW, NXPASS, 
     2                NXDRIV, NXDIR, NXINDX, NXHEAD )
                  LASTPS = NXTAPE
               END IF
               IF( .NOT. LASTPS ) THEN
                  WARNTS(ISTA) = .TRUE.
C                 And record the longest gap for information
                  THISGAP = NINT( ( (STOPJ(ISCN)) - 
     1                   STARTJ(LASTTSYS(ISTA)) ) * 1440d0 ) 
                  IF( THISGAP .GT. TSYSGAP(ISTA) ) THEN
                     TSYSGAP(ISTA) = THISGAP
                  END IF
               END IF
            END IF
C
C           For VEX scans check whether the current pass is
C           OK for not doing a postpass
C
            IF ( TPFOOT1(ISCN,ISTA) .GT. TPLENG(ISTA)*0.90 .AND.
     1          TPDIR .EQ. -1 ) THEN
C              good start
               PASSOK(ISTA) = .TRUE.
            END IF
         END IF
      END DO
C
C        Other checks depend on this not being the first scan
C        Close the STASCN loop and start Majour loop again (note the
C        RETURN half way through this to ensure not first scan.
C
      DO ISTA = 1, NSTA
C     
C     Will Need some station dependent values
C
         CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, 
     1       DOFAST, DOREW, TPPASS, TPDRIV, TPDIR, 
     2       TPINDX, TPHEAD )
C
C
C
         IF( STASCN(ISCN,ISTA) ) THEN
C        
C        Current VEX does not allow for different tape starts, so 
C        check and set the overall offset TPOFF
C
            IF( TPOINI ) THEN
C           Remember tpstart is already the offset from startj
               IF( ABS(TPSTART(ISCN,ISTA) - TAPOFF)
     .             .GT. 0.1d0/86400D0 ) THEN
                  CALL PRTSCN( ISCN )
                  WRITE(MSGTXT, '( A, I3, A )' ) 
     .                'Station tape starts differ,'// 
     .                ' set MINPAUSE to ',
     .                INT(TPSTART(ISCN,ISTA)*86400d0
     .                /SPEEDUP(NSETUP(ISCN,ISTA))),
     .                's to produce VEX file!'
                  CALL ERRLOG( MSGTXT )
               END IF
            ELSE
               TAPOFF = TPSTART(ISCN,ISTA) 
               TPOINI = .TRUE.
            END IF
         END IF

         IF( ISCN .EQ. 1 ) RETURN
         IF( STASCN(ISCN,ISTA) .AND. STASCN(ISCN-1,ISTA) ) THEN 
C        
C        Thus OK to pick up the tape info of last scan
C        
            CALL TPPACK( 'UNPACK', TPDAT(1,(ISCN-1),ISTA), 
     1          DDTAPE, DDFAST, DDREW, LAPASS, 
     2          LADRIV, LADIR, LAINDX, LAHEAD )
C        
C        First check if TAPOFF moves into previous scan
C        
            IF( (STARTJ(ISCN) - TAPOFF) .LT.  STOPJ(ISCN-1) 
     1          .AND.  ISCN .NE. SCAN1 ) THEN
C           
C           Problem if stations overlap
C           
               CALL PRTSCN( ISCN )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Tape early start failed ',
     2             'station ', STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         Early tape starts only '//
     .             'work if there are sufficient gaps.')
               WARNFS = .TRUE.
            END IF
         END IF
C
C
C     End loop on STASCN(ISCN&ISCN-1) 
C     Next Vex specific things
C
         IF( STASCN(ISCN,ISTA) .AND. STASCN(ISCN-1,ISTA) .AND.
     1       CONTROL(STANUM(ISTA)) .EQ. 'VEX' ) THEN
C        
C        Next check for tape reversal < 36s...
C        
            IF( ISCN .NE. SCAN1 .AND.
     3          .NOT. ( DOTAPE .OR. DOFAST .OR. DOREW ) .AND. 
     5          TPDIR .NE. LADIR .AND.
     1          .NOT. ( DDTAPE .OR. DDFAST .OR. DDREW ) .AND.
     2          (STARTJ(ISCN)-TAPOFF) .LT.
     3          (STOPJ(ISCN-1) + 36.1d0/86400d0) ) THEN
C           issue a serious warning
               CALL PRTSCN( ISCN )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Tape reversal < 36s ',
     2             'station ',STATION(STANUM(ISTA))
               WARNFS = .TRUE.
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         A longer gap is required'
     1             //' to stop and reverse the tape.')
            END IF
C
C           Now check for mode change < 40s 
C
            IF ( MODSCN(ISCN) .NE. MODSCN(ISCN-1) .AND.
     1          (STARTJ(ISCN) - TAPOFF) .LT. 
     2          (STOPJ(ISCN-1) + 40.1d0/86400d0)
     3          .AND.  ISCN .NE. SCAN1 .AND.
     4          .NOT. ( DOTAPE .OR. DOFAST .OR. DOREW )) THEN
C              If the stations match it is impossible
               CALL PRTSCN( ISCN )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Mode setup < 40s for ',
     3             'station ', STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         Stations need 40s for '//
     1             'any mode change, incl frequency shift.')
               WARNFS = .TRUE.
            END IF
C     Need possibly time for tape change
C     No longer need to issue warning, disabled
C     Taken care of in tpsch in Sched main.
C     
C            IF( DOTAPE .AND.
C     1          (STARTJ(ISCN)-TAPOFF).LT.
C     .          (STOPJ(ISCN-1) + 420.1d0/86400d0)
C     2          .AND.  ISCN .NE. SCAN1 .AND.
C     3          STASCN(ISCN-1,ISTA) ) THEN
C               CALL PRTSCN( ISCN )
C               WRITE( MSGTXT, '( A, A, A )' ) 
C     1             'VXSCH: WARNING: Need 7m for tape change ',
C     2             'station ',STATION(STANUM(ISTA))
C               CALL WLOG( 1, MSGTXT ) 
C               WARNFS = .TRUE.
C               CALL PUTOUT( MSGTXT )
C            END IF
C        
C        Check postpass, for VEX tapes only, 1 Drive
C        stations, previous scan not continuous and no
C        long gap: just a warning for now.
C        
            IF( DOTAPE .AND.
     2          STNDRIV(STANUM(ISTA)) .EQ. 1 .AND.
     3          TPDRIV .EQ. LADRIV .AND.
     4          DENSITY(ISTA) .EQ. 'H' .AND.
     5          LADIR .EQ. -1 .AND.
     6          .NOT. ( DOFAST .OR. DOREW ) .AND.
     7          .NOT. NOREC(ISCN) .AND.
     8          (STARTJ(ISCN)-TAPOFF-STOPJ(ISCN-1))*1440d0
     9          .LE. 37.0 .AND.
     1          .NOT. PASSOK(ISTA) ) THEN
               
               CALL PRTSCN( ISCN )
               WRITE( MSGTXT, '( A, A )' )
     1             'VXSCHK:  Thin tape still requires to'/
     2             /' be post-passed at ',STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, A )' )
     1             '         Last pass should be continuous, ',
     2             'or allow a 37 minute gap.'
               CALL WLOG( 1, MSGTXT )
            END IF
C
C           For VEX scans check whether the current pass is
C           OK for not doing a postpass
C
            IF ( .NOT. ( TPDIR .EQ. -1 .AND. 
     1          TPFOOT1(ISCN,ISTA) .GT. TPLENG(ISTA)*0.90 ) .AND.
     2          ( STARTJ(ISCN) - TAPOFF - STOPJ(ISCN-1) 
     1          .GT. 1.D0 / 86400.D0 .OR. NOREC(ISCN) .OR. 
     2          TPDIR .EQ. 1 ) ) THEN
               PASSOK(ISTA) = .FALSE.
            END IF
C        
C        End loop on STASCN(ISCN&ISCN-1) VEX
C
         END IF
         
      END DO
      RETURN
      END


