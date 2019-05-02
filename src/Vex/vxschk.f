      SUBROUTINE VXSCHK( ISCN, TAPOFF, WARNFS, WARNTS, WARNTSOF, 
     1                  NTSYS, NTSYSON, TSYSGAP, WARNBANK)
Cf2py intent(in) ISCN
Cf2py intent(out) TAPOFF
Cf2py intent(in, out) WARNFS, WARNTS, WARNTSOF, NTSYS, NTSYSON, TSYSGAP
Cf2py intent(in, out) WARNBANK
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
C        40s mode change   (THIS MIGHT BE TOO HIGH - RCW Sep 1, 2014).
C        OBSOLETE: tape change time
C        37m postpass
C
C     Note 29/12/00 Craig suggested to make sure one knows what 
C     last station is (LASTISCN in SCHOPT) I think however, that
C     is not urgent
C
C     Moved the PARAMETER statement in front of the DATA statements
C     as required by the standard and a user's compiler.  Feb. 4, 2010 RCW.
C
C     Removing tape items, including all TPPACK calls.  July 20, 2010. RCW.
C     Copies of the routine before that in svn revision 294 and earlier.
C     Also in sched_ARCHIVE_nonSVN/obsolete_routines
C
      INTEGER ISCN, LASTSCN
      DOUBLE PRECISION TAPOFF, LASTSTOP
      LOGICAL WARNFS

      INCLUDE 'sched.inc'      
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'

      INTEGER ISTA
      LOGICAL TPOINI, WARNTS(MAXSTA), WARNBANK
C
      LOGICAL WARNTSOF(MAXSTA), FIELDSY(MAXSTA)
      INTEGER LASTTSYS(MAXSTA), LASTTSON(MAXSTA), DATLAT, I 
      INTEGER NTSYSON(MAXSTA), NTSYS(MAXSTA), TSYSGAP(MAXSTA), THISGAP
      REAL LASTBYTE(MAXSTA)
      REAL MAXDISKU
      PARAMETER (MAXDISKU = 22.*60. * 2.016 * (1024./1000.) / 8.)
      SAVE LASTTSYS, LASTTSON, LASTBYTE
      DATA LASTTSYS /MAXSTA*0/
      DATA LASTTSON /MAXSTA*0/
      DATA LASTBYTE /MAXSTA*0.0/
C-----------------------------------------------------------------------
C
C     MAXDISKU is 22 mins of 2 Gbps recording (allow factor 1.008 for
C     headers).
C
      IF ( DEBUG .AND. ISCN .LE. 2) 
     .    CALL WLOG( 1,'VXSCHK:  Checking VEX tapes ')
C
C     Don't know offset and isn't set
C
      TAPOFF = 0.
      TPOINI = .FALSE.
C
C     Major loop over all stations in scan
C
      DO ISTA = 1, NSTA
C
C        Check if this is likely to be a field system station.  Maybe
C        we need to make this more explicit in the station catalog 
C        because CONTROL=VEX no longer necessarily implies field system.
C        Field system stations require some checks not needed elsewhere.
C        Added May 30, 2012 RCW
C
         FIELDSY(ISTA) = CONTROL(STANUM(ISTA)) .EQ. 'VEX' .AND.
     1      STATION(STANUM(ISTA))(1:3) .NE. 'VLA'
C
C        Process the stations actually in the scan.
C
         IF( STASCN(ISCN,ISTA) ) THEN
C        
C           Current VEX does not allow for different tape starts, so 
C           check and set the overall offset TPOFF.  Note that this
C           should have already been enforced earlier in SCHED when
C           the scans were being established (RCW).
C
            IF( TPOINI ) THEN
C           Remember tpstart is already the offset from startj
               IF( ABS(TPSTART(ISCN,ISTA) - TAPOFF)
     1             .GT. 0.1d0/86400D0 ) THEN
                  CALL PRTSCN( ISCN, 'VXSCHK' )
                  WRITE( MSGTXT, '( A, I3, A )' ) 
     1                'Station tape starts differ,'// 
     2                ' set MINPAUSE to ',
     3                INT(TPSTART(ISCN,ISTA)*86400d0
     4                /SPEEDUP(NSETUP(ISCN,ISTA))),
     5                's to produce VEX file!'
                  CALL ERRLOG( MSGTXT )
               END IF
            ELSE
               TAPOFF = TPSTART(ISCN,ISTA) 
               TPOINI = .TRUE.
            END IF
C        
C        Other checks depend on this not being the first scan
C        Close the STASCN loop.  These checks also apply only
C        to the field system.
C
         END IF
         IF( STASCN(ISCN,ISTA) .AND. FIELDSY(ISTA)
     1      .AND. DISK(STANUM(ISTA)) .NE. 'LBADR' ) THEN
C
C           Field system scans should be longer than 15s, 
C           according to NRV
C        
            IF( NINT( ( STOPJ(ISCN) - (STARTJ(ISCN)-TAPOFF) ) 
     1          * 86400d0) .LT. 15 ) THEN
               CALL PRTSCN( ISCN, 'VXSCHK' )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Scan length < 15s',
     2             'for station ',STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         Currently FS supports '//
     1             'minimal scan length of 15s')
               WARNFS = .TRUE.
            END IF
C 
C           continuous motion for long periods should cause a *MAJOR*
C           warning because bank switches can only occur during gaps in
C           recording.  The definition of 'long' depends on the datarate
C           (we worry about disk consumption, not time).
C
C           Find the last scan that this station participated in (will
C           be zero if this is the first scan for this station).
            LASTSCN = 0
            DO I = SCAN1, ISCN-1
              IF( STASCN(I, ISTA) ) LASTSCN = I
            END DO
C
C           Only need to check for long passes at disk stations.
C           Elsewhere in SCHED, there are warnings about long 
C           recording scans, so don't try to duplicate that here.
C           This warning is for field system stations so restrict
C           it to CONTROL=VEX.
C
            IF( USEDISK(ISTA) .AND. FIELDSY(ISTA) ) THEN
               IF (LASTSCN .EQ. 0) THEN
                  LASTBYTE(ISTA) = 0
               ELSE IF( NINT( ( (STARTJ(ISCN)-TAPOFF) - 
     1               STOPJ(LASTSCN) ) * 86400d0) .GT. 10)  THEN
C                 This means there is a gap before this scan 
                  LASTBYTE(ISTA) = GBYTES(LASTSCN, ISTA)
               END IF
               IF( (GBYTES(ISCN,ISTA) - LASTBYTE(ISTA))  
     1               .GT. MAXDISKU ) THEN
C                 This means there was no recent gap - issue a warning
                  WARNBANK = .TRUE.
C     1        gbytes(iscn,ista), lastbyte(ista), maxdisku
               END IF
            END IF
C
C 
C           Scans longer than 15 min should cause a warning
C           because Tsys is only measured at scan start
C           Restrict this to the field system stations
C           as others tend to have continuous Tsys.
C
            IF( NINT( ( STOPJ(ISCN) - (STARTJ(ISCN)-TAPOFF) ) 
     1          * 1440d0) .GT. 15 .AND. FIELDSY(ISTA) ) THEN
               WARNTS(ISTA) = .TRUE.
C              And record the longest gap for information
               THISGAP = NINT( ( STOPJ(ISCN) - 
     1                   (STARTJ(ISCN)-TAPOFF) ) * 1440d0)
               IF( THISGAP .GT. TSYSGAP(ISTA) ) THEN
                  TSYSGAP(ISTA) = THISGAP
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
C           check if there was a Tsys gap this scan.  Avoid a zero
C           index by setting the last stop time to a day earlier for
C           the first scan.
C
            IF( LASTSCN .EQ. 0 ) THEN
               LASTSTOP = (STARTJ(ISCN)-TAPOFF) - 1.D0
            ELSE
               LASTSTOP = STOPJ(LASTSCN)
            END IF
            IF( NINT( ( (STARTJ(ISCN)-TAPOFF) - LASTSTOP ) 
     1          * 86400d0) .GT. 10 ) THEN
C
C              This means there is a TSYS in this scan 
C
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
C           a warning if so.
C
            IF( NINT( ( (STOPJ(ISCN)) - STARTJ(LASTTSYS(ISTA)) ) 
     1          * 1440d0) .GT. 15 ) THEN
               WARNTS(ISTA) = .TRUE.
C              And record the longest gap for information
               THISGAP = NINT( ( (STOPJ(ISCN)) - 
     1                STARTJ(LASTTSYS(ISTA)) ) * 1440d0 ) 
               IF( THISGAP .GT. TSYSGAP(ISTA) ) THEN
                  TSYSGAP(ISTA) = THISGAP
               END IF
            END IF
C
         END IF
      END DO
C
C     Other checks depend on this not being the first scan
C     Close the STASCN loop and start Majour loop again (note the
C     RETURN half way through this to ensure not first scan.
C
      DO ISTA = 1, NSTA
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
                  CALL PRTSCN( ISCN, 'VXSCHK' )
                  WRITE( MSGTXT, '( A, I3, A )' ) 
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
C        First check if TAPOFF moves into previous scan
C        
            IF( (STARTJ(ISCN) - TAPOFF) .LT.  STOPJ(ISCN-1) 
     1          .AND.  ISCN .NE. SCAN1 ) THEN
C           
C           Problem if stations overlap
C           
               CALL PRTSCN( ISCN, 'VXSCHK' )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Tape early start failed ',
     2             'station ', STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         Early tape starts only '//
     1             'work if there are sufficient gaps.')
               WARNFS = .TRUE.
            END IF
         END IF
C
C
C        End loop on STASCN(ISCN&ISCN-1) 
C        Next Vex specific things
C
         IF( STASCN(ISCN,ISTA) .AND. STASCN(ISCN-1,ISTA) .AND.
     1       FIELDSY(ISTA) ) THEN
C        
C           Removed tape reversal check.
C        
C           Check for mode change < 40s 
C           RCW  Sep 1, 2014.  This number is probably too high but
C           I'm not sure what to change it do.  So just alter the
C           warning for now.
C
            IF ( MODSCN(ISCN) .NE. MODSCN(ISCN-1) .AND.
     1          (STARTJ(ISCN) - TAPOFF) .LT. 
     2          (STOPJ(ISCN-1) + 40.1d0/86400d0)
     3          .AND.  ISCN .NE. SCAN1 ) THEN
C
C              If the stations match it is impossible
C
               CALL PRTSCN( ISCN, 'VXSCHK' )
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXSCHK:  WARNING: Mode setup <= 40s for ',
     3             'station ', STATION(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1,'         FS Stations once needed 40s '//
     1             'for any mode change, incl frequency shift.')
               CALL WLOG( 1,'         This may no longer be true '//
     1             'but the new value is not yet clear.')
               WARNFS = .TRUE.
            END IF
C        
C           Removed postpass warning and calculations.
C
C        End loop on STASCN(ISCN&ISCN-1) VEX
C
         END IF
         
      END DO

      RETURN
      END
