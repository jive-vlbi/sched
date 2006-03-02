      SUBROUTINE VXSCHK( ISCN, TAPOFF, WARNFS, WARNTS )
      IMPLICIT NONE
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 061200
C     This routine checks several scan related       
C     timing issues for the $CHED section, including
C     tape issues. It returns a common TAPOFF
C
C     For all stations in scan:
C        tape offsets are all equal
C     For all VEX scans
C         check scan is 15s
C         set Warnts, unless last reverse
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
      INTEGER ISCN
      DOUBLE PRECISION TAPOFF
      LOGICAL WARNFS, WARNTS

      INCLUDE 'sched.inc'      
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'

      INTEGER ISTA
      LOGICAL TPOINI, LASTPS
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
      LOGICAL PASSOK(MAXSTA)
      SAVE PASSOK
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
C        scans longer than 15 min should cause a warning
C        because Tsys is only measured at scan start
C        Unfortunately we still like PI to avoid postpasses
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
               IF( .NOT. LASTPS ) WARNTS = .TRUE.
            END IF
C
C        For VEX scans check whether the current pass is
C        OK for not doing a postpass
C
            IF ( TPFOOT1(ISCN,ISTA) .GT. TPLENG(ISTA)*0.90 .AND.
     1          TPDIR .EQ. -1 ) THEN
C           good start
               PASSOK(ISTA) = .TRUE.
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


