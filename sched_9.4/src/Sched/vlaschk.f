      SUBROUTINE VLASCHK
C
C     Check some VLA parameters that can vary with scans.
C
C     To keep the VLA checking together, do some scan based checks
C     here.  Only do the first time this routine is called.
C     Only do for scans that include the VLA.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     KS, ICH, ISTA, ISCN, VSTA, PKS, PSCN
      INTEGER     YEAR, DAY1, IH
      DOUBLE PRECISION  START
      CHARACTER   LMODE*2, PVMODE*2
      CHARACTER   TFORM*8, LINE1*132
      LOGICAL     OKVLB, PHSWARN, PHASING, VLAUSED
      LOGICAL     WRTHEAD
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'VLASCHK starting' )
C
C     Determine if the VLA was used and, if so, get its station
C     number.
C
      IF( VLAONLY ) GO TO 999
      VSTA = 0
      DO ISTA = 1, NSTA
         IF( STANAME(ISTA)(1:3) .EQ. 'VLA' ) VSTA = ISTA
      END DO
      VLAUSED = VSTA .NE. 0
      IF( .NOT. VLAUSED ) GO TO 999
C
C     Check that a VLBI mode is being requested for recording scans.
C     Otherwise VLA data will not be possible to correlate in a 
C     normal manner as the fringe rotators etc will be on.  Allow 
C     pointing to occur during reference pointing scans (mode IR).
C
      OKVLB = .TRUE.
      PHASING = .FALSE.
      DO ISCN = 1, NSCANS
         IF( VLBITP .AND. .NOT. NOREC(ISCN) .AND. 
     1       STASCN(ISCN,VSTA) ) THEN
            IF( VLAMODE(ISCN) .NE. 'VS' .AND. 
     1          VLAMODE(ISCN) .NE. 'VX' .AND.
     2          VLAMODE(ISCN) .NE. 'VA' .AND. 
     3          VLAMODE(ISCN) .NE. 'VB' .AND.
     4          VLAMODE(ISCN) .NE. 'VL' .AND.
     5          VLAMODE(ISCN) .NE. 'VR' .AND.
     6          VLAMODE(ISCN) .NE. 'IR' ) THEN
               OKVLB = .FALSE.
            END IF
C
C           Detect phasing being used.
C
            IF( VLAMODE(ISCN) .EQ. 'VA' .OR. 
     3          VLAMODE(ISCN) .EQ. 'VB' .OR.
     4          VLAMODE(ISCN) .EQ. 'VL' .OR.
     5          VLAMODE(ISCN) .EQ. 'VR' ) THEN
               PHASING = .TRUE.
            END IF
         END IF
      END DO
      IF( .NOT. OKVLB ) THEN
         CALL WLOG( 1, 'VLASCHK:  --------   WARNING   ----------' )
         CALL WLOG( 1, '   For successful VLBI, the VLAMODE must ' //
     1       'be VS, VX, VA, VL, or VR' )
         CALL WLOG( 1, '   for all recording scans.' )
      END IF
C
C     Check that reasonable IFs are being used given the phasing 
C     requests.
C
      IF( PHASING ) THEN
         LMODE = ' '
         WRTHEAD = .TRUE.
         DO ISCN = SCAN1, SCANL
            PHSWARN = .FALSE.
C        
C           Save the last phasing mode IF channels and last phasing 
C           mode.  Allow the phasing scan to be a non-recording scan.
C        
            IF( VLBITP .AND. STASCN(ISCN,VSTA) ) THEN
               IF( VLAMODE(ISCN) .EQ. 'VA' .OR.
     1             VLAMODE(ISCN) .EQ. 'VB' .OR.
     2             VLAMODE(ISCN) .EQ. 'VR' .OR.
     3             VLAMODE(ISCN) .EQ. 'VL' ) THEN
                  LMODE = VLAMODE(ISCN)
               END IF
            END IF
C
C           Now check the recording scans.  Don't worry about non-recording
C           scans and single dish scans.  Also only worry if the VLA is
C           in the scan.
C
            IF( VLBITP .AND. .NOT. NOREC(ISCN) .AND. 
     1          STASCN(ISCN,VSTA) .AND. VLAMODE(ISCN) .NE. 'VS' ) THEN
               KS = NSETUP(ISCN,VSTA)
C        
C              Look for a VX scan before any phasing scans.
C        
               IF( VLAMODE(ISCN) .EQ. 'VX' .AND. LMODE .EQ. ' ' ) THEN
                  PHSWARN = .TRUE.
               END IF
C        
C              Check for correspondence between the IF's used and the
C              phasing mode of the last phasing scan.  Note that the
C              "last phasing scan" can be this scan if in active 
C              phasing mode.
C        
               DO ICH = 1, NCHAN(KS)
                  IF( IFCHAN(ICH,KS) .EQ. 'A' .AND. 
     1              ( LMODE .NE. 'VA' .AND. LMODE .NE. 'VR' ) ) THEN
                      PHSWARN = .TRUE.
                      PVMODE = LMODE 
                      PKS = KS
                      PSCN = ISCN
                  END IF
                  IF( IFCHAN(ICH,KS) .EQ. 'B' .AND. 
     1              ( LMODE .NE. 'VB' .AND. LMODE .NE. 'VR' ) ) THEN
                      PHSWARN = .TRUE.
                      PVMODE = LMODE 
                      PKS = KS
                      PSCN = ISCN
                  END IF
                  IF( IFCHAN(ICH,KS) .EQ. 'C' .AND. 
     1              ( LMODE .NE. 'VB' .AND. LMODE .NE. 'VL' ) ) THEN
                      PHSWARN = .TRUE.
                      PVMODE = LMODE 
                      PKS = KS
                      PSCN = ISCN
                  END IF
                  IF( IFCHAN(ICH,KS) .EQ. 'D' .AND. 
     1              ( LMODE .NE. 'VA' .AND. LMODE .NE. 'VL' ) ) THEN
                      PHSWARN = .TRUE.
                      PVMODE = LMODE 
                      PKS = KS
                      PSCN = ISCN
                  END IF
               END DO
C
C              Give details for the offending scan.
C
               IF( PHSWARN ) THEN
                  IF( WRTHEAD ) THEN
                     CALL WLOG( 1, ' ' )
                     CALL WLOG( 1, 
     1                  'VLASCHK:  **** WARNING ****' )
                     CALL WLOG( 1, 
     1                  '    One or more scans at the VLA uses an ' //
     2                  'unphased IF.' )
                     CALL WLOG( 1, 
     1                  '    This might be ok for VLA only ' //
     2                  'calibration scans.' )
                     CALL WLOG( 0,
     1                  '    For VX mode scans, the check was '//
     2                  'against the preceeding phasing scan.' )
                     CALL WLOG( 1, 
     1                  '    Details are in sched.runlog.' )  
                     CALL WLOG( 0, 
     1                  '    The offending scans are:' )
                     CALL WLOG( 0, 
     1                  '    The second set of IFCHANs for VX ' //
     2                  'scans are from the last phasing scan.' )
                     CALL WLOG( 0, 
     1                  ' Start time     Source   VLAMODE ' //
     2                  '    IFCHANs' )
                  END IF
                  LINE1 = ' '
                  CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
                  WRITE( LINE1(1:3), '( I3 )' ) DAY1
                  LINE1(6:13) = TFORM( START, 'T', 0, 2, 2, '::@' )
                  LINE1(15:26) = SCNSRC(ISCN)
                  LINE1(29:30) = VLAMODE(ISCN)
                  IH = 35
                  DO ICH = 1, NCHAN(KS)
                     LINE1(IH:IH+2) = IFCHAN(ICH,KS)
                     IH = IH + 2
                  END DO
                  IF( VLAMODE(ISCN) .EQ. 'VX' ) THEN
                     IH = IH + 4
                     DO ICH = 1, NCHAN(PKS)
                        LINE1(IH:IH+2) = IFCHAN(ICH,PKS)
                        IH = IH + 2
                     END DO
                  END IF
                  CALL WLOG( 0, LINE1 )
                  WRTHEAD = .FALSE.
               END IF
            END IF
         END DO
      END IF
C
C     Check pointing stuff.
C
      DO ISCN = 1, NSCANS
         IF( STASCN(ISCN,VSTA) ) THEN
            IF( VLAPEAK(ISCN) .NE. ' ' .AND.
     1          VLAPEAK(ISCN) .NE. 'T' .AND.
     2          VLAPEAK(ISCN) .NE. 'R' .AND.
     3          VLAPEAK(ISCN) .NE. 'S' .AND.
     4          VLAPEAK(ISCN) .NE. 'D' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A, I5 )' ) 
     1             'VLASCHK: Invalid VLAPEAK: ', VLAPEAK(ISCN),
     2             ' in scan ', ISCN 
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( 
     1            ' VLAPEAK must be '' '', ''T'', ''R'', ''S'', or'//
     2            ' ''D'' ' )
            END IF
C
            IF( (VLAPEAK(ISCN) .EQ. 'R' .OR. VLAPEAK(ISCN) .EQ. 'S' )
     1          .AND. VLAMODE(ISCN) .NE. 'IR' ) THEN
               CALL ERRLOG( 
     1             'VLASCHK: For VLAPEAK=R or S, VLAMODE must be IR' )
            END IF
C
            IF( VLAMODE(ISCN) .NE. 'IR' .AND.
     1          DOPEAK(ISCN) .GT. 0 ) THEN
               CALL WLOG( 1, 'VLASCHK ******  WARNING  ******' )
               CALL WLOG( 1, '      PEAK specified, but VLA not ' //
     1            'in pointing mode (VLAMODE=IR).  Intentional?' )
               CALL WLOG( 1, '      PEAK no longer ' //
     1            'affects VLA pointing.  See VLAPEAK in manual.' )
            END IF
C
         END IF
      END DO
C
C     Check the integration time.
C
      DO ISCN = 1, NSCANS
         IF( STASCN(ISCN,VSTA) ) THEN
            IF( VLAINTEG(ISCN) .LT. 0 .OR. VLAINTEG(ISCN) .GT. 999 )
     1         THEN
               WRITE( MSGTXT, '( A, I5, A, I5 )' ) 'VLASCHK: VLAINTEG ',
     1            VLAINTEG(ISCN), ' outside range 0 to 999 in scan ',
     2            ISCN
               CALL ERRLOG( MSGTXT )
            END IF
         END IF
      END DO
C
  999 CONTINUE
C
      RETURN
      END
