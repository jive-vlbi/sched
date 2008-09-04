      SUBROUTINE WRAP( ISCN, LSCN, ISTA )
C
C     Routine for sched that determines where in the full range of
C     azimuth for ALTAZ antennas the antenna will be.  Typically there
C     will be a range of azimuths where there are two possible antenna
C     positions (cable wrap) and which is in use depends on the 
C     recent history of scans.  This routine attempts to duplicate
C     what will happen at the antenna.  It has to worry about the
C     full time of the scan (by looking at both ends).
C
C     This routine will be called by OPTGEO for use while optimizing
C     the pointing to take into account slew times and in SCHSRC
C     so that AZ1 and AZ2 in the main schedule variables have the
C     wrap position.  Then SLEW does not need to worry about wraps.
C
      INCLUDE 'sched.inc'
C
      INTEGER        ISCN, LSCN, ISTA, KSTA
      INTEGER        IWRAP, NWRAP1, NWRAP2, IMSLEW
      REAL           CURAZ1, CURAZ2, LASTAZ, MINSLEW, AZSLEW
C ------------------------------------------------------------------
      KSTA = STANUM(ISTA)
C
C     Don't do anything if the mount is not ALTAZ.
C
      IF( MOUNT(KSTA) .NE. 'ALTAZ' ) RETURN
C
C     First get the previous position.  If the source was down,
C     assume that it was tracking at the station elevation limit. 
C     If this is the first scan for the station, assume that it
C     is at the low point of the wrap range.  This used to be the
C     mid point, but that option was causing problems at Effelsberg.
C
      IF( LSCN .NE. 0 ) THEN
         LASTAZ = AZ2(LSCN,ISTA)
      ELSE
         LASTAZ = AX1LIM(1,KSTA)
      END IF
C
C     Get the current antenna position.  These will be in range 0-360.
C     according to SCHGEO.
C
      CURAZ1 = AZ1(ISCN,ISTA)
      CURAZ2 = AZ2(ISCN,ISTA)
C
C     Adjust CURAZ2 to be on same wrap as CURAZ1 (but CURAZ2 might 
C     be outside the 0-360 range).
C
      IF( CURAZ2 - CURAZ1 .GT. 180.0 ) CURAZ2 = CURAZ2 - 360.0
      IF( CURAZ1 - CURAZ2 .GT. 180.0 ) CURAZ2 = CURAZ2 + 360.0
C
C     Find the range of possible wraps assuming that CURAZ1 is in 0-360.
C       Note:  For AX1LIM(1,KSTA) > 0 or AX1LIM(2,KSTA) < 360,
C       more wraps than necessary are checked, but these will be rare
C       cases and it is better to keep the code simple.
C
      NWRAP1 = INT( AX1LIM(1,KSTA) / 360.0 ) - 1
      NWRAP2 = INT( AX1LIM(2,KSTA) / 360.0 )
C
C     Now find the shortest slew consistent with the AZ limits.
C     Initialize IMSLEW so that, if no acceptable slew found, 
C     the antenna will be assumed to be down and the azimuth 
C     should be left in the original range.
C
      MINSLEW = 99999.0
      IMSLEW = 0
      DO IWRAP = NWRAP1, NWRAP2
         IF( CURAZ1 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     1       CURAZ1 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) .AND.
     2       CURAZ2 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     3       CURAZ2 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) ) THEN
            AZSLEW = ABS( CURAZ1 + IWRAP * 360.0 - LASTAZ )
            IF( AZSLEW .LT. MINSLEW ) THEN
               MINSLEW = AZSLEW
               IMSLEW = IWRAP
            END IF
         END IF
      END DO
C
C     Set the new azimuths into the az arrays.
C
      AZ1(ISCN,ISTA) = CURAZ1 + IMSLEW * 360.0
      AZ2(ISCN,ISTA) = CURAZ2 + IMSLEW * 360.0
C
      RETURN
      END
