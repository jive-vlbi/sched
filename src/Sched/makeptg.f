      SUBROUTINE MAKEPTG( LASTISCN, ISCN, KEEP )
Cf2py intent(in) LASTISCN, ISCN
Cf2py intent(in, out) KEEP
C
C     Subroutine for SCHED, called by SCHOPT, that converts a scan into
C     a reference pointing scan.  This involves:
C
C       Changing the antenna list to only include antennas that have
C       been specified as able to do reference pointing.
C
C       Change the setup file to the reference pointing setup file.
C
C     For the VLA, it also sets up non-pointing scans to use the
C     pointing results if any have been derived.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
      INCLUDE     'schpeak.inc'
C
      INTEGER      ISCN, LASTISCN(*)
      LOGICAL      KEEP
C
      INTEGER      IGRP, NPST, ISTA, ICHN, IG, KS, ISRC
      INTEGER      USESET, USELSET, GNSET
      LOGICAL      USEGRP(MPKGRP), FIRST, DOCRD
      DOUBLE PRECISION  LVSTOP(MAXSTA)
      REAL         LVAZ(MAXSTA), LVEL(MAXSTA)
      SAVE         LVSTOP, LVAZ, LVEL, FIRST
      DATA         FIRST  / .TRUE. /
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. SCAN1 + 5 ) 
     1         CALL WLOG( 0, 'MAKEPTG starting' )
C
      IF( FIRST ) THEN
         DO ISTA = 1, NSTA
            LVSTOP(ISTA) = 0.D0
            LVAZ(ISTA) = 1.E5
            LVEL(ISTA) = 1.E5
         END DO
         FIRST = .FALSE.
      END IF
C
C     Get the source number
C
      ISRC = SRCNUM(ISCN)
C
C     Deal with a non-pointing scan.  For now, this only means setting
C     VLAPEAK to an appropriate value.  Don't change it if the user
C     has set it (values other than D).  Otherwise set it to T if
C     there has been a pointing scan recently and close enough.  The
C     tolerance limits should probably be input parameters some day.
C     Note that the default for LVSTOP should preclude setting 
C     VLAPEAK = T before any pointing has been done, including in
C     projects that don't involve pointing.
C
C     Note the on the VLBA, you can't turn off the use of the last
C     peak until the project code changes.  Once pointing is started,
C     it must be continued, which is why nothing can be done here.
C
C     It is assumed that the AZ and EL are already associated with
C     the scans, which should be true after ADDPEAK or the SCNGEO
C     call earlier in SCHOPT.
C
      IF( POINT(ISCN) .LT. 0 ) THEN
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA))(1:3) .EQ. 'VLA' .AND.
     1          STASCN(ISCN,ISTA) ) THEN
C
               IF( VLAPEAK(ISCN) .EQ. 'DETERMINE' ) THEN
C
C                 Check if the last pointing was appropriate.
C
                  IF( STARTJ(ISCN)-LVSTOP(ISTA) .LT. 2.0D0 / 24.D0 .AND.
     1                ABS( AZ1(ISCN,ISTA) - LVAZ(ISTA) ) .LT. 50.0 .AND. 
     2                ABS( EL1(ISCN,ISTA)-LVEL(ISTA) ) .LT. 40.0 ) THEN
C
                     VLAPEAK(ISCN) = 'APPLY'
                  ELSE
                     VLAPEAK(ISCN) = 'OFF'
                  END IF
C
               END IF
C
            END IF
         END DO
C        
      ELSE
C
C        Peaking requested.  Be sure that a pointing file (peak.cmd or 
C        whatever) was provided.
C
         IF( NPKGRP .LE. 0 ) THEN
            WRITE( MSGTXT, '( 2A )' )
     1        'MAKEPTG: Automatic conversion of scans to pointing ',
     2        'was requested, but no pointing '
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1        '         groups are available.  This should not happen.'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'MAKEPTG:  Programming problem.' )
         END IF
C   
C        Get the peakup group.
C   
         IGRP = POINT(ISCN)
C
C        Set STASCN to only include stations in the peakup group.
C        If POINT=0, take stations in any group.
C        Count those stations and record which groups are used.
C   
         DO IG = 1, NPKGRP
            USEGRP(IG) = .FALSE.
         END DO
         NPST = 0
         DO ISTA = 1, NSTA
            STASCN(ISCN,ISTA) = STASCN(ISCN,ISTA) .AND.
     1           ( ( PKGROUP(ISTA) .EQ. IGRP .OR. 
     2           IGRP .EQ. 0 ) .AND. PKGROUP(ISTA) .NE. 0 )
            IF( STASCN(ISCN,ISTA) ) THEN
               NPST = NPST + 1
               USEGRP(PKGROUP(ISTA)) = .TRUE.
            END IF
         END DO
C   
C        Skip the scan if there are no stations in it after the above
C        selection.
C   
         IF( NPST .EQ. 0 ) THEN
            KEEP = .FALSE.
            RETURN
         END IF
C
C        If POINT is zero (IGRP=0), that means use any stations in
C        the pointing file.  Insist that all stations involved use 
C        the same setup file.  Check that to be sure.  Waited to here
C        so that we only check groups that were used.
C
         USESET = 0
         USELSET = 0
         IF( IGRP .EQ. 0 .AND. NPKGRP .GT. 1 ) THEN
            DO IG = 1, NPKGRP
C
               IF( USEGRP(IG) ) THEN
C
C                 Record the setup when first seen.
C
                  IF( USESET .EQ. 0 ) USESET = PKLSET(IG)
                  IF( USELSET .EQ. 0 ) USELSET = PKLSETL(IG)
C
C                 Look for a change that shouldn't happen.
C
                  IF( PKLSET(IG) .NE. USESET .OR. 
     1                PKLSETL(IG) .NE. USELSET) THEN
                     CALL WLOG( 1, 'MAKEPTG:  POINT was set to zero '//
     1                  '(or no value) for some scan and' )
                     CALL WLOG( 1, '          not all groups in the '//
     1                  'PEAKFILE use the same setup file' )
                     CALL WLOG( 1, '          or the same line '//
     1                  'setup file.')
                     CALL WLOG( 1, '          Either specify  '//
     1                  'pointing groups with POINT or use common '//
     2                  'pointing setups.' )
                     CALL ERRLOG( 
     1                  'MAKEPTG: change POINT or pointing setups.' )
                  END IF
C
               END IF
C
            END DO
         END IF
C   
C        Make the necessary conversions.  Assume that the source is
C        right - either set by the scheduler or added by ADDPEAK.
C   
C        Setup file.  Use different ones for line and continuum 
C        sources.
C   
         IF( CALCODE(SRCNUM(ISCN)) .NE. 'L' ) THEN
            SETNUM(ISCN) = PKLSET( MAX( 1, IGRP ) )      
         ELSE
            SETNUM(ISCN) = PKLSETL( MAX( 1, IGRP ) )      
         END IF
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               NSETUP(ISCN,ISTA) = GNSET(ISCN,ISTA)
            END IF
         END DO
C   
C        Doppler specifications.
C          If the source did not have VLSR set, it was set to -1.E9.
C          If the bandwidth should be 2 MHz, the CALCODE is 'L', which
C          is also a flag to use the difference scheme for getting
C          pointing data (not implemented for reference pointing).
C   
C        If the RDBE_PFB is in use, use the CRD parameters, not the
C        main DOPCAL etc., because the PFB can't tune adequately.
C        Search all stations for a PFB.
C
         DOCRD = .FALSE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
               IF( DBE(KS) .EQ. 'RDBE_PFB' ) DOCRD = .TRUE.
            END IF
         END DO
C
         IF( VLSR(1,SRCNUM(ISCN)) .GT. -1.E8 ) THEN
C
C           Needs doppler
C
            IF( DOCRD ) THEN
C
C              Don't set DOPCAL because we cannot tune the PFB.
C              Just set CRDDOP.
C
               GOTCRD(ISCN) = .TRUE.
               CRDDOP(ISCN) = .TRUE.
               CRDNCH(ISCN) = 2
               CRDSETCH(1,ISCN) = 1
               CRDSETCH(2,ISCN) = 2
               IF( CALCODE(ISRC) .EQ. 'L' ) THEN
                  DO ICHN = 1, MAXCHN
                     CRDBW(ICHN,ISCN) = 2.0D0
                  END DO
               ELSE
                  DO ICHN = 1, MAXCHN
                     CRDBW(ICHN,ISCN) = MIN( 16.D0, BBFILT(ICHN,KS) )
                  END DO
               END IF
               DO ICHN = 1, MAXCHN
                  CRDFREQ(ICHN,ISCN) = 0.0D0
               END DO
            ELSE
C
C              Not PFB, so tune the main channels.
C
               DOPCAL(ISCN) = .TRUE.
               DO ICHN = 1, MAXCHN
                  BW(ICHN,ISCN) = 2.0D0
                  FREQ(ICHN,ISCN) = 0.0D0
               END DO
            END IF
            PCAL(ISCN) = 'off'
         ELSE
C
C           Not a line source.  Set for continuum pointing.
C           Do not need any Doppler, crd or main.
C           Set for setup file bandwidth.

            DOPCAL(ISCN) = .FALSE.
            DO ICHN = 1, MAXCHN
               BW(ICHN,ISCN) = 0.0D0
            END DO
         END IF
C   
         TANT1(ISCN) = .FALSE.
         TANT2(ISCN) = .FALSE.
         NOTSYS(ISCN) = .TRUE.
         IF( DOPEAK(ISCN) .LE. 0 ) DOPEAK(ISCN) = 1
         LINES(ISCN) = PKLINES( MAX( 1, POINT(ISCN) ) )
         NOREC(ISCN) = .TRUE.
C   
C        Store the last time and position of a pointing scan for use 
C        in determining whether to use these results in future scans.  
C        This is really only useful for the VLA, so that is the 
C        only station filled in, but keep everything in arrays in case
C        it gets useful on other stations later.   While at it,
C        set the VLAMODE and VLAPEAK if the VLA is in the scan.
C        Not using the LASTISCN values because that might not have
C        been a pointing scan.
C        
C        For the VLA, assume we start fresh without using
C        previous solutions.  I am not setting up the AUTOPEAK
C        or POINT functions to handle double reference pointing yet.
C        We may not have the geometry yet so get what is needed.
C   
C        Again, assume that AZ and EL are already calculated.
C
         DO ISTA = 1, NSTA
            IF( ( STATION(STANUM(ISTA)) .EQ. 'VLA' .OR. 
     1          STATION(STANUM(ISTA)) .EQ. 'VLA27' ) .AND.
     2          STASCN(ISCN,ISTA) ) THEN
C
               LVSTOP(ISTA) = STOPJ(ISCN)
               LVAZ(ISTA) = AZ1(ISCN,ISTA)
               LVEL(ISTA) = EL1(ISCN,ISTA)
C
C              This IF block is redundant until other stations 
C              are involved.
C
               IF( STATION(STANUM(ISTA)) .EQ. 'VLA' .OR. 
     1             STATION(STANUM(ISTA)) .EQ. 'VLA27' ) THEN
                  VLAMODE(ISCN) = ' '
                  VLAPEAK(ISCN) = 'DETERMINE'
               END IF
            END IF
         END DO
C   
      END IF
C   
      RETURN
      END
