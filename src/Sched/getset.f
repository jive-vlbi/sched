      SUBROUTINE GETSET
C
C     Routine for SCHED that reads setup files.
C
C     The routine reads all of the setup files in the array SETFILE.  
C     There are NSETF such files - all that were encountered in the 
C     main input.
C
C     Note that this routine is called after SCHIN - after the main
C     program input is read.  It will only need to actually read any
C     setup files that are external to the main input.  Imbedded
C     setups are read from SCHFILES using RDSET.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    ISETF, KS, ICH ! , len1
      LOGICAL    READSET
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0,'GETSET: Starting.' )
C
C     Loop through the setup files that were encountered in the
C     schedule.  Each may have several groups.  Read any that still
C     need it.
C
      DO ISETF = 1, NSETF
C
C        Require a setup for all scans.
C
         IF( SETFILE(ISETF) .EQ. 'DUMMY' .OR. 
     1       SETFILE(ISETF) .EQ. 'DEFAULT' ) THEN
            CALL ERRLOG( ' SETUP file required. ' )
C
         ELSE
C
C           Check if setup was read in-line.  If not, go get it.
C
            READSET = .TRUE.
            IF( NSET .NE. 0 ) THEN
               DO KS = 1, NSET
                  IF( SETFILE(ISETF) .EQ. SETNAME(KS) ) 
     1                READSET = .FALSE.
               END DO
            END IF
            IF( READSET ) THEN
               CALL RDSET( SETFILE(ISETF), IUSET, .TRUE., ISETF )
            END IF
C
         END IF
C
      END DO
C
C     Initialize the allowed IF frequency ranges.  These will be
C     used to check that Doppler or schedule set frequencies are ok.
C     For now, don't know anything, so set wide open.  Later set
C     according to the frequency catalog values.
C     Also initialize CORINV, the frequency offset when we are going
C     to count on the correlator inverting the sideband.
C
      DO KS = 1, NSET
         DO ICH = 1, MCHAN
            FIFMIN(ICH,KS) = 0.D0
            FIFMAX(ICH,KS) = 1.D15
            CORINV(ICH,KS) = 0.D0
         END DO
      END DO
C
      RETURN
      END
