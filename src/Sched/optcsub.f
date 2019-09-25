      SUBROUTINE OPTCSUB( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in) LASTISCN, KSCN, ISCN
Cf2py intent(out) ADJUST, KEEP, DONE
C

C  ****** doesn't seem to be working properly  Mar 17, 2010.  Not making
C         subarrays and many output scans skipped.  Need to debug.
C         Might partly be a gfortran issue - which means more variables
C         might need to be SAVEd.

C     Routine for SCHED that trys to optimize sky coverage over each   
C     antenna by examining a grid in AZ and EL.  This routine is
C     different from OPTCELLS in that it uses, even encourages, 
C     subarraying.  There will be two primary subarrays, each with
C     at least OPMINANT antennas.  If the best such combination
C     leaves 2 or more antennas free (sources not up), those antennas
C     will go to some other source as a 3ed subarray.
C
C     During operation, the geometry parameters for the input
C     scans will be used to hold geometry for the current test time.
C
C     This routines does not take into account GAP, but that is 
C     added by OPTTIM later in SCHOPT.
C
      INCLUDE 'sched.inc'
C
      INTEGER     MAXISC
      PARAMETER   (MAXISC=1000)       ! Number of input scans.
C
      INTEGER           LASTISCN(MAXSTA), ISCN, JSCN, KSCN, ISTA
      INTEGER           SCNSTA(MAXSTA), NS1, NS2, NS3, SCN1, SCN2, SCN3
      LOGICAL           KEEP, ADJUST, DONE
      DOUBLE PRECISION  TIME1, TIME2, TIME3
      SAVE              SCN1, SCN2, SCN3, NS1, NS2, NS3
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'OPTCSUB starting ' )
      DONE = .FALSE.
      KEEP = .TRUE.
      ADJUST = .TRUE.
C
C     Some initializations on first scan.
C
      IF( KSCN .EQ. 1 ) THEN
C
         IF( OPNOSUB ) CALL ERRLOG( 'OPTCSUB: Mode CSUB and OPNOSUB'//
     1       ' do not make sense together.' )
C
C        Start the set of new scans beyond NSCANS and use first start
C        time.  TAPPROX is the approximate start time of the next scan.
C        Don't allow this time to be adjusted.
C
         ADJUST   = .FALSE.
         STARTJ(ISCN) = STARTJ(1)
C
C        Make sure that OPDUR is available.
C
         IF( OPDUR .LE. 0.D0 ) THEN
            CALL ERRLOG( 
     1       ' OPTCSUB: For OPTMODE=CSUB, OPDUR must be given.' )
         END IF
C
      END IF
C
C     If there are no subarrays needing to be put into scans,
C     get the subarrays for the next time.
C
      IF( NS1 .LT. 2 .AND. NS2 .LT. 2 .AND. NS3 .LT. 2 ) THEN
C
         CALL OPTCSAR( LASTISCN, KSCN, ISCN, SCNSTA, NS1, NS2, NS3, 
     1                 SCN1, SCN2, SCN3, TIME1, TIME2, TIME3 )
C
      END IF
C
C     Pass out the next subarray.
C
      JSCN = 0
 
C          Here scn2 is bad (big negative number) when ns2 is reasonable.
      IF( NS1 .GE. 2 ) THEN
         JSCN = SCN1
         STARTJ(ISCN) = TIME1
         NS1 = 0
      ELSE IF( NS2 .GE. 2 ) THEN
         JSCN = SCN2
         STARTJ(ISCN) = TIME2
         NS2 = 0
      ELSE IF( NS3 .GE. 2 ) THEN
         JSCN = SCN3
         STARTJ(ISCN) = TIME3
         NS3 = 0
      END IF
C
      IF( JSCN .EQ. 0 ) THEN
         WRITE( MSGTXT, '( A, I5 ) ' ) 'OPTCSUB: No data for scan ', 
     1                    ISCN
         CALL ERRLOG( MSGTXT )
      END IF
C
C     Copy JSCN stuff into scan ISCN.
C     The nominal STARTJ and STOPJ must also be set.
C
      CALL SCNDUP( ISCN, JSCN, .FALSE., 'OPTCSUB' )
      STOPJ(ISCN) = STARTJ(ISCN) + DUR(ISCN)
C
C     Set the flags for which antennas are in the current scan.
C
      DO ISTA = 1, NSTA
         STASCN(ISCN,ISTA) = JSCN .EQ. SCNSTA(ISTA)
      END DO
C      write(*,*) 'optcsub: ', iscn, (stascn(iscn,ista),ista=1,nsta),
C     1    jscn, scn1, scn2, scn3
C
      RETURN
      END
