      SUBROUTINE VXSCNS
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 130596 
C     Major overhaul to accomodate simplifications
C     in registering freq and pcal changes Huib 210599
C
C     This is the routine that sorts out the main hurdle in 
C     SCHED -> VEX: The fact that FREQ and PCAL can be changed
C     in the schedule section, but require a global $MODE change
C     in VEX. In addition most FREQ changes, imply changes in 
C     PHASE_CAL and sometimes vice versa. 
C     Routine sets up the array MODSCN in VXLINK.INC
C     And calls for the creation of new modes: VXTRAMD
C
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER OLDSCN, PSETI, OLDIPS, FRSTSCN
      INTEGER VXMDIFP(MFSET,MPSET)
      INTEGER ISCN, ISETFL, IMODE, I, IFS, IPS, J
C      INTEGER JSCN, RECSCN
      INTEGER ISTA, NMDORI, VXGTST, ISET
      CHARACTER CALSET*4, CALSCN*4
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'VXSCNS: Starting.' )
C
C     Loop through scans
C
      NMDORI = NMDVEX
      OLDSCN = 0
      OLDIPS = 0
C CR 050106: initialise VXMDIFP
      DO I = 1, MFSET
        DO J = 1, MPSET
          VXMDIFP(I,J) = 0
        END DO
      END DO
C
      DO ISCN = SCAN1, SCANL
C
C        First default IMODE to old mode which equals a setup file
C
         ISETFL = SETNUM(ISCN)
         IMODE = -1
         DO I = 1, NMDORI
            IF( MDISFIL(I) .EQ. ISETFL ) IMODE = I
         END DO
         IF( IMODE .LT. 0 ) CALL ERRLOG('VXSCNS: Unexpected Mode'//
     1           ' encountered ')
C
         MODSCN(ISCN) = IMODE
C
C        If this Mode uses FORMAT=NONE, then  do nothing more here. This
C        mode can later be replaced with the mode of the previous 
C        recording scan, or can simply be skipped when writing the
C        scans.
C
         ISET = VXGTST( IMODE )
         IF( FORMAT(ISET)(1:4) .NE. 'NONE' ) THEN
C
C          The VEX modes are the same across antennas, but some
C          antennas may join later, so find the "oldest" freq mode
C          or an IPS change, set IFS and IPS to A valid number
C
           FRSTSCN = MAXSCN + 1
           IPS = 0
           DO ISTA = 1, NSTA
              IF( STASCN(ISCN,ISTA) ) THEN
                 IF (FSETSCN(FSETI(ISCN,ISTA)).LT.FRSTSCN) THEN
                    IFS = FSETI(ISCN,ISTA)
                    FRSTSCN = FSETSCN(IFS)
C                   bug fix CR 20051114: make sure ips and ifs get set
C                   to values from the same antenna
                    IPS = PSETI(ISCN,ISTA)
                 END IF
                 IF (IPS.GT.MPSET.OR.IFS.GT.MFSET) THEN
                    CALL ERRLOG('VXSCNS: Too many frequency'//
     .                  ' mode or Pcal changes ')
                    STOP
                 END IF
C
C                I can only assume this was a leftover Huib v75
C
C                 IF (IPS .NE. PSETI(ISCN,ISTA)) THEN
C                    WRITE( MSGTXT, '( A, A, I5 )' ) 'Problem with ',
C     1                  'PCAL in scan ',ISCN
C                    CALL WLOG( 1, MSGTXT )
C                    CALL PRTSCN( ISCN )
C                    CALL ERRLOG('VXSCNS: All telescopes should '//
C     .                  'use similar PCal setup ')
C                    STOP
C                 END IF
              ENDIF
           END DO
C
C          In case no station is recording this could have resulted in IFS=0, IPS=0
C
           IF( IPS .EQ. 0 ) IPS = 1
           IF( IFS .EQ. 0 ) THEN
              IFS = 1
              FRSTSCN = FSETSCN(IFS)
           END IF
C
C          Account for the telescopes that came in for first time
C
           DO ISTA = 1, NSTA
              IF( STASCN(ISCN,ISTA) ) THEN  
                 IF (VXMDIFP(IFS,IPS) .GT. 0 .AND. 
     .               FSETSCN(FSETI(ISCN,ISTA)).NE.FRSTSCN .AND. 
     .               FSETSCN(FSETI(ISCN,ISTA)).EQ.ISCN ) THEN
                    IF( DEBUG ) THEN
                       MSGTXT = ' '
                       WRITE( MSGTXT, '( A, A, A, I3 )' ) 
     .                     'VXSCNS: Telescope: ', STANAME(ISTA),
     .                     ' joins mode set in scan: ', 
     .                     FSETSCN(FSETI(ISCN,ISTA))
                       CALL WLOG( 1, MSGTXT )
                    END IF
                    MODSET(ISTA,VXMDIFP(IFS,IPS)) = MODSET(ISTA,IMODE)
C
C                   set VXMDIFP and MODSET for the new station's IFS and IPS
C                   (bug fix, CR 20051005)
C
                    VXMDIFP(FSETI(ISCN,ISTA), PSETI(ISCN,ISTA)) =
     1                          VXMDIFP(IFS,IPS)
C
                 END IF
              END IF
           END DO
C
C          Totally new mode, remember IFS, IPS are set for one good value
C          Problem ifs, ips could be first scan unmodified or modified..
C
           IF( VXMDIFP(IFS,IPS) .EQ. 0 ) THEN
C
              CALSET = SPCAL(FSETKS(IFS))
              CALSCN = PSPCAL(IPS)
              CALL UPCASE(CALSET)
              CALL UPCASE(CALSCN)
              IF( DEBUG ) THEN
                 MSGTXT = ' '
                 WRITE( MSGTXT, '( A, I3 )' )
     .               'VXSCNS: Unused or new mode in scan ',ISCN
                 CALL WLOG( 1, MSGTXT )
              END IF
              IF( FREQ(1,FRSTSCN).LT.1E-6 .AND. 
     .            BW(1,FRSTSCN).LT.1E-6 .AND. 
     .            CALSET .EQ. CALSCN ) 
     .            THEN
C     
C                 Must set all other telescopes too
C
                 DO ISTA = 1, NSTA
                    IF( STASCN(ISCN,ISTA) ) THEN
C     
                       VXMDIFP(FSETI(ISCN,ISTA),
     .                     PSETI(ISCN,ISTA)) = IMODE
                    END IF
                 END DO
              ELSE
C
C                if this is not the original mode?
C
                 CALL VXTRAMD(IMODE,IFS,IPS)
                 FRSTSCN = ISCN
                 MODSCN(ISCN) = NMDVEX
C
C                 Must set all other telescopes too
C
                 DO ISTA = 1, NSTA
                    IF( STASCN(ISCN,ISTA) ) THEN
C     
                       VXMDIFP(FSETI(ISCN,ISTA),
     .                     PSETI(ISCN,ISTA)) = NMDVEX
                    END IF
                 END DO
C
C                list all antennas in this mode
C
                 DO ISTA = 1, NSTA  
                    IF( STASCN(ISCN,ISTA) ) THEN
                       MODSET(ISTA,NMDVEX) = MODSET(ISTA,IMODE)
                    END IF
                 END DO
              END IF
C
           END IF
C
C          Any Mode switch
C
           IF (FRSTSCN.NE.OLDSCN .OR.
     .         IPS.NE.OLDIPS) THEN
              IF( FSETSCN(IFS) .NE. OLDSCN ) THEN 
                 OLDSCN = FSETSCN(IFS)
              END IF
              IF ( OLDIPS .NE. IPS ) THEN 
                 OLDIPS = IPS
              END IF 
           END IF
           MODSCN(ISCN) = VXMDIFP(IFS,IPS)
         END IF
C
C        end loop all scans
C
      END DO
C     CR 050818: The logic below works if you want the FORMAT=NONE scans
C       to appear in the VEX file as non-recording scans, using the mode from
C       another scan. However, at the current time FS stations would
C       prefer that there were simply gaps in the schedule so they can
C       use their own pointing procedures. So leave this section of code
C       commented out for now and in VXSCH, the FORMAT=NONE scans can be
C       skipped.
C
C     Now loop through the scans again and replace the MODSCN for any with 
C     FORMAT=NONE (not processed first time) with the MODSCAN from a recording 
C     scan.
C
C      DO ISCN = SCAN1, SCANL
C         ISET = VXGTST( MODSCN(ISCN) )
C         IF( FORMAT(ISET)(1:4) .EQ. 'NONE' ) THEN
C           RECSCN = 0
CC          Need to allow possibility of using a later scan in case there
CC          is no previous recording scan (there must be one recording
CC          scan somewhere)
C           DO JSCN = SCAN1, SCANL
C             IF( RECSCN .EQ. 0 .OR. JSCN .LT. ISCN ) THEN
C               IF( .NOT. NOREC(JSCN) ) THEN
C                 RECSCN = JSCN
C               END IF
C             END IF
C           END DO
C           IF( RECSCN .EQ. 0 ) THEN
C             CALL ERRLOG( 'VXSCNS: there do not appear to be any ' //
C     1       'recording scans in this schedule! Check use of '//
C     2       'NORECORD, POINT and FORMAT=NONE' )
C           ELSE
C             MODSCN(ISCN) = MODSCN(RECSCN)
C           END IF
C         END IF
C      END DO
C     
      RETURN
      END
