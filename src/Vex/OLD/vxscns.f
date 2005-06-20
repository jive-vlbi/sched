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
      INTEGER ISCN, ISETFL, IMODE, I, IFS, IPS
      INTEGER ISTA, NMDORI
      CHARACTER CALSET*4, CALSCN*4
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'VXSCNS: Starting.' )
C
C     Loop through scans
C
      NMDORI = NMDVEX
      OLDSCN = 0
      OLDIPS = 0
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
C        The VEX modes are the same across antennas, but some
C        antennas may join later, so find the "oldest" freq mode
C        or an IPS change, set IFS and IPS to A valid number
C
         FRSTSCN = MAXSCN + 1
         IPS = 0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN  
               IF (FSETSCN(FSETI(ISCN,ISTA)).LT.FRSTSCN) THEN
                  IFS = FSETI(ISCN,ISTA)
                  FRSTSCN = FSETSCN(IFS)
               END IF
               IF (IPS .EQ. 0) IPS = PSETI(ISCN,ISTA)
               IF (IPS.GT.MPSET.OR.IFS.GT.MFSET) THEN
                  CALL ERRLOG('VXSCNS: Too many frequency'//
     .                ' mode or Pcal changes ')
                  STOP
               END IF
C
C              I can only assume this was a leftover Huib v75
C
C               IF (IPS .NE. PSETI(ISCN,ISTA)) THEN
C                  CALL ERRLOG('VXSCNS: All telescopes should '//
C     .                'use similar PCal setup ')
C                  STOP
C               END IF
            ENDIF
         END DO
C
C        In case no station is recording this could have resulted in IFS=0, IPS=0
C
         IF( IPS .EQ. 0 ) IPS = 1
         IF( IFS .EQ. 0 ) THEN
            IFS = 1
            FRSTSCN = FSETSCN(IFS)
         END IF
C
C        Account for the telescopes that came in for first time
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN  
               IF (VXMDIFP(IFS,IPS) .GT. 0 .AND. 
     .             FSETSCN(FSETI(ISCN,ISTA)).NE.FRSTSCN .AND. 
     .             FSETSCN(FSETI(ISCN,ISTA)).EQ.ISCN ) THEN
                  IF( DEBUG ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, A, A, I3 )' ) 
     .                   'VXSCNS: Telescope: ', staname(ista),
     .                   ' joins mode set in scan: ', 
     .                   FSETSCN(FSETI(ISCN,ISTA))
                     CALL WLOG( 1, MSGTXT )
                  END IF
                  MODSET(ISTA,VXMDIFP(IFS,IPS)) = MODSET(ISTA,IMODE)
C
               END IF
            END IF
         END DO
C
C        Totally new mode, remeber IFS, IPS are set for one good value
C        Problem ifs, ips could be first scan unmodified or modified..
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
     .             'VXSCNS: Unused or new mode in scan ',ISCN
               CALL WLOG( 1, MSGTXT )
            END IF
            IF( FREQ(1,FRSTSCN).LT.1E-6 .AND. 
     .          BW(1,FRSTSCN).LT.1E-6 .AND. 
     .          CALSET .EQ. CALSCN ) 
     .          THEN
C     
C               Must set all other telescopes too
C
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) ) THEN
C     
                     VXMDIFP(FSETI(ISCN,ISTA),
     .                   PSETI(ISCN,ISTA)) = IMODE
                  END IF
               END DO
            ELSE
C
C               if this is not the original mode?
C
               CALL VXTRAMD(IMODE,IFS,IPS)
               FRSTSCN = ISCN
               MODSCN(ISCN) = NMDVEX
C
C               Must set all other telescopes too
C
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) ) THEN
C     
                     VXMDIFP(FSETI(ISCN,ISTA),
     .                   PSETI(ISCN,ISTA)) = NMDVEX
                  END IF
               END DO
C
C              list all antennas in this mode
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
C        Any Mode switch
C
         IF (FRSTSCN.NE.OLDSCN .OR.
     .       IPS.NE.OLDIPS) THEN
            IF( FSETSCN(IFS) .NE. OLDSCN ) THEN 
               OLDSCN = FSETSCN(IFS)
            END IF
            IF ( OLDIPS .NE. IPS ) THEN 
               OLDIPS = IPS
            END IF 
         END IF
         MODSCN(ISCN) = VXMDIFP(IFS,IPS)
C
C        end loop all scans
C
      END DO
C     
      RETURN
      END
