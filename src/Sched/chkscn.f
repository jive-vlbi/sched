      SUBROUTINE CHKSCN
C
C     Routine for SCHED called by the main routine that checks scan 
C     parameters after SCHOPT and all other scan adjusting routines
C     are run.  Also, collects the "frequency sets".  See also
C     CHKSC1 which is called before SCHOPT.
C
C     VEX driven correlators need to have all stations to be correlated
C     together to be in the same scan.  That was not true of the log
C     driven correlators.  Warn about probable attempts to schedule
C     parallel scans that are meant to be correlated together.
C     Added Dec. 26, 2012.  RCW.
C
C     Set up INTENTS for pointing at stations other than the VLA.
C     Added Jan. 18, 2013.  RCW.
C
C     Deal with VLAPTIME.  Jan. 4, 2013
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, KCHAN, ISET, ISTA, KSTA, LSRC, JSCN, KS, KF
      INTEGER    ICH, IINT
      INTEGER    NEXPECT, NLATE, NNEVER, NSLATE, CRSETC(MAXCHN)
      REAL       NSRCCHG
      LOGICAL    DOPWARN, WARNLONG, OVWARN, VLAPTG
      LOGICAL    SKIPI, SKIPJ
      DOUBLE PRECISION  FRSUM(MAXCHN), FRBB(MAXCHN), FRBW(MAXCHN)
      DOUBLE PRECISION  TIME1, TIMEDUR, MINFR(MSET), MAXFR(MSET)
      DOUBLE PRECISION  MINFRI, MAXFRI, MINFRJ, MAXFRJ
      DOUBLE PRECISION  MINDEF, MAXDEF, FR1, FR2
      DOUBLE PRECISION  SCANLEN, VPT
      INTEGER            CRDN
      DOUBLE PRECISION   CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      CHARACTER          CRDS(MCHAN)*1, USEDDBE*8, USEDSETF*80
      PARAMETER         ( MINDEF = 1.D15 )
      PARAMETER         ( MAXDEF = 0.D0 ) 
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKSCN starting' )
C
C     Try to protect against errors in use of DOPSRC.  Assume that if  
C     DOPSRC .NE. SOURCE, SOURCE is probably meant to be a continuum
C     calibrator.  Therefore check in that situation if DOPPLER
C     Continuum sources have channel velocities of -1.E9
C
      DOPWARN = .FALSE.
      DO ISCN = SCAN1, SCANL
         IF( SRCNUM(ISCN) .NE. IDOPSRC(ISCN) .AND. DOPCAL(ISCN) ) THEN
            DO KCHAN = 1, MSCHN(SETNUM(ISCN))
               IF( VLSR(KCHAN,SRCNUM(ISCN)) .GT. -1.E8 .AND.
     1             VLSR(KCHAN,IDOPSRC(ISCN)) .GT. -1.E8 ) THEN
                  DOPWARN = .TRUE.
               END IF
            END DO
         END IF
      END DO
      IF( DOPWARN ) THEN
         CALL WLOG( 1, 'CHKSCN:  **** WARNING ****' )
         CALL WLOG( 1, '    This schedule contains scans ' //
     1      'for which SOURCE and DOPSRC are different and' )
         CALL WLOG( 1, '    are both line sources.' //
     1              '  Is this intentional?' )
      END IF
C
C     Determine which setups were actually used.  Some had been kept
C     because they might be used in inserted pointing scans.  Here
C     is where we flag them as not used.
C
      DO ISET = 1, NSET
         USED(ISET) = .FALSE.
         DO ISCN = SCAN1, SCANL
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) .AND. NSETUP(ISCN,ISTA) .EQ. ISET )
     1             THEN
                  USED(ISET) = .TRUE.
                  GO TO 100
               END IF
            END DO
         END DO
  100    CONTINUE
      END DO
C
C     Check for violations of the maximum number of scans per hour 
C     specified.
C
      DO ISTA = 1, NSTA
         IF( MAXSRCHR(STANUM(ISTA)) .LT. 1.D4 ) THEN
            LSRC = 0
            NSRCCHG = 0
            DO ISCN = SCAN1, SCANL
                IF( STASCN(ISCN,ISTA) ) THEN
                   IF( LSRC .EQ. 0 ) TIME1 = STARTJ(ISCN)
                   TIMEDUR = STOPJ(ISCN) - TIME1
                   IF( SRCNUM(ISCN) .NE. LSRC ) NSRCCHG = NSRCCHG + 1.0
                   LSRC = SRCNUM(ISCN)
                END IF
            END DO
C
C           Abort with info if there are too many changes.
C
            IF( NSRCCHG / TIMEDUR .GT. MAXSRCHR(STANUM(ISTA)) * 24.0 ) 
     1           THEN
               CALL WLOG( 1, 'CHKSCN: ***************************' //
     1                '****************' )
               CALL WLOG( 1, '    TOO MANY SOURCES PER HOUR AT ' //
     1               STANAME(ISTA) )
               MSGTXT = '  '
               WRITE( MSGTXT, '( A, F5.1 )' )
     1               '    OBSERVATORY WILL NOT ALLOW MORE THAN', 
     2               MAXSRCHR(STANUM(ISTA))
               CALL WLOG( 1, MSGTXT )
               MSGTXT = '  '
               WRITE( MSGTXT, '( A, F5.1 )' )
     1               '    YOU HAVE SPECIFIED ', 
     2               NSRCCHG / ( TIMEDUR * 24.D0 )
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, 
     1             '    REDUCE NUMBER OF SCANS FOR THIS STATION' )
C               Switch to this line if we want this to stop SCHED.
C               CALL ERRLOG( 
C     1             '    REDUCE NUMBER OF SCANS FOR THIS STATION' )
               CALL WLOG( 1, 'CHKSCN: ***************************' //
     1                '****************' )
            END IF
         END IF
      END DO
C
C     Do some checking that only applies when using real setups.
C
      IF( .NOT. NOSET ) THEN
C
C        Get the "frequency sets".
C
         CALL GETFSET
C
C        "pcal sets" are no longer used.
C
C         CALL GETPSET
C
C        Check for possible data loss due to correlator resyncs and 
C        formatter reconfigures.
C
         CALL RESYNC
C
C        Check for adequate time to set the levels in the RDBE.
C
         CALL RDBELEVT
C
C        Enforce use of only one personality on the RDBE and DBBC
C
         DO ISTA = 1, NSTA
            USEDDBE = 'xx'
            KSTA = STANUM(ISTA)
            IF( DAR(KSTA)(1:4) .EQ. 'RDBE' .OR. 
     1          DAR(KSTA)(1:4) .EQ. 'DBBC' ) THEN
               DO ISCN = SCAN1, SCANL
                  IF( STASCN(ISCN,ISTA) ) THEN
                     KS = NSETUP(ISCN,ISTA)
                     IF( USEDDBE .EQ. 'xx' ) THEN
                        USEDDBE = DBE(KS)
                        USEDSETF = SETNAME(KS)
                     ELSE IF( USEDDBE .NE. DBE(KS) ) THEN
                        CALL WLOG( 1, 'CHKSCN: Cannot use 2 DBE ' //
     1                     'personalities in one experiment' )
                        CALL WLOG( 1, '        You used ' // USEDDBE //
     1                     ' and ' // DBE(KS) // ' at ' // 
     2                     STANAME(ISTA) )
                        CALL WLOG( 1, '        The setup files were:' )
                        CALL WLOG( 1, '           ' // SETNAME(KS) )
                        CALL WLOG( 1, '           ' // USEDSETF )
                        CALL ERRLOG( '       Use the same DBE for ' //
     1                     'any given station.' )
                     END IF
                  END IF
               END DO
            END IF
         END DO
C
      END IF
C
C     Check for scans that are excessively long.  Setting 40min as
C     the standard for now.  Such scans put a lot of data at risk if
C     there are issues such as some that we've had with mid-scan disk
C     changes.
C
      WARNLONG = .TRUE.
      DO ISCN = SCAN1, SCANL
         IF( STOPJ(ISCN) - STARTJ(ISCN) .GT. 40.0 / 1440.D0 .AND. 
     1       WARNLONG ) THEN
            CALL WLOG( 1, 'See sched.runlog for warning on long scans.')
            CALL WRTMSG( 0, 'CHKSCN', 'warnlong' )
            WARNLONG = .FALSE.
         END IF
      END DO
C
C     Look for scans that start or, worse, end before very many antennas
C     are on-source.  Allow a one second tolerance on the start to 
C     avoid round off problems.
C
      NSLATE = 0
      DO ISCN = SCAN1, SCANL
         NEXPECT = 0
         NLATE   = 0
         NNEVER  = 0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               NEXPECT = NEXPECT + 1
               IF( TONSRC(ISCN, ISTA) .GT. STARTJ(ISCN) + ONESEC ) 
     1             NLATE = NLATE + 1
               IF( TONSRC(ISCN, ISTA) .GT. STOPJ(ISCN) )
     1             NNEVER = NNEVER + 1
            END IF
         END DO
C
C        Warn of an individual scan with few antennas ever reaching the 
C        source.
C
         IF( NNEVER .GT. NEXPECT/2 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, 2A )' )
     1         'CHKSCN:  WARNING - Scan ', ISCN, ' had fewer than ', 
     2         'half the antennas on source by the stop time!'
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Accumulate the number of scans with most antennas not on source
C        at the start.
C
         IF( NLATE .GT. NEXPECT/2 ) NSLATE = NSLATE + 1
      END DO
C
C     Tell the user about the number of scans with late arrival by most
C     antennas.
C
      IF( NSLATE .GT. 0 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, 2A )' )
     1      'CHKSCN: ',  NSLATE, ' scans had more than half the ',
     2      'antennas arrive on-source after the start time.'
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2A )' )
     1      '              This could be normal if using duration ',
     2      'scheduling with small gaps.'
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     Check some VLA issues.
C        This routine doesn't do anything now.  Perhaps a new one will
C        be needed.  The original routine has been put in 
C        ~/files/sched_ARCHIVE_nonSVN/obsolete_routines/
C
C        CALL VLASCHK
C
C
C     Look for parallel scans meant to be correlated.  Don't bother
C     checking frequencies in detail - just check the range.
C
C     First gather the minimum and maximum frequency in each setup
C     group.  Just loop through the frequency sets and find the max
C     and min for the associated setup group.
C
C     Initialize.
C
      DO KS = 1, NSET
         MINFR(KS) = MINDEF
         MAXFR(KS) = MAXDEF
      END DO
C
C     Get the extrema.
C
      DO KF = 1, NFSET
         KS = FSETKS(KF)
         ISTA = ISCHSTA(ISETSTA(KS))
         CALL FSFREQ( KF, FRSUM, FRBB, FRBW,
     1                CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
         DO ICH = 1, NCHAN(KS)
            IF( NETSIDE(ICH,KS) .EQ. 'U' ) THEN
               FR1 = FRSUM(ICH)
               FR2 = FRSUM(ICH) + FRBW(ICH)
            ELSE
               FR1 = FRSUM(ICH) - FRBW(ICH)
               FR2 = FRSUM(ICH)
            END IF
            MINFR(KS) = MIN( MINFR(KS), FR1 )
            MAXFR(KS) = MAX( MAXFR(KS), FR2 )
         END DO            
      END DO
C
C     Now do the double scan loop looking for matches.
C     Note that we don't need to check the station because other
C     code assures that a station will not have two scans that
C     overlap.
C
      OVWARN = .TRUE.
      DO ISCN = SCAN1 + 1, SCANL
        IF( .NOT. NOREC(ISCN) ) THEN
          DO JSCN = SCAN1, ISCN - 1
            IF( .NOT. NOREC(JSCN) ) THEN
              IF( SCNSRC(ISCN) .EQ. SCNSRC(JSCN) .AND.
     1           STARTJ(ISCN) .LT. STOPJ(JSCN) .AND.
     2           STOPJ(ISCN) .GT. STARTJ(JSCN) ) THEN
                MINFRI = MINDEF
                MAXFRI = MAXDEF
                MINFRJ = MINDEF
                MAXFRJ = MAXDEF
                SKIPI = .TRUE.
                SKIPJ = .TRUE.
                DO ISTA = 1, NSTA
                   KS = NSETUP(ISCN,ISTA)
                   IF( STASCN(ISCN,ISTA) ) THEN
                      SKIPI = .FALSE.
                      MINFRI = MIN( MINFR(KS), MINFRI )
                      MAXFRI = MAX( MAXFR(KS), MAXFRI )
                   END IF
                   IF( STASCN(JSCN,ISTA) ) THEN
                      SKIPJ = .FALSE.
                      MINFRJ = MIN( MINFR(KS), MINFRJ )
                      MAXFRJ = MAX( MAXFR(KS), MAXFRJ )
                   END IF
                END DO               
C
C               Don't get confused by empty scans.  They can have
C               weird properties - like overlapping times.
C
                IF( .NOT. SKIPI .AND. .NOT. SKIPJ ) THEN
C
C                  First complain if frequency groups not found.
C
                   IF( MINFRI .EQ. MINDEF .OR. MAXFRI .EQ. MAXDEF ) THEN
                      MSGTXT = ' '
                      WRITE( MSGTXT, '( A, I5, A )' )
     1                   'CHKSCN: Frequency group not found for scan ',
     2                   ISCN, '.  Should not happen.'
                      CALL ERRLOG( MSGTXT )
                   END IF
                   IF( MINFRJ .EQ. MINDEF .OR. MAXFRJ .EQ. MAXDEF ) THEN
                      MSGTXT = ' '
                      WRITE( MSGTXT, '( A, I5, A )' )
     1                   'CHKSCN: Frequency group not found for scan ',
     2                   JSCN, '.  Should not happen.'
                      CALL ERRLOG( MSGTXT )
                   END IF
C
C                  Now if there are overlapping frequencies, actually
C                  warn about the overlapping scans.
C
                   IF( MINFRI .LT. MAXFRJ .AND. MAXFRI .GT. MINFRJ ) 
     1                 THEN
                     IF( OVWARN ) THEN
                       CALL WRTMSG(  0, 'CHKSCN', 'overlap_scans' )
                       CALL WLOG( 1, 'CHKSCN:  ***** WARNING ****** ' )
                       MSGTXT = ' '
                       WRITE( MSGTXT, '( A, 2I5, A )' )
     1                   '  Two scans (', ISCN, JSCN, 
     3                   ') overlap in source, time, and frequency.'
                       CALL WLOG( 1, MSGTXT )
                       MSGTXT = ' '
                       WRITE( MSGTXT, '( 3A )' )
     1                   '  At VEX driven correlators (others too?) ',
     2                   'antennas in these scans will not be cross ',
     3                   'correlated.'
                       CALL WLOG( 1, MSGTXT )
                       CALL WLOG( 1, 
     1                      '  See sched.runlog for more info.' )
                       OVWARN = .FALSE.
                     ELSE
                       MSGTXT = ' ' 
                       WRITE( MSGTXT, '( A, 2I5 )' )
     1                    '  Additional overlapping scans: ', ISCN, JSCN
                       CALL WLOG( 1, MSGTXT )
                     END IF
                  END IF
                END IF
              END IF
            END IF
          END DO
        END IF
      END DO
C
C     Add phasing INTENTs for stations other than VLA.
C
      CALL PHINT
C
C     Check that VLA phasing scans are at least 4 times VLAPTIME
C     in length.  VLASTA set in VLASCNS.  Establish the PHASING
C     parameter which will ease writing the VLAPTIME based intent
C     in VXSCH.
C
C     Loop through scans to be sure the VLA phasing time is appropriate.
C     Use less indention here because of deep nest and long lines.
C     First be sure the VLA is in the observation and that phasing is
C     requested.  VLASCNS took care of adding INTENTS when VLAMODE
C     is used.
C
C     While at it, this is a convenient place to throw a fit if the
C     user is trying to phase and reference point at the same time.
C
      IF( VLASTA .NE. 0 ) THEN
       DO ISCN = SCAN1, SCANL
        PHASING(ISCN) = .FALSE.
        VLAPTG = .FALSE.
        IF( STASCN(ISCN,VLASTA) ) THEN
          IF( NSCINT(ISCN) .NE. 0 ) THEN
            DO IINT = 1, NSCINT(ISCN)
              IF( INDEX( INTENT(ISCINT(IINT,ISCN)),
     1           'AUTOPHASE_DETERMINE' ) .NE. 0 .AND.
     2         ( INDEX( INTENT(ISCINT(IINT,ISCN)), ':' ) .EQ. 0 .OR.
     3           INDEX( INTENT(ISCINT(IINT,ISCN)), 'VLA' ) .NE. 0 ) )
     4              THEN
                PHASING(ISCN) = .TRUE. 
              END IF
              IF( INDEX( INTENT(ISCINT(IINT,ISCN)),
     1           'REFERENCE_POINTING_DETERMINE' ) .NE. 0 .AND.
     2         ( INDEX( INTENT(ISCINT(IINT,ISCN)), ':' ) .EQ. 0 .OR.
     3           INDEX( INTENT(ISCINT(IINT,ISCN)), 'VLA' ) .NE. 0 ) )
     4              THEN
                VLAPTG = .TRUE. 
              END IF
            END DO
          END IF
C
C         If it's a phasing scan, complain if it is also a pointing scan.
C         If not, then check the length.
C
          IF( PHASING(ISCN) ) THEN  
             IF( VLAPTG ) THEN
                MSGTXT = ' '
                WRITE(MSGTXT, '( 2A, I5 )' )
     1             'CHKSCN:  VLA reference pointing AND phasing ',
     2             'requested in scan ', ISCN
                CALL WLOG( 1, MSGTXT )
                CALL WLOG( 1, '         That is not possible.' )
                CALL ERRLOG( 
     1              'Make separate pointing and phasing scans.' )
             END IF
C
             VPT = DBLE( VLAPTIME(ISCN) ) * ONESEC 
             SCANLEN = STOPJ(ISCN) - 
     1            MAX ( ( STARTJ(ISCN) - TPSTART(ISCN,VLASTA) ), 
     2                    TONSRC(ISCN,VLASTA) )
             IF( SCANLEN / VPT .LT. 4.D0 ) THEN
                MSGTXT = ' ' 
                WRITE( MSGTXT, '( 2A )' )
     1             'CHKSCN:  ** VLA phasing scan must be at least 4 ',
     2             'times longer than the phasing time from VLAPTIME.'
                CALL WLOG( 1, MSGTXT )
                MSGTXT = ' '
                WRITE( MSGTXT, '( A, I5, 3A, F8.1, A, F8.1 )' )
     1             '         Please fix scan', ISCN, ' on ',
     2             SCNSRC(ISCN), ' Scan length:', SCANLEN / ONESEC,
     3             '  VLAPTIME:',  VPT / ONESEC
                CALL ERRLOG( MSGTXT )
             END IF
          END IF
        END IF
       END DO
      END IF
C
C
      RETURN
      END
