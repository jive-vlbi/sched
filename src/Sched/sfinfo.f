      SUBROUTINE SFINFO
C
C     Routine for SCHED called by SETDEFS that gets information about
C     the logical channels of a setup file.  It has to look through
C     the setup groups to do so.
C
C     The logical channels deal with the possibility that some stations
C     only record a subset of the setup recorded at other stations.  
C     Possible examples include use of a single polarization at a 
C     station in a dual polarization observation, or use of smaller 
C     number of BBCs at some stations than at others.  It is assumed 
C     that at least one station has all channels and that there are not
C     two setups with the same number of channels and dual polarization
C     (there can be setups with the same number of channels where on
C     is single polarization).
C    
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER       ISETF, KS, ICHN, KCHN, LEN1
      LOGICAL       SFDUAL(MAXSET), SGDUAL(MSET), DUP(MAXSET)
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SFINFO: Starting.' )
C
      IF( NSETF .GE. 1 ) THEN
         DO ISETF = 1, NSETF
            OKXC(ISETF) = .TRUE.
            DO ICHN = 1, MAXCHN
               SFFREQ(1,ISETF) = 0.D0
            END DO
         END DO
      END IF
C
C     Don't bother if doing VLA only.
C
      IF( .NOT. VLAONLY ) THEN
C
C        Initialize.
C
         DO ISETF = 1, NSETF
            SFDUAL(ISETF) = .FALSE.
            DUP(ISETF) = .FALSE.
         END DO
C
C        See if any stations in each setup file use dual polarization.
C        Also do some initialization.  I'm not looking for polarization
C        pairs - just the presence of 2 polarizations.  In such a case,
C        some pair of channels will have opposite polarizations.
C
         DO KS = 1, NSET
            ISETF = ISETNUM(KS)
            SGDUAL(KS) = .FALSE.
            IF( NCHAN(KS) .GE. 2 ) THEN
               DO ICHN = 2, NCHAN(KS)
                  IF( POL(ICHN,KS) .NE. POL(ICHN-1,KS) ) THEN
                     SGDUAL(KS) = .TRUE.
                     SFDUAL(ISETF) = .TRUE.
                  END IF
               END DO
            END IF
         END DO
C
C        Find a "complete" group and establish the "logical" channels.  
C        Want a group with the to the maximum number of channels and,
C        if any groups have it, with dual polarization.  Bias toward 
C        saving the first group of each file by going backwards.
C        Buck out any correlator inversions that might be expected
C        (currently only set in CHKRDBE).
C
         DO KS = NSET, 1, -1
            ISETF = ISETNUM(KS)
            IF( NCHAN(KS) .EQ. MSCHN(ISETF) .AND. 
     1          ( SGDUAL(KS) .EQV. SFDUAL(ISETF) ) ) THEN
               DO ICHN = 1, NCHAN(KS)
                  SFFREQ(ICHN,ISETF) = FREQREF(ICHN,KS) -
     1                                 CORINV(ICHN,KS)
                  SFFILT(ICHN,ISETF) = BBFILT(ICHN,KS)
                  SFPOL(ICHN,ISETF) = POL(ICHN,KS)
                  IF( CORINV(ICHN,KS) .NE. 0.D0 ) THEN
                     IF( NETSIDE(ICHN,KS) .EQ. 'L' ) THEN
                        SFSIDE(ICHN,ISETF) = 'U'
                     ELSE
                        SFSIDE(ICHN,ISETF) = 'L'
                     END IF
                  ELSE
                     SFSIDE(ICHN,ISETF) = NETSIDE(ICHN,KS)
                  END IF
               END DO
            END IF
         END DO
C
C        Now relate the channels in each group to the logical channels.
C        If any group has a channel that does not correspond to a 
C        logical channel, set OKXC(ISETF) to .FALSE..  This will 
C        block a number of tests and features later.
C
C        The following pointers are associated with each setup group.
C        SFCHAN.  For each channel in this setup group, this points to
C           the logical channel of the setup file.
C        SGCHAN.  For this setup group, this points to the channel
C           in the setup group associated with indexed setup file logical
C           channel.
C
C        Again, deal with correlator inversions flagged by CORINV.
C        Assume that NETSIDE and SFSIDE will be either 'L' or 'U' 
C        and nothing else so you can check for opposite sidebands
C        by just checking that they are not equal.  CORINV is the
C        amount that got added to FREQREF to get the LO setting.
C        For comparison with channels that will get correlated against
C        this one, subtract CORINV.
C
         DO KS = 1, NSET
            ISETF = ISETNUM(KS)
C
            DO ICHN = 1, MCHAN
               SFCHAN(ICHN,KS) = 0
               SGCHAN(ICHN,KS) = 0
            END DO
C
            DO ICHN = 1, NCHAN(KS)
               DO KCHN = 1, MSCHN(ISETF)
C
C                 Match center frequency, bandwidth, and polarization.
C
                  IF( ABS( FREQREF(ICHN,KS) - CORINV(ICHN,KS) - 
     1               SFFREQ(KCHN,ISETF) ) .LT. 0.05D0 * BBFILT(ICHN,KS) 
     2               .AND. SFFILT(ICHN,ISETF) .EQ. BBFILT(ICHN,KS)
     3               .AND. POL(ICHN,KS) .EQ. SFPOL(KCHN,ISETF) ) THEN
C
C                    Match sideband, allowing for correlator inversion.
C
                     IF(  ( CORINV(ICHN,KS) .EQ. 0.D0 .AND. 
     1                 NETSIDE(ICHN,KS) .EQ. SFSIDE(KCHN,ISETF) ) .OR.
     2                 ( CORINV(ICHN,KS) .NE. 0.D0 .AND. 
     3                 (NETSIDE(ICHN,KS) .NE. SFSIDE(KCHN,ISETF) ) ) )
     4                    THEN
C
                        SFCHAN(ICHN,KS) = KCHN
                        SGCHAN(KCHN,KS) = ICHN
C   
                     END IF
                  END IF
               END DO
            END DO         
         END DO
C
C        Determine if all channels of all groups in a file are accounted
C        for in the "logical" channels.  Also get upset if some channels
C        are duplicates.
C
         DO KS = 1, NSET
            ISETF = ISETNUM(KS)
            DO ICHN = 1, NCHAN(KS)
               IF( SFCHAN(ICHN,KS) .EQ. 0 ) THEN
C              write(*,*) 'sfinfo set okxc F ', ks, isetf, ichn, 
C     1           sfchan(ichn,ks), freqref(ichn,ks), corinv(ichn,ks),
C     2           sffreq(ichn,isetf)
                  OKXC(ISETF) = .FALSE.
               END IF
               IF( ICHN .GE. 2 ) THEN
                  DO KCHN = 1, ICHN - 1
                     IF( SFCHAN(ICHN,KS) .EQ. SFCHAN(KCHN,KS) .AND.
     1                   SFCHAN(ICHN,KS) .NE. 0 ) THEN
                        DUP(ISETF) = .TRUE.
                     END IF
                  END DO
               END IF
            END DO
         END DO
C
C        Warn the user if there are missmatched channels.
C
         DO ISETF = 1, NSETF
            IF( .NOT. OKXC(ISETF) ) THEN
               CALL WLOG( 1, 
     1             'SFINFO:  **** WARNING ****' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' )
     1            '    The setup file:',
     2            SETFILE(ISETF)(1:LEN1(SETFILE(ISETF))),
     3            '  has unmatched channels at different stations.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' ) 
     1           '    Is this intentional?', 
     2           ' Some checks will be disabled.'
               CALL WLOG( 1, '    DOPPLER, FREQ, and BW scan '//
     1               'inputs will be disabled for this setup.' )
            END IF
C
            IF( DUP(ISETF) ) THEN
               CALL WLOG( 1, 
     1               'SFINFO:  **** WARNING ****' )
               WRITE( MSGTXT, '( A, A, A )' )
     1            '    The setup file:',
     2            SETFILE(ISETF)(1:LEN1(SETFILE(ISETF))),
     3            '  has duplicate channels.'
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '      (ie, Freqency match to 5% of BW. '
     1             // '  Other properties match.)' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' ) 
     1             '    Is this intentional?  ', 
     2             'This messes up SCHED internal bookkeeping'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               CALL WLOG( 1, '    As a result, DOPPLER, FREQ, and BW '//
     1              'will be disabled for scans using this setup.' )
               CALL WLOG( 1, '    If those are not needed, the '//
     1              'schedule should be ok.' )
C
C              Flag the bookkeeping as bad for later.
C
               OKXC(ISETF) = .FALSE.
            END IF
        END DO
      END IF
C
      RETURN
      END
