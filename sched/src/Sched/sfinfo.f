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
C
         DO KS = NSET, 1, -1
            ISETF = ISETNUM(KS)
            IF( NCHAN(KS) .EQ. MSCHN(ISETF) .AND. 
     1          ( SGDUAL(KS) .EQV. SFDUAL(ISETF) ) ) THEN
               DO ICHN = 1, NCHAN(KS)
                  SFFREQ(ICHN,ISETF) = FREQREF(ICHN,KS)
                  SFPOL(ICHN,ISETF) = POL(ICHN,KS)
                  SFSIDE(ICHN,ISETF) = NETSIDE(ICHN,KS)
               END DO
            END IF
         END DO
C
C        Now relate the channels in each group to the logical channels.
C        If any group has a channel that does not correspond to a 
C        logical channel, set OKXC(ISETF) to .FALSE..  This will 
C        block a number of tests and features later.
C
C        SFCHAN is the pointer to the setup file logical channel for
C        each setup group channel for this setup group.
C        SGCHAN is the pointer to the setup group channel for each
C        setup file logical channel.
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
                  IF( ABS( FREQREF(ICHN,KS) - SFFREQ(KCHN,ISETF) ) .LT.
     1                 0.05D0 * BBFILT(ICHN,KS) .AND.
     2                POL(ICHN,KS) .EQ. SFPOL(KCHN,ISETF) .AND.
     3                NETSIDE(ICHN,KS) .EQ. SFSIDE(KCHN,ISETF) ) THEN
C
                     SFCHAN(ICHN,KS) = KCHN
                     SGCHAN(KCHN,KS) = ICHN
C   
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
               CALL WLOG( 1, '    The setup file:' )
               CALL WLOG( 1, '    ' //
     1              SETFILE(ISETF)(1:LEN1(SETFILE(ISETF))) )
               CALL WLOG( 1, '    has unmatched channels at '//
     1              'different stations.' )
               CALL WLOG( 1, '    Is this intentional?' )
               CALL WLOG( 1, '    Some checks will be disabled.' )
               CALL WLOG( 1, '    DOPPLER, FREQ, and BW will be '//
     1               'disabled for this setup.' )
            END IF
C
            IF( DUP(ISETF) ) THEN
               CALL WLOG( 1, 
     1               'SFINFO:  **** WARNING ****' )
               CALL WLOG( 1, '    The setup file:' )
               CALL WLOG( 1, '    ' //
     1              SETFILE(ISETF)(1:LEN1(SETFILE(ISETF))) )
               CALL WLOG( 1, '    has duplicate channels.  ' //
     1                      '  Is this intentional?' )
               CALL WLOG( 1, '    This messes up SCHED internal ' //
     1               'bookkeeping.  As a result,' )
               CALL WLOG( 1, '    DOPPLER, FREQ, and BW will be '//
     1               'disabled for scans using this setup.' )
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







