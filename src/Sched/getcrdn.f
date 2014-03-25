      SUBROUTINE GETCRDN( ISCN, ISTA, CRDN, CRSETC )
C
C     Subroutine used in writing the VLBA crd files that determines
C     how many legacy system baseband channels to use when recording 
C     through the RDBE and which RDBE channels to use for the basic
C     setup information such as band, polarization, IF etc.
C     the number of channels used in the legacy hardware.  The routine
C     is required because there can be more RDBE channels than legacy
C     channels, especially since the number of BBCs was drawn down to
C     four (early 2014).
C
C     The results can be controlled by the user, usually for reference
C     pointing, using CRDNCH (often used with CRDFREQ or CRDDOP),
C     CRDSETCH, CRDCH1.  More commonly, this routine will set up 
C     default values.
C     
C     The defaults will sample all IFs in use.  This mainly matters
C     when the S/X system is in use and some observations were lost before
C     this was set carefully.  If the legacy system thinks it is X only,
C     which happened with the RDBE_PFB and only 4 S band channels, it will
C     aim the subreflector at the X band receiver.  But the new system
C     knows about all channels and deploys the ellipsoid.  Thus there
C     are no S band fringes because the subreflector is aimed at X band, 
C     and there are no X band fringes because the ellipsoid is blocking
C     the optical path.  The diagnosis is hard because the data all look
C     reasonable.
C
C     This routine is called by VLBASU, FSFREQ (which is called many
c     times), and PCALFQ so be sure nothing is done that would create 
C     problems when called multiple times.
C
C     Input: 
C        ISCN:    Current scan number.  Often called with the refernece
C                 scan of a frequency set.
C        ISTA:    Schedule station number for the station.
C                 Note in some routines, the catalog number would be more
C                 natural, but we need ISTA for NSETUP etc.
C     Output:
C        CRDN:    Then number of channels for the crd files.
C        CR1:     The setup channel to use for the first crd channel
C                 parameters that are the same in the RDBE and legacy system.
C        CRN:     The setup channel to use for the last crd channel.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER    MIF
      PARAMETER  (MIF=4)
      INTEGER    ISCN, KSTA, CRDN, CRSETC(*), ISTA
      INTEGER    ICH, IIF, ICHCR, ICRD, I, J, K, ITEMP
      INTEGER    KS, NIF, NCHIF(MIF), CHIF(MAXCHN,MIF)
      INTEGER    NCRDIF(MIF), CRDBBC(MAXCHN,MIF), IFCHI(MAXCHN)
      CHARACTER  NAMIF(MIF)*2
      REAL       RINCR
      LOGICAL    CRDEBUG
      DATA       CRDEBUG / .FALSE. /
C-----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GETCRDN: starting' )
C
C     Get the station catalog index for schedule station ISTA and
C     the setup group.  Note that the frequency sets are not yet known when
C     this is called from DOPCRD, so don't invoke them.
C
      KSTA = STANUM(ISTA)
      KS = NSETUP(ISCN,ISTA)
C      
C     First deal with some of the cases where the CRD parameters should 
C     match the setup.
C
C     For VLBA stations, this is when the RDBE is not in use (DAR not RDBE)
C     and the crd files are actually setting up the experiment.
C
C     Otherwise use this in the cases where no crd file will be written.
C     See CRDWRT for the IF statement there.  VLBADAR is probably obsolete -
C     see routine STREAD.
C
      IF( DAR(KSTA)(1:4) .NE. 'RDBE' .OR. .NOT. (
     1   ( CONTROL(STANUM(ISTA)) .EQ. 'VLBA' .OR. 
     2      VLBADAR(STANUM(ISTA)) ) ) )  THEN
         CRDN = NCHAN(KS)
         DO ICHCR = 1, NCHAN(KS)
            CRSETC(ICHCR) = ICHCR
         END DO
      ELSE
C
C        Ok, we are going to need crd parameters but SCHED is focusing
C        on setting up the RDBE.   In other words, this is not primarly 
C        a legacy (MARK5A or pointing) schedule.  
C
C        For the number of channels, use CRDNCH if it was given.
C        INFDB did not let it be greater than MAXCRD, which is 4 after
C        Feb 2014 because some BBCs of the original 8 are being
C        removed.  We will only be allocating channel per BBC, even
C        though more are allowed for MARK5A/pointing schedules.
C
C        Note that the frequency or doppler settings don't matter here.
C
         IF( CRDNCH(ISCN) .GT. 0 ) THEN
C
C           Use the specified number of channels.  Check the total.
C           Use current scan, not setup reference scan as the CRD
C           parameters can change.
C
            CRDN = CRDNCH(ISCN)
            IF( CRDN .GT. NCHAN(KS) ) THEN
               CALL WLOG( 1, 'GETCRDN: The specified CRDNCH assumes' //
     1            ' more channels than are in the setup file: ' )
               MSGTXT = ' '
               WRITE( MSGTXT, '(  A, A )' ) 
     1               '         ', SETNAME(KS)
               CALL ERRLOG( MSGTXT )
            END IF
         ELSE
C
C           Set the values when using the default.  This is when the 
C           user did not set CRDNCH.  Note that CRDCH1 cannot be set
C           without setting CRDNCH - enforced by INFDB.
C
            CRDN = MIN( MAXCRD, NCHAN(KS) )
         END IF
C
C        Now set the setup file channels to specify the channels to use 
C        for basic information such as IF name etc.
C
C        First if CRDCH1 or CRDSETCH was used, CRDSETCH will be filled
C        out in INFDB.  But protect against invalid values now that the
C        setups are well established.
C
         IF( CRDSETCH(1,ISCN) .GT. 0 ) THEN
            DO ICHCR = 1, CRDN
               CRSETC(ICHCR) = CRDSETCH(ICHCR,ISCN) 
               IF( CRSETC(ICHCR) .LT. 1 .OR. 
     1             CRSETC(ICHCR) .GT. NCHAN(KS) ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2, A, I3 )' )
     1               'GETCRDN:  CRDSETCH(', ICHCR, ') = ', 
     2               CRDSETCH(ICHCR,ISCN), 
     3               ' not between 1 and the number of channels', 
     4               NCHAN(KS)
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2, A, I3 )' )
     1               '          for scan ', ISCN, ' setup: ',
     2               SETNAME(KS)
                  CALL WLOG( 1, MSGTXT )
                  IF( CRDCH1(ISCN) .GT. 0 ) THEN
                     CALL WLOG( 1, 
     1               '          Note CRDSETCH was derived '//
     2               'from CRDCH1 and CRDCHN' )
                  END IF
                  CALL ERRLOG( 'Fix the channel request.' )
               END IF
            END DO
C
         ELSE
C
C           Channels not specified.  Set the defaults.
C           Try to put at least one channel in each IF.  Also
C           spread the channels reasonably far apart if 
C           possible.
C
C           Set up a list of IF channels to spread the CRD channels over.
C           Get the following from the setup information.
C             NIF           - number of IF channels
C             NCHIF(IIF)   - number of baseband channels per IF channel
C             NAMIF(IIF)   - name of each IF channel
C             IFCHI(ICH)    - IF number for setup baseband channel
C             CHIF(ICF,IIF) - List of setup BB channels for each IF
C
C           There is a spreadsheet sched_4bbc.ods that I used to help
C           think this through.
C             
C
            NIF = 0
            DO IIF = 1, MIF
               NAMIF(IIF) = ' '
               NCHIF(IIF) = 0
               DO ICH = 1, NCHAN(KS)
                  CHIF(ICH,IIF) = 0
               END DO
            END DO
            DO ICH = 1, NCHAN(KS)
               IFCHI(ICH) = 0
            END DO
C
C           See what IFs are there and collect the list of setup BB
C           channels that are use that IF.  Get started with the
C           first, then look through the rest behaving differently
C           for IFs previously seen or new.
C
            NIF = 1
            NAMIF(1)  = IFCHAN(1,KS)
            NCHIF(1)  = 1
            CHIF(1,1) = 1
            IFCHI(1)  = 1
C
C           Loop over the other channels, if there are any.
C
            IF( NCHAN(KS) .GE. 2 ) THEN
               DO ICH = 2, NCHAN(KS)
                  DO IIF = 1, NIF
                     IF( IFCHAN(ICH,KS) .EQ. NAMIF(IIF) ) THEN
C
C                       Matches a channel seen before.
C
                        NCHIF(IIF) = NCHIF(IIF) + 1
                        CHIF(NCHIF(IIF),IIF) = ICH
                        IFCHI(ICH) = IIF
C
                     END IF
                  END DO
C
C                 Deal with a new IF.  But if the number of IFs goes 
C                 over the max, don't add it as we won't be able to 
C                 use it.  Later, if IFCHI is still 0, it had an IF 
C                 that was not counted.  For the VLBA, that should 
C                 never be an issue as there are only 4 IFs possible.
C
                  IF( IFCHI(ICH) .EQ. 0 .AND. NIF .LT. MIF ) THEN
                     NIF = NIF + 1
                     NAMIF(NIF) = IFCHAN(ICH,KS)
                     NCHIF(NIF) = 1
                     CHIF(NCHIF(NIF),NIF) = ICH
                     IFCHI(ICH) = NIF
                  END IF
               END DO
            END IF
C
C           Get the number of crd channels to allocate to each IF.
C           This loops through the crd channels assigning an IF to
C           each.  Here it steps through the available IFs as often
C           as necessary to allocate an IF to all crd channels.
C           Don't allocate more channels to an IF than there are 
C           setup channels that have that IF.
C           
            DO IIF = 1, NIF
               NCRDIF(IIF) = 0
            END DO
C
            IIF = 0
            ICHCR = 0
            I = 0
            DO WHILE ( ICHCR .LT. CRDN )
               I = I + 1
               IIF = MOD( I, NIF ) + 1
               IF( NCRDIF(IIF) .LT. NCHIF(IIF) ) THEN
                  NCRDIF(IIF) = NCRDIF(IIF) + 1
                  ICHCR = ICHCR + 1
               END IF
               IF( I .GT. MIF * MAXCHN ) CALL ERRLOG( 'GETCRDN:' //
     1            ' Programming error in IF assignments. ' //
     2            'Please report.' )
            END DO               
C
C           Now we have the number of setup BB channels per IF (NCHIF)
C           and the number of CRD channels per IF (NCRDIF).
C
C           Get which of the BB channels (CHIF) to keep.  Evenly space
C           the ones kept within the list.  To get the increment, you
C           want to divide the number of setup channels in the IF
C           by the number you can keep.  But you want the kept ones
C           centered in the list.  That is best done by using an
C           increment that is the number of channels in either case
C           plus 1 to account for the upper end that you don't want
C           to hit.  Getting this right is what the spreadsheet was
C           for.
C
            ICRD = 0
            DO IIF = 1, NIF
               RINCR = REAL( NCHIF(IIF) + 1 ) / 
     1                 REAL( NCRDIF(IIF) + 1 )
               DO J = 1, NCRDIF(IIF)
                  K = NINT( J * RINCR )
                  CRDBBC(J,IIF) = CHIF(K,IIF)
                  ICRD = ICRD + 1
                  CRSETC(ICRD) = CRDBBC(J,IIF)
               END DO
            END DO

C
C           Sanity check.  ICRD should equal CRDN.
C
            IF( ICRD .NE. CRDN ) THEN
               CALL WLOG( 1, 'GETCRDN:  Programming problem: '//
     1              'Number of CRD channels assigned not right.' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, A, I5 )' )
     1            '    CRDN = ', CRDN, '  ICRD = ', ICRD 
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, A, 16I4 )' )
     1            '    CRSETC = ', (CRSETC(I),I=1,ICRD)
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( '    Please report this problem' )
            END IF
C
C           Sort the CRSETC list so that the CRD channels come
C           in the same order.
C
            IF( CRDN .GT. 1 ) THEN
               DO I = 1, CRDN - 1
                  DO J = I + 1, CRDN
                     IF( CRSETC(J) .LT. CRSETC(I) ) THEN
                        ITEMP = CRSETC(I)
                        CRSETC(I) = CRSETC(J)
                        CRSETC(J) = ITEMP
                     END IF
                  END DO
               END DO
            END IF           
C
C           So the list is made.
C           I'd be tempted to write the results, but it happens
C           every scan.  Do so if CRDEBUG is on.
C
            IF( CRDEBUG ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, I4, I3, A, 16I5 )' ) 
     1             'GETCRDN:  ISCN, KS, CRDN: ', ISCN, KS, CRDN,
     2             '  CRSETC: ', (CRSETC(I),I=1,CRDN)
               CALL WLOG( 1, MSGTXT )
            END IF
C
         END IF
      END IF
C
C     Make sure the highest numbered CRSETC is not
C     to high.
C
      IF( CRSETC(CRDN) .GT. NCHAN(KS) ) THEN
         CALL WLOG( 1, 'GETCRDN: The specified CRD parameters ' //
     1      'assume more channels than are in the' )
         MSGTXT = ' '
         WRITE( MSGTXT, '(  A, A )' ) 
     1         '         setup file: ', SETNAME(KS)
         CALL WLOG( 1, MSGTXT )
      END IF
C
      IF( DEBUG ) CALL WLOG( 0, 'GETCRDN: ending' )
      RETURN
      END
