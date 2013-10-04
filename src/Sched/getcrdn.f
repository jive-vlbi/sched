      SUBROUTINE GETCRDN( ISCN, ISTA, CRDN, CR1, CRN )
C
C     Subroutine used in writing the VLBA crd files that determines
C     the number of channels used in the legacy hardware.  This
C     will only differ from the setup file value when the the
C     legacy hardware is not being used to produce the VLBI data,
C     which is the case when the RDBE is in use.  The value can differ
C     from the setup file number when a user has specified the
C     CRD parameters (CRDNCH, CRDFREQ, CRDDOP - usually for reference
C     pointing) or when the setup has too many channels for the BBCs
C     (normal when using the PFB personality).
C
C     This routine returns a number of channels, and the range
C     of the setup channels from which to get many of the channel 
C     properties, such as IF, polarization, receiver, and so forth.
C     The frequency and bandwidth may come from the CRD inputs or from
C     efforts to adjust them to match the capabilities of the legacy 
C     hardware.  They may differ from the setup file values.
C     See FSFREQ for details on how this is done.
C
C     Note that CR1 and CRN are meant to apply to the setup file
C     channels.  CRDN is just CRN-CR1+1 and is the number of legacy
C     system channels specified in the crd files.  Care must be 
C     taken to use the right indexing when dealing with setup 
C     file parameters or crd file output.
C
C     This routine will be called by both VLBASU and FSFREQ so be
C     sure nothing is done that would create problems when called 
C     multiple times.
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
      INTEGER    ISCN, KSTA, KSCN, CRDN, CR1, CRN, ISTA
      INTEGER    KS, KF
C-----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GECRDN: starting' )
C
C     Get the station catalog index for schedule station ISTA.
C     Get the frequency set.
C
      KSTA = STANUM(ISTA)
      KS = NSETUP(ISCN,ISTA)
      KF = FSETI(ISCN,ISTA)
      KSCN = FSETSCN(KF)
C      
C     First the case where the CRD parameters should match the setup.
C     This is when the RDBE is not in use or the station is does not
C     use the legacy VLBA control system.  In fact, for non-VLBA or 
C     non VLBA control system stations, the routine probably will 
C     not even be called, but cover the case anywhere.
C
      IF( DAR(KSTA)(1:4) .NE. 'RDBE' .OR. 
     1   ( CONTROL(KSTA) .NE. 'VLBA' .AND. .NOT. VLBADAR(KSTA) ) ) THEN
         CRDN = NCHAN(KS)
         CR1 = 1
         CRN = CRDN
      ELSE
C
C        Ok, we are going to need crd parameters.  For the number
C        of channels, all that matters is if CRDNCH was specified.
C        differences over whether CRDFREQ or CRDDOP were specified
C        don't impact here.  Recall that CRDNCH has been restricted
C        to 8 or less by INFDB so that we can use a BBC per channel
C        when not using the original setup values.  That avoids
C        a mess under some circumstances when both sidebands of a
C        BBC were used in the setup.  But, of course, the crd parameters
C        like CRDNCH are not used except when the RDBE is in use and
C        that device does not have such pairs.
C
         IF( CRDNCH(KSCN) .GT. 0 ) THEN
C
C           Use the specified number of channels.  Check the total.
C
            CRDN = CRDNCH(KSCN)
            CR1 = CRDCH1(KSCN)
            CRN = CR1 + CRDN - 1
            IF( CRN .GT. NCHAN(KS) ) THEN
               CALL WLOG( 1, 'GETCRDN: The specified CRDNCH and ' //
     1            'CRDCH1 assume more channels than are in the' )
               MSGTXT = ' '
               WRITE( MSGTXT, '(  A, A )' ) 
     1               '         setup file: ', SETNAME(KS)
               CALL WLOG( 1, MSGTXT )
            END IF
         ELSE
C
C           Set the values when using the default.  This is when the 
C           user did not set CRDNCH.  Note that CRDCH1 cannot be set
C           without setting CRDNCH - enforced by INFDB.
C
C           For the PFB, use the center 8 of the 16 channels.
C           Otherwise use the first channels out to 8 (number of
C           BBCs) or, if smaller, the number of setup channels.
C           Note that we are using only 8 channels when the BBCs
C           can actually do 16 with dual sidebands to avoid 
C           lots of programming complexity to be sure the opposite
C           sidebands are set in a possible manner.
C
            IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
               CRDN = 8
               CR1 = 5
               CRN = CR1 + CRDN - 1
            ELSE 
               CRDN = MIN( 8, NCHAN(KS) )
               CR1 = 1
               CRN = CRDN
            END IF
         END IF
      END IF
      RETURN
      END
