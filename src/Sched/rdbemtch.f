      SUBROUTINE RDBEMTCH( KS, KF, ICH, IIF, RFIF1, RFIF2, 
     1        RFRQ1, RFRQ2, RCHOVER )
C
C     Routine called by FMATCH to penalize a frequency file entry if 
C     there are crossover problems.  At the time of the call, 
C     FREQREF, BBFILT, and NETSIDE are known for each channel.  
C     Other parameter might not have been set yet.
C
C     KS is the setup group.
C     KF is the frequency catalog group.
C     ICH is the baseband channel number.
C     IIF is the frequency file entry IF number.
C     RFIF1 and RFIF2 are lower and upper RF frequencies for the
C     frequency catalog entry.
C     RFRQ1 and RFRQ2 are lower and upper RF frequencies for the
C     baseband channel.  They don't appear to be used.  
C     RCHOVER was set in FMATCH without knowledge of crossover
C     frequencies.  It is reduced here by the amount of data
C     on the far side of a crossover from FREQREF.
C
C     BBCFR1 and BBCFR2 are the baseband frequencies of the LO 
C     edge (FREQREF) and other edge BBC frequencies for the
C     baseband channel. 
C
C     BBIF1 and BBIF2 are the baseband frequencies corresponding
C     to the edges of the RF band (RFIF1 and RFIF2)
C
C     BBCFR1 
C
C     CROSS are the baseband frequencies of the crossovers.
C
C     Note this routine tries to work even when the RF range is
C     not the usual 512 MHz for the RDBE.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER           KS, KF, IIF, ICH, IC
      INTEGER           IFSIDE, BBSIDE, NCROSS, LEN1
      REAL              RCHOVER, RCHOIN
      DOUBLE PRECISION  RFIF1, RFIF2, BBIF1, BBIF2
      DOUBLE PRECISION  RFRQ1, RFRQ2, BBCFR1, BBCFR2

      INTEGER           MCROSS
      PARAMETER         (MCROSS=16)
      DOUBLE PRECISION  CROSS(MCROSS)
      CHARACTER         LDBE*8
      SAVE              LDBE, CROSS, NCROSS
      DATA              LDBE / '    ' /
C ----------------------------------------------------------------------
C
C     Set the crossover points if needed.  These are fixed for each
C     personality.
C
      IF( DBE(KS) .NE. LDBE ) THEN
         IF( DBE(KS) .EQ. 'RDBE_DDC' ) THEN
            NCROSS = 2
            DO IC = 1, NCROSS
               CROSS(IC) = 640 + ( IC - 1 ) * 256
            END DO
         ELSE IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
            NCROSS = 15
            DO IC = 1, NCROSS
               CROSS(IC) = 528.D0 + ( IC - 1 ) * 32.D0
            END DO
         END IF
      END IF
      LDBE = DBE(KS)
C
C     Get the BBCFREQ and IF sideband with this frequency set and
C     channel, assuming the LO of this frequency entry.  Also get
C     to   <apparent projectus interruptus - what was the meant to be?>
C
C     This is called before potential sideband inversion (CORINV -
C     used when RF sideband can't be matched with the PFB)
C     or using FREQ or DOPPLER on a scan basis.  Don't worry 
C     about that now.  The choice of frequency file group will 
C     likely be ok.  If necessary, the user can force the choice
C     of frequency information with FIRSTLO. IFSIDE is the IF sideband.
C     while BBSIDE is the baseband sideband.
C     The BBFRADn values will be the lower and upper IF values
C     corresponding the the freq catalog entry RF range.  They
C     will be in inverse order relative to FRADn for lower IF
C     sideband.
C
      IF( FREQREF(ICH,KS) .GT. FLO1(IIF,KF) ) THEN
         IFSIDE = 1
         BBCFR1 = FREQREF(ICH,KS) - FLO1(IIF,KF)
         BBIF1  = RFIF1 - FLO1(IIF,KF)
         BBIF2  = RFIF2 - FLO1(IIF,KF)
      ELSE
         IFSIDE = -1
         BBCFR1 = FLO1(IIF,KF) - FREQREF(ICH,KS)
         BBIF1  = FLO1(IIF,KF) - RFIF2
         BBIF2  = FLO1(IIF,KF) - RFIF1
      END IF
C
      IF( ( NETSIDE(ICH,KS) .EQ. 'U' .AND. IFSIDE .EQ. 1 ) .OR.
     1    ( NETSIDE(ICH,KS) .EQ. 'L' .AND. IFSIDE .EQ. -1 ) ) THEN
         BBCFR2 = BBCFR1 + BBFILT(ICH,KS)
         BBSIDE = 1
      ELSE 
         BBCFR2 = BBCFR1 - BBFILT(ICH,KS)
         BBSIDE = -1
      END IF
C
C     Subtract the part lost to the crossovers in the RDBE_DDC.
C     Only bother when the crossover is within both the 
C     frequency window and the channel band.  Otherwise
C     CHOVER will not be affected.  Recall that any lost to the IF
C     edges is already accounted for.
C
C     Actually this is calculated for the PFB too, but the frequencies
C     are not allowed (elsewhere) to be set to values that would span
C     a crossover.  RCHOIN related to the debug printout.
C
      RCHOIN = RCHOVER
      DO IC = 1, NCROSS
C
C        First check that the crossover is in the IF.
C
         IF( CROSS(IC) .GT. BBIF1 .AND. 
     1       CROSS(IC) .LE. BBIF2 ) THEN
C
C           Now, if the crossover is in the baseband, 
C           get the amount of the baseband spectrum that is on 
C           the far side of the crossover from the LO sum.  This
C           part will be lost so add it to RCHOVER.
C           The different sidebands need to be treated slightly
C           differently because the side of the crossover that 
C           will be kept is at the LO sum band edge.  The IF
C           edges get into the act here for those cases where the
C           IF is limited to less than the expected 512 MHz and
C           part of the frequency loss has already entered RCHOVER
C           through the CHOVER calculation in FMATCH.
C
            IF( BBSIDE .EQ. 1 .AND. ( CROSS(IC) .GT. BBCFR1 .AND. 
     1          CROSS(IC) .LE. BBCFR2 ) ) THEN
               RCHOVER = RCHOVER - 
     1            ( MIN( BBIF2, BBCFR2 ) - CROSS(IC) )
            ELSE IF( BBSIDE .EQ. -1 .AND. ( CROSS(IC) .LT. BBCFR1 
     1          .AND. CROSS(IC) .GE. BBCFR2 ) ) THEN
               RCHOVER = RCHOVER - 
     1            ( CROSS(IC) - MAX( BBIF1, BBCFR2 ) )
            END IF
C
C           Some debug printout in the loop.
C
            IF( SDEBUG ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, 
     1             '( A, 4I3, F8.2, I5, 3F8.2, I3, 2F8.2 )' )
     2             'RDBEMTCH CROSS: ', KS, KF, ICH, IIF, FLO1(IIF,KF),
     3             IC, CROSS(IC), BBCFR1, BBCFR2, BBSIDE, 
     4             BBIF1, BBIF2
                   CALL WLOG( 0, MSGTXT )
            END IF
         END IF
      END DO
C
C     Provide feedback if debugging.
C
      IF( SDEBUG .AND. RCHOIN .GT. RCHOVER ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I4, A, F6.2, A, A, A )' )
     1         'RDBEMTCH: Channel ', ICH, 
     2         ' will lose ', RCHOIN - RCHOVER, 
     3         ' MHZ due to a crossover if Freq. catalog group ', 
     4         FRNAME(KF)(1:LEN1(FRNAME(KF))), ' is used.'
         CALL WLOG( 0, MSGTXT )
      END IF
C
      RETURN
      END
