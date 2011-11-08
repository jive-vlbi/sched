      SUBROUTINE RDBEMTCH( KS, KF, ICH, IIF, FRAD1, FRAD2, 
     1        RFRQ1, RFRQ2, RCHOVER )
C
C     Routine called by FMATCH to penalize a frequency file entry if 
C     there are crossover problems.  At the time of the call, 
C     FREQREF, BBFILT, and NETSIDE are known for each channel.  
C     Other parameter might not have been set yet.
C
C     KS is the setup group.
C     KF is the frequency catalog group.
C     RCHOVER was set in FMATCH without knowledge of crossover
C     frequencies.  It is reduced here by the amount of data
C     on the far side of a crossover from FREQREF.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER           KS, KF, IIF, ICH, IC
      INTEGER           IFSIDE, BBSIDE, NCROSS, LEN1
      REAL              RCHOVER, RCHOIN
      DOUBLE PRECISION  FRAD1, FRAD2, BBFRAD1, BBFRAD2
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
C     to 
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
         IFSIDE  = 1
         BBCFR1  = FREQREF(ICH,KS) - FLO1(IIF,KF)
         BBFRAD1 = FRAD1 - FLO1(IIF,KF)
         BBFRAD2 = FRAD2 - FLO1(IIF,KF)
      ELSE
         IFSIDE  = -1
         BBCFR1  = FLO1(IIF,KF) - FREQREF(ICH,KS)
         BBFRAD1 = FLO1(IIF,KF) - FRAD2
         BBFRAD2 = FLO1(IIF,KF) - FRAD1
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
C     Subtract part lost to the crossovers in the RDBE_DDC.
C     Only bother when the crossover is within both the 
C     frequency window and the channel band.  Otherwise
C     CHOVER will not be affected.
C
      RCHOIN = RCHOVER
      DO IC = 1, NCROSS
         IF( ( CROSS(IC) .GT. BBFRAD1 .AND. 
     2         CROSS(IC) .LE. BBFRAD2 ) .AND.
     3       ( CROSS(IC) .GT. BBCFR1 .AND. 
     4         CROSS(IC) .LE. BBCFR2 ) ) THEN
            IF( BBSIDE .EQ. 1 ) THEN
               RCHOVER = RCHOVER - 
     1            ( MIN( BBFRAD2, BBCFR2 ) - CROSS(IC) )
            ELSE
               RCHOVER = RCHOVER - 
     1            ( CROSS(IC) - MAX( BBFRAD2, BBCFR2 ) )
            END IF
C
C           Let the user know something happened.
C
            MSGTXT = ' '
         END IF
C
      END DO
C
C     Provide feedback if debugging.
C
      IF( DEBUG .AND. RCHOIN .GT. RCHOVER ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I4, A, F6.2, A, A, A )' )
     1         'RDBEMTCH: Channel ', ICH, 
     2         ' will lose ', RCHOIN - RCHOVER, 
     3         ' MHZ due to a crossover if Freq. catalog group ', 
     4         FRNAME(KF)(1:LEN1(FRNAME(KF))), ' is used.'
         CALL WLOG( 1, MSGTXT )
      END IF
C

      RETURN
      END
