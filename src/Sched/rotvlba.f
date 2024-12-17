      SUBROUTINE ROTVLBA( ISCN, ISTA, PTADD, LASTDY )
C
C     Writes focus/rotation pattern, which consists of a 
C     at a number of focus and rotation positions pointing sequence.  
C     Units are mm.
C
C     Feb 6, 2007  Account for rotation moving on curved path, not
C     straight, in calculation of az and el collimation offsets. RCW.
C
C     Feb. 22, 2011.  The rotation numbers actually used on the VLBA
C     (or at least reported) have a different zero point and different
C     sign convention (direction corresponding to a positive rotation)
C     from the numbers on the chart I used to get the FEEDPOS 
C     values below.  The actual numbers have an arbitrary zero, so don't
C     use them for the curvature calculations.  But do change curvature
C     calculation to recognize that a positive rotation is clockwise
C     looking down on the feed circle.  Do that by subtracting the
C     original values of FEEDPOS from 360 deg and changing the equations
C     for the collimation offsets.
C
C     June 27, 2011.  I'm getting a lot of failed rotation patterns so
C     something went wrong in the above logic.
C
C     In tjun26u, we had the following pattern:
C
C       Schedule     tsm FRM rec    Schedule      a priori    Measured  a pr. beta
C      foc    rot    foc     rot    azc   elc    azc    elc   azc  elc    azc  elc
C    -17.0    0.0   4.73  245.89    0.0   0.0   4.26  -4.10   4.2 -4.0    0.0  0.0
C      0.0  -12.0   6.42  233.89  -5.93  5.34  -1.67   1.24  -1.1  2.1  -4.69  6.46
C      0.0    0.0   6.43  245.90    0.0   0.0   4.26  -4.10   4.2 -4.0   0.0   0.0
C      0.0   12.0   6.43  257.89   4.69 -6.46   8.95 -10.56   9.1 -9.8   5.93 -5.34
C     17.0    0.0   8.13  245.88    0.0   0.0   4.26  -4.10   4.2 -4.0    0.0  0.0
C
C     So all is good in the focus pattern.
C
C     The scheduled rotation offsets have the same sign convention as 
C     the reported positions.  
C     
C     Reported rotation of 1cm is 303 so convention is clockwise looking
C     down (confirmed by looking at a photograph of Pie Town).
C     At the position of the 2cm receiver, one would expect that the 
C     Az collimation change would be larger for an increase in rotation
C     than for a decrease of the same amount.  That seems to be the 
C     opposite from what the a priori numbers request.  The Measured data 
C     for the positive move are from one point and are marginal.  But it looks
C     sort of like there isn't much effect in reality.  Certainly the
C     a priori numbers don't seem right.
C
C     Aarg - the focrot files have been using SCHED 9.4 and the Feb 2011 fixes
C     were not in that.  They are only in the beta.  But even so, I think
C     I'm not getting it quite right.  Get Jim to get some more data using
C     the beta version.
C
C     July 6, 2011.  Got more 2cm data.  Much improved, but not perfect.
C     Try to get a fit from rotanal using the second order versions.
C
      INCLUDE           'sched.inc'
      INCLUDE           'schset.inc'
C
      INTEGER           I, PTADD, ISCN, ISTA, IPAT, KS
      INTEGER           RLEVEL(MCHAN), DOY, LASTDY, IDUR, ISDUR
      REAL              DOAZ, DOEL, ROTINC, FOCINC, FEEDPOS
      REAL              DOFOC, DOROT, DOAZPAT, DOELPAT, LOWLIM
      DOUBLE PRECISION  PSTOP, FRACD
      CHARACTER         TSTOP*9, VLBAD*9, HUMAND*16, TFORM*9
      LOGICAL           BEGPAT, ENDPAT
C --------------------------------------------------------------------
C     Get the setup file pointer.
C
      KS = NSETUP(ISCN,ISTA)
C
C     Get the center pointing position.  This is a sum from both
C     the main schedule and the setup file.
C
      DOAZ = SAZCOL(ISCN) + AZCOLIM(KS)
      DOEL = SELCOL(ISCN) + ELCOLIM(KS)
C
C     Get some required data for the F/R pattern.
C     On-line system ignores commands to go beyond limit.
C     Limits are 28 and 684 mm.
C     13 cm has minimum of 65 other than at Nl
C     KP is 40 at 13 cm and 57 at 6 cm.
C     Use a maximum FOCINC of 30 and treat Nl specially
C     by not letting focus get below -10 at 13 cm and 
C     -25 at 6 cm.  Note normally FOCINC would be 100 for 13 cm
C     and 44cm for 6 cm.
C
C     FEEDPOS is the position measured clockwise when looking 
C     down on the feed circle.  The zero point is on the
C     E/W axis that goes between the 13cm and 4cm feeds.
C
      LOWLIM = -30.0
      IF( FE(2,KS) .EQ. '2cm' .OR. FE(4,KS) .EQ. '2cm' ) THEN
         ROTINC = 8.0                !  Expect 12 GHz (masers)
         FOCINC = 17.0
         FEEDPOS = ( 360. - 138.0 ) * RADDEG
      ELSE IF( FE(2,KS) .EQ. '1.3cm' .OR. FE(4,KS) .EQ. '1.3cm' .OR. 
     1         FE(2,KS) .EQ. '1cm' .OR. FE(4,KS) .EQ. '1cm') THEN
         ROTINC = 4.5
         FOCINC = 10.0
         FEEDPOS = ( 360. - 78.0 ) * RADDEG
      ELSE IF( FE(1,KS) .EQ. '7mm' .OR. FE(3,KS) .EQ. '7mm' ) THEN
         ROTINC = 2.2
         FOCINC = 5.0
         FEEDPOS = ( 360. - 155.0 ) * RADDEG
      ELSE IF( FE(2,KS) .EQ. '3mm' .OR. FE(4,KS) .EQ. '3mm' ) THEN
         ROTINC = 1.1
         FOCINC = 2.5
         FEEDPOS = ( 360. - 180.0 ) * RADDEG
      ELSE IF( FE(2,KS) .EQ. '90cm' .OR. FE(4,KS) .EQ. '90cm' .OR.
     1         FE(2,KS) .EQ. '50cm' .OR. FE(4,KS) .EQ. '50cm' .OR.
     2         FE(1,KS) .EQ. '20cm' .OR. FE(3,KS) .EQ. '20cm' ) THEN
         CALL ERRLOG( 'PTVLBA:  Cannot do rotpat for 50/90 or 20 cm.' )
      ELSE IF( FE(1,KS) .EQ. '6cm' .OR. FE(3,KS) .EQ. '6cm' ) THEN
         ROTINC = 20.0
         FOCINC = 30.0
         FEEDPOS = ( 360. - 108.0 ) * RADDEG
         IF( STCODE(STANUM(ISTA)) .EQ. 'Nl' ) LOWLIM = -25.0
      ELSE IF( FE(2,KS) .EQ. '4cm' .OR. FE(4,KS) .EQ. '4cm' ) THEN
         ROTINC = 12.0
         FOCINC = 26.0
         IF( FE(1,KS) .EQ. '13cm' .OR. FE(3,KS) .EQ. '13cm' ) THEN
            FEEDPOS = ( 360. - 32.0 ) * RADDEG
         ELSE
            FEEDPOS = ( 360. - 323.0 ) * RADDEG
         END IF
C
C     Put 13 cm after the 4cm so the sx is right.
C
      ELSE IF( FE(1,KS) .EQ. '13cm' .OR. FE(3,KS) .EQ. '13cm' ) THEN
         ROTINC = 45.0
         FOCINC = 30.0
         FEEDPOS = ( 360. - 32.0 ) * RADDEG
         IF( STCODE(STANUM(ISTA)) .EQ. 'Nl' ) LOWLIM = -10.0
      ELSE
         WRITE( MSGTXT, '( A, 4( 1X, A ) )' )
     1       'ROTVLBA:  Unrecognized FE: ', FE(1,KS), FE(2,KS), 
     2       FE(3,KS), FE(4,KS)
         CALL ERRLOG( MSGTXT )
      END IF

C
C     Do setup scan on half power point to get reasonable levels.
C     First go to half power point.  Note that subroutine VLBA
C     only does setup scans if tape is being written.
C
      DO I = 1, NCHAN(KS)
         RLEVEL(I) = -1 
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(KS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
C
C     Use a stop time that is STARTJ + PTSLEW.  This makes it
C     unnecessary to use PTADD as is used in PTVLBA.
C     Don't use durations until actually into the patterns.  
C     Deal with day changes.
C
      PSTOP = STARTJ(ISCN) + PTSLEW(ISCN) / 86400.D0
      CALL SCHDAY( PSTOP, VLBAD, HUMAND, DOY, FRACD )
      TSTOP = TFORM( FRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
      IF( DOY .NE. LASTDY ) THEN
         WRITE( IUVBA, '(''date='', A9 )' ) VLBAD
         LASTDY = DOY
      END IF
C
C     Scan info.
C
      WRITE( IUVBA, '( A, F7.2, A, F7.2, /, A, /, 3A )' )
     1      'azcolim=', DOAZ - PTINCR(KS), ' elcolim=', DOEL,
     2      'focus=0.0  rotation=0.0  dur=0s ',
     3      'stop=', TSTOP, '   !NEXT! '
C
C     Then sit there while fixing the BBC levels.  Take long
C     enough for this to get a pcx test.  Assume that 2 times
C     PTDUR will be long enough.  The scan is set up in PTPAT.
C
      DO I = 1, NCHAN(KS)
         RLEVEL(I) = 256
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(KS), RLEVEL, LLEVEL, MLEVEL,
     1 .FALSE., IUVBA ) 
C 
C     Now do the offset forcus and rotation positions if that is
C     desired.  For rotation, 40" (0.6666 arc min) collimation
C     change per degree is expected.  A 1 degree rotation
C     corresponds to a shift of the beam on the feed circle of
C     FCR*sin(1) = FCR*0.01745 where FCR is the radius of the feed
C     circle (which I don't happen to know at the moment - but it
C     is not needed - it divides out).  For large rotations, 
C     the beam shifts on a circle so you cannot just multiply the 
C     1 degree number by the rotation shift.  Instead, calculate 
C     the beam shift in the az and el axes and use that to get the 
C     pointing shift.  See the math in the code.
C     
C     Assume zero shift for focus offsets.
C 
C     Divide the total time into ROTPAT segments.  Test if 
C     they are of appropriate length. This allows for multiple 
C     pointing patterns at each focus/rotation position, if 
C     desired.  
C
      ISDUR = NINT( DUR(ISCN) * 86400.D0 ) 
      IDUR = ( ISDUR - PTSLEW(ISCN) - ROTPAT * 2 * PTDUR ) / ROTPAT
      IF( MOD( IDUR, 10 * PTDUR ) .NE. 0 ) THEN

         CALL WLOG( 1, 'ROTVLBA: Scan length not well ' //
     1       'matched to focus/rotation pattern time.' )
         CALL WLOG( 1, '  (need DUR = PTSLEW + ROTPAT*' //
     1       '(2*PTDUR+N*10*PTDUR)).' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 4( A, I4 ) )' ) 
     1       '  DUR =', ISDUR, ', PTSLEW =', PTSLEW(ISCN),
     2       ', ROTPAT =', ROTPAT, ', and PTDUR =', PTDUR
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I4  )' ) 
     1       '  Time available for each loop of 10 scan pointings:',
     2       IDUR
         CALL WLOG( 1, MSGTXT )
      END IF
      DO IPAT = 1, ROTPAT
C
C        Deal with time, including possible day change.
C
         PSTOP = PSTOP + ( DUR(ISCN) - PTSLEW(ISCN) / 86400.D0 ) /
     1            ROTPAT
         CALL SCHDAY( PSTOP, VLBAD, HUMAND, DOY, FRACD )
         TSTOP = TFORM( FRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
         IF( DOY .NE. LASTDY ) THEN
            WRITE( IUVBA, '(''date='', A9 )' ) VLBAD
            LASTDY = DOY
         END IF
         WRITE( IUVBA, '( A, A )' ) 'stop = ', TSTOP
C
C        Get the focus and rotation values.
C        Change the sign on DOAZPAT's addition to go with the
C        change in the FEEDPOS convention.
C
         DOFOC = FOCUS(ISCN) + FOCINC * FOC(IPAT)
         IF( DOFOC .LT. LOWLIM ) DOFOC = LOWLIM
         DOROT = ROTATION(ISCN) + ROTINC * ROT(IPAT)
         DOAZPAT = DOAZ + ( 0.6666 / 0.01745 ) * 
     1       ( COS( DOROT * RADDEG + FEEDPOS ) - COS( FEEDPOS ) )
         DOELPAT = DOEL + ( 0.6666 / 0.01745 ) *
     1       ( SIN( DOROT * RADDEG + FEEDPOS ) - SIN( FEEDPOS ) )
         WRITE( IUVBA, '( A, F7.2, A, F7.2 )' ) 
     1      'focus = ', DOFOC, '  rotation = ', DOROT
         WRITE( IUVBA, '( A, F7.2, A, F7.2 )' )
     1      'azcolim=', DOAZPAT - PTINCR(KS), ' elcolim=', DOELPAT
         BEGPAT = IPAT .EQ. 1
         ENDPAT = IPAT .EQ. ROTPAT
         CALL PTPAT( IUVBA, DOAZPAT, DOELPAT, PTINCR(KS), 
     1        PTOFF(KS), PTDUR, QUAL(ISCN) )
      END DO
C
      RETURN
      END


