      SUBROUTINE SCHPRE
C
C     The source catalog can contain B1950, J2000, and/or apparent 
C     coordinates of date. SCHED prints all three.  Where there is a 
C     choice, the control file will contain the epoch given in the 
C     catalog.  If more than one was given (flagged in PEPOCH), 
C     the preference order is DATE, J2000, B1950.
C
C     The routine precesses to the epochs not provided in the catalog 
C     using the Starlink SLALIB precession routines. Date coordinates
C     (where not given) are computed for the stop time of the first 
C     scan.  The B1950/J2000 conversions assume that the coordinates 
C     were determined from observations at epoch PRECDATE (User input)
C     which defaults to 1979.9, which is the value used to determine
C     B1950 coordinates on the VLA.
C
C     Note that any use of B1950 coordinates can introduce a lot of
C     uncertainty in source positions at milliarcsecond levels.  It
C     is not recommended.
C
C     Proper motions and paralax added March 2003 RCW.  
C
C     Modified for apparent rather than mean coordinates of date and
C     B1950/J2000 coordinate transforms fixed by John Reynolds, 
C     Feb. 1991
C
C     Updated the SLA routines to GNU licensed versions in April 2013.
C     Without modification of this code, this caused the coordinates
C     of date to change by a few tens of milli-arcseconds.  The B1950
C     and J2000 did not change.

C
      INCLUDE 'sched.inc'
C
      INTEGER           ISRC, YEAR, DAY
      DOUBLE PRECISION  DR, DD  !  Dummy proper motion parameters.
      DOUBLE PRECISION  STOP    !  Dummy for stop time.
      DOUBLE PRECISION  RAIN, DECIN, RAOUT, DECOUT
      DOUBLE PRECISION  RPMRA, RPMDEC
      DOUBLE PRECISION  NEWEPO, SLA_EPJ
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCHPRE:  Starting.' )
C
      CALL TIMEJ( STOPJ(1), YEAR, DAY, STOP )      
      DO ISRC = 1, MSRC
C
C        Check if some sort of coordinates were provided.
C
         IF(C1950(ISRC).EQ.' ' .AND. C2000(ISRC).EQ.' ' 
     1        .AND. CDATE(ISRC).EQ.' ' ) THEN
            CALL ERRLOG( ' No coordinates for '//SOURCE(1,ISRC) )
         END IF
C
C        First check if we need to deal with proper motions or
C        paralax.  Recall that PMRA and PMDEC are in arcsec/yr
C        (converted in RDSRC from input mas values),
C        DRA is in change of time seconds/UT day, and DDEC is in
C        change of arcseconds/UT day.
C
         IF( PMRA(ISRC) .NE. 0.D0 .OR.
     1       PMDEC(ISRC) .NE. 0.D0 .OR.
     2       PARALAX(ISRC) .NE. 0.D0 ) THEN
C
C           We need to take out the proper motion and paralax.
C
C           First get the starting point coordinates.
C
            IF( C2000(ISRC) .EQ. '*' ) THEN
               RAIN = RA2000(ISRC)
               DECIN = D2000(ISRC)
            ELSE IF( C1950(ISRC) .EQ. '*' ) THEN
               RAIN = RA1950(ISRC)
               DECOUT = D1950(ISRC)
            ELSE IF( CDATE(ISRC) .EQ. '*' ) THEN
               RAIN = RAP(ISRC)
               DECOUT = DECP(ISRC)
            END IF
C
C           Get the time near the observation to use to get
C           current positions.  If PMTIME is zero, set it to
C           the new reference time so that DRA and DDEC can
C           be used to deal with fast proper motions.
C
            IF( PMTIME(ISRC) .NE. 0.D0 ) THEN
               NEWEPO = SLA_EPJ( PMTIME(ISRC) )
            ELSE
               NEWEPO = SLA_EPJ( STOPJ(1) )
               PMTIME( ISRC ) = STOPJ(1)
            END IF
C
C           Convert the proper motions to radians per year.
C           RPMRA is supposed to be the rate of change of the 
C           coordinate, not the angular rate.  Hence the divide
C           by cos(dec)
C
            RPMRA = PMRA(ISRC) * RADDEG / ( 3600.D0 * COS( DECIN ) )
            RPMDEC = PMDEC(ISRC) * RADDEG / 3600.0D0
C
C           Apply the proper motions and paralax.  Note I am ignoring
C           the velocity term.
C
            CALL SLA_PM( RAIN, DECIN, RPMRA, RPMDEC, PARALAX(ISRC), 
     1                 0.D0, EPOCHT(ISRC), NEWEPO, RAOUT, DECOUT )
C
C           Install the new coordinates.
C
            IF( C2000(ISRC) .EQ. '*' ) THEN
               RA2000(ISRC)  = RAOUT
               D2000(ISRC) = DECOUT
            ELSE IF( C1950(ISRC) .EQ. '*' ) THEN
               RA1950(ISRC)  = RAOUT
               D1950(ISRC) = DECOUT
            ELSE IF( CDATE(ISRC) .EQ. '*' ) THEN
               RAP(ISRC)  = RAOUT
               DECP(ISRC) = DECOUT
            END IF
C
C           Add the PMRA and PMDEC to the DRA and DDEC
C           Some stellar proper motions are large enough to
C           cause motions of a few beams for VLBI during
C           a day.  This is no problem for pointing, but
C           to be pedantic, here we go.
C
            DRA(ISRC) = DRA(ISRC) + PMRA(ISRC)
     1                 / ( 365.25D0 * 15.D0 * COS( DECOUT ) )
            DDEC(ISRC) = DDEC(ISRC) + PMDEC(ISRC) / 365.25D0
         END IF
C
C        Get the missing coordinates.
C
C        If 1950 coordinates were not given, get them.
C
         IF( C1950(ISRC).EQ.' ' ) THEN
            IF( C2000(ISRC).EQ.'*' ) THEN
               CALL SLA_FK54Z( RA2000(ISRC), D2000(ISRC),
     1            PRECDATE, RA1950(ISRC), D1950(ISRC), DR, DD )
            ELSE
               CALL SLA_AMP( RAP(ISRC), DECP(ISRC), STOPJ(1),
     1            2000.D0, RA2000(ISRC), D2000(ISRC) )
               CALL SLA_FK54Z( RA2000(ISRC), D2000(ISRC),
     1            PRECDATE, RA1950(ISRC), D1950(ISRC), DR, DD )
            END IF
         END IF
C
C        If 2000 coordinates were not given, get them.
C
         IF( C2000(ISRC).EQ.' ' ) THEN
            IF( C1950(ISRC).EQ.'*' ) THEN
               CALL SLA_FK45Z( RA1950(ISRC), D1950(ISRC),
     1            PRECDATE, RA2000(ISRC), D2000(ISRC) )
            ELSE
               CALL SLA_AMP( RAP(ISRC), DECP(ISRC), STOPJ(1),
     1            2000.D0, RA2000(ISRC), D2000(ISRC) )
            END IF
         END IF
C
C        If coordinates of date were not given, get them.
C
         IF( CDATE(ISRC).EQ.' ' ) THEN
            CALL SLA_MAP( RA2000(ISRC), D2000(ISRC),
     1         0.D0, 0.D0, 0.D0, 0.D0, 2000.D0, STOPJ(1),
     2         RAP(ISRC), DECP(ISRC) )
         END IF
C
      END DO
C
C     Do any required precessions to the full catalog source
C     list if it will be used.
C
      IF( PLOT ) CALL SRLPRE( PRECDATE, STOPJ(1) )
C
      RETURN
      END


